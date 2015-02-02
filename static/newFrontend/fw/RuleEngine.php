<?php

class RuleEngine {
	
	/*
	 * $conjunctViolations[$conjId][] = array('src' => $srcAtom, 'tgt' => $tgtAtom)
	 */
	static $conjunctViolations = array();
	
	public static function checkRules($roleId){
		
		RuleEngine::checkProcessRules($roleId);
		
		RuleEngine::checkInvariantRules();
		
	}
	
	/* 
	 * $cacheConjuncts
	 * 		true: chache conjuncts
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	// TODO: function can be made simpler.
	public static function checkProcessRules($roleId = null, $cacheConjuncts = true){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkProcessRules'] as $hook) call_user_func($hook); // Hook functions
		
		if(!is_null($roleId)){
			$role = new Role($roleId);
			
			Notifications::addLog("------------------------- CHECKING PROCESS RULES (for role $role->name) -------------------------");
			foreach ($role->maintains as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
				
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				foreach ((array)$violations as $violation) Notifications::addViolation($rule, $violation['src'], $violation['tgt']);
			}
		}else{
			Notifications::addLog("------------------------- CHECKING ALL PROCESS RULES -------------------------");
			foreach(RuleEngine::getAllProcessRuleNames() as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
				
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				foreach ((array)$violations as $violation) Notifications::addViolation($rule, $violation['src'], $violation['tgt']);;
			}
		}
	
	}
	
	/*
	 * $cacheConjuncts
	 * 		true: chache conjuncts
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	public static function checkInvariantRules($allInvariantConjuctsIds = null, $cacheConjuncts = true){		
		$invariantRulesHold = true; // default
		
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkInvariantRules'] as $hook) call_user_func($hook); // Hook functions 
		
		// check invariant rules
		Notifications::addLog('------------------------- CHECKING INVARIANT RULES -------------------------');
		
		// If $allInvariantConjuctsIds is provided (i.e. not null, which is something different than an empty array), check only those invariant conjuncts
		if(!is_null($allInvariantConjuctsIds)) {
			Notifications::addLog("Checking all provided conjuncts");
			foreach ((array)$allInvariantConjuctsIds as $conjunctId){
				$violations = RuleEngine::checkConjunct($conjunctId, $cacheConjuncts);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					
					$conjunct = RuleEngine::getConjunct($conjunctId);
					foreach ($conjunct['invariantRuleNames'] as $ruleName){
						$rule = RuleEngine::getRule($ruleName);
						Notifications::addInvariant("Violation of rule '".$rule['name']."'");
					}
					
				}
			}

		// Otherwise check all invariantConjuncts
		}else{
			Notifications::addLog("Checking all invariant rules");
			foreach (RuleEngine::getAllInvariantRulesNames() as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
			
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					Notifications::addInvariant("Violation of rule '".$rule['name']."'");
				}
			}
		}
		
		return $invariantRulesHold;
		
	}
	
	/*
	 * $cacheConjuncts
	 * 		true: chache conjuncts
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	public static function checkRule($rule, $cacheConjuncts = true){
		$db = Database::singleton();
		$violations = array();
		
		Notifications::addLog("Checking rule '" . $rule['name']."'");
		try{
			foreach($rule['conjunctIds'] as $conjunctId){
				$result = array_merge((array)$result, RuleEngine::checkConjunct($conjunctId, $cacheConjuncts));
			}
			
			if(count($result) == 0){
				Notifications::addInfo("Rule '".$rule['name']."' holds", 'Rules that hold');
				
			}else{
				$violations = $result;
			}
			return $violations;
		}catch (Exception $e){
			Notifications::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}
	
	/*
	 * $conjuncts = array of conjunctIds
	 * $cacheConjuncts
	 * 		true: chache conjuncts
	 * 		false: don't cache conjuncts (is used by first run of ExecEngine)
	 * 		default: true
	 */
	public static function checkConjuncts($conjuncts, $cacheConjuncts = true){
		foreach((array)$conjuncts as $conjunctId) RuleEngine::checkConjunct($conjunctId);
	}
	
	/*
	 * $cacheConjuncts
	 * 		true: chache conjuncts, i.e. store them locally in self::$conjunctViolations and, if there are violations, in the database table `__all_signals__`
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	private static function checkConjunct($conjunctId, $cacheConjuncts = true){
		Notifications::addLog("Checking conjunct '" . $conjunctId."' cache:".var_export($cacheConjuncts, true));
		try{
			
			// If conjunct is already evaluated and conjunctCach may be used -> return violations
			if(array_key_exists($conjunctId, self::$conjunctViolations) && $cacheConjuncts){
				Notifications::addLog("Conjunct is already evaluated, getting violations from cache");
				return self::$conjunctViolations[$conjunctId];
			
			// Otherwise evaluate conjunct, cache and return violations
			}else{
			
				$db = Database::singleton();
				
				// Evaluate conjunct
				$conjunct = RuleEngine::getConjunct($conjunctId);
				$violations = $db->Exe($conjunct['violationsSQL']);
				
				// Cache violations
				if($cacheConjuncts) self::$conjunctViolations[$conjunctId] = $violations;
				
				
				if(count($violations) == 0){
					Notifications::addLog("Conjunct '".$conjunctId."' holds");
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `__all_signals__` WHERE `conjId` = '$conjunctId'";
					$db->Exe($query);
					
				}elseif($cacheConjuncts){
					Notifications::addLog("Conjunct '".$conjunctId."' broken, caching violations in database");
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `__all_signals__` WHERE `conjId` = '$conjunctId'";
					$db->Exe($query);
					
					// Add new conjunct violation to database table __all_signals__
					$query = "INSERT IGNORE INTO `__all_signals__` (`conjId`, `src`, `tgt`) VALUES ";
					foreach ($violations as $violation) $values[] = "('".$conjunctId."', '".$violation['src']."', '".$violation['tgt']."')";
					$query .= implode(',', $values);
					$db->Exe($query);
				}else{
					Notifications::addLog("Conjunct '".$conjunctId."' broken");
				}
				
				return $violations;
			}	
			
		}catch (Exception $e){
			Notifications::addError("While checking conjunct '".$conjunctId."': ".$e->getMessage);
		}
	}
	
	public static function getSignalsFromDB($conjunctIds){
		$db = Database::singleton();
		
		$conjunctIds = array_unique($conjunctIds); // remove duplicates
		
		if (count($conjunctIds) > 0) {
			// TODO: DB Query can be changed to WHERE `conjId` IN (<conjId1>, <conjId2>, etc)
			$query = "SELECT * FROM `__all_signals__` WHERE " . implode(' OR ', array_map( function($conjunctId) {return "`conjId` = '$conjunctId'";}, $conjunctIds));
			return $signals = $db->Exe($query);
		} else {
			Notifications::addInfo("No conjunctIds provided (can be that this role does not maintain any rule)");
		}
		
		return false;
		
	}
	
	public static function getRule($ruleName){
		// from Generics.php
		global $allRules;

		if(!array_key_exists($ruleName, $allRules)) throw new Exception("Rule $ruleName does not exists in allRules");
		
		return $rule = $allRules[$ruleName];
		
	}
	
	public static function getPairView($srcAtom, $srcConcept, $tgtAtom, $tgtConcept, $pairView){
		$database = Database::singleton();
		
		Notifications::addLog('Creating violation message');
		$pairStrs = array();
		$interfaceNames = array();
		foreach ($pairView as $segment){
			// text segment
			if ($segment['segmentType'] == 'Text' && substr($segment['Text'], 0, 5) == '{IFC}'){
				$interfaceNames = explode(';', substr($segment['Text'], 5));
					
			}elseif ($segment['segmentType'] == 'Text'){				
				$pairStrs[] = $segment['Text'];
					
				// expressie segment
			}elseif($segment['segmentType'] == 'Exp'){
				// select starting atom depending on whether the segment uses the src of tgt atom.
				$atom = $segment['srcOrTgt'] == 'Src' ? $srcAtom : $tgtAtom;
	
				// quering the expression
				$query = "SELECT DISTINCT `tgt` FROM (".$segment['expSQL'].") AS results WHERE src='".addslashes($atom)."'"; // SRC of TGT kunnen door een expressie gevolgd worden
				$rows = $database->Exe($query);
	
				// returning the result
				if(count($row) > 1) throw new Exception('Expression of pairview results in more than one tgt atom');
				$pairStrs[] = $rows[0]['tgt'];
	
				// unknown segment
			}else{
				$errorMessage = "Unknown segmentType '" . $segment['segmentType'] . "' in pairview";
				throw new Exception($errorMessage);
			}
		}
		return array('violationMessage' => implode($pairStrs)
					,'interfaceNames'	=> $interfaceNames);
	}
	
	public static function getConjunct($conjunctId){
		// from Generics.php
		global $allConjuncts;
		
		if(!array_key_exists($conjunctId, $allConjuncts)) throw new Exception("Conjunct $conjunctId does not exists in allConjuncts");
		
		return $conjunct = $allConjuncts[$conjunctId];
		
	}
	
	/*
	 * $affectedConcepts is expected to be already unique (i.e. no duplicate entries)
	 * $affectedRelations is expected to be already unique (i.e. no duplicate entries)
	 * relations in $affectedRelations must be specified with full relation signature (i.e. rel_<relName>_<srcConcept>_<tgtConcept>)
	 * 
	 */
	public static function getAffectedSigConjuncts($affectedConcepts, $affectedRelations){
		
		$affectedConjuncts = array();
		foreach($affectedConcepts as $concept){
			array_merge($affectedConjuncts, (array)Concept::getAffectedSigConjuncts($concept));
		}
		foreach($affectedRelations as $fullRelationSignature){
			array_merge($affectedConjuncts, (array)Relation::getAffectedSigConjunctIds($fullRelationSignature));
		}
		
		return array_unique($affectedConjuncts); // remove duplicate entries.
	}
	
	public static function getAffectedInvConjuncts($affectedConcepts, $affectedRelations){
	
		$affectedConjuncts = array();
		foreach($affectedConcepts as $concept){
			array_merge($affectedConjuncts, (array)Concept::getAffectedInvConjuncts($concept));
		}
		foreach($affectedRelations as $fullRelationSignature){
			array_merge($affectedConjuncts, (array)Relation::getAffectedInvConjunctIds($fullRelationSignature));
		}
	
		return array_unique($affectedConjuncts); // remove duplicate entries.
	}
	
	
	/*
	 * This function returns all InvariantRulesNames. Currently there is no such array in Generics.php.
	 * Therefore this function finds all the processRuleNames (as provided by the $allRoles array) and
	 * selects those rules from $allRules, that are not processRules (a rule is either a processRule or 
	 * an invariantRule, but not both).
	 * 
	 * TODO: 
	 */
	public static function getAllInvariantRulesNames(){
		// from Generics.php
		global $allRoles;
		global $allRules;
		
		// list of all process rules
		$processRuleNames = array();
		foreach($allRoles as $role){
			$processRuleNames = array_merge($processRuleNames, $role['ruleNames']);
		}
		
		// list of all rules
		$allRuleNames = array_keys($allRules);
		
		// return list of all invariant rules (= all rules that are not process rules)
		return array_diff($allRuleNames, $processRuleNames);
	}
	
	public static function getAllProcessRuleNames(){
		// from Generics.php
		global $allRoles;
		global $allRules;
		
		// list of all process rules
		$processRuleNames = array();
		foreach($allRoles as $role){
			$processRuleNames = array_merge($processRuleNames, $role['ruleNames']);
		}
		
		return $processRuleNames;
	}

}

?>