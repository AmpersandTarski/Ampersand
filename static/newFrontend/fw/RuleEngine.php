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
			
			ErrorHandling::addLog("------------------------- CHECKING PROCESS RULES (for role $role->name) -------------------------");
			foreach ($role->maintains as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
				
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				foreach ((array)$violations as $violation) ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
			}
		}else{
			ErrorHandling::addLog("------------------------- CHECKING ALL PROCESS RULES -------------------------");
			foreach(RuleEngine::getAllProcessRuleNames() as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
				
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				foreach ((array)$violations as $violation) ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);;
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
		ErrorHandling::addLog('------------------------- CHECKING INVARIANT RULES -------------------------');
		
		// If $allInvariantConjuctsIds is provided (i.e. not null, which is something different than an empty array), check only those invariant conjuncts
		if(!is_null($allInvariantConjuctsIds)) {
			ErrorHandling::addLog("Checking all provided conjuncts");
			foreach ((array)$allInvariantConjuctsIds as $conjunctId){
				$violations = RuleEngine::checkConjunct($conjunctId, $cacheConjuncts);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					
					$conjunct = RuleEngine::getConjunct($conjunctId);
					foreach ($conjunct['invariantRuleNames'] as $ruleName){
						$rule = RuleEngine::getRule($ruleName);
						ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
					}
					
				}
			}

		// Otherwise check all invariantConjuncts
		}else{
			ErrorHandling::addLog("Checking all invariant rules");
			foreach (RuleEngine::getAllInvariantRulesNames() as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
			
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
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
		
		ErrorHandling::addLog("Checking rule '" . $rule['name']."'");
		try{
			foreach($rule['conjunctIds'] as $conjunctId){
				$result = array_merge((array)$result, RuleEngine::checkConjunct($conjunctId, $cacheConjuncts));
			}
			
			if(count($result) == 0){
				ErrorHandling::addInfo("Rule '".$rule['name']."' holds", 'Rules that hold');
				
			}else{
				$violations = $result;
			}
			return $violations;
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}
	
	/*
	 * $cacheConjuncts
	 * 		true: chache conjuncts, i.e. store them locally in self::$conjunctViolations and, if there are violations, in the database table `__all_signals__`
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	private static function checkConjunct($conjunctId, $cacheConjuncts = true){
		ErrorHandling::addLog("Checking conjunct '" . $conjunctId."'");
		try{
			
			// If conjunct is already evaluated and conjunctCach may be used -> return violations
			if(array_key_exists($conjunctId, self::$conjunctViolations) && $cacheConjuncts){
				ErrorHandling::addLog("Conjunct is already evaluated, getting violations from cache");
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
					ErrorHandling::addLog("Conjunct '".$conjunctId."' holds");
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `__all_signals__` WHERE `conjId` = '$conjunctId'";
					$db->Exe($query);
					
				}elseif($cacheConjuncts){
					ErrorHandling::addLog("Conjunct '".$conjunctId."' broken, caching violations in database");
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `__all_signals__` WHERE `conjId` = '$conjunctId'";
					$db->Exe($query);
					
					// Add new conjunct violation to database table __all_signals__
					$query = "INSERT IGNORE INTO `__all_signals__` (`conjId`, `src`, `tgt`) VALUES ";
					foreach ($violations as $violation) $values[] = "('".$conjunctId."', '".$violation['src']."', '".$violation['tgt']."')";
					$query .= implode(',', $values);
					$db->Exe($query);
				}else{
					ErrorHandling::addLog("Conjunct '".$conjunctId."' broken");
				}
				
				return $violations;
			}	
			
		}catch (Exception $e){
			ErrorHandling::addError("While checking conjunct '".$conjunctId."': ".$e->getMessage);
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
			ErrorHandling::addInfo("No conjunctIds provided (can be that this role does not maintain any rule)");
		}
		
		return false;
		
	}
	
	public static function getRule($ruleName){
		// from Generics.php
		global $allRules;

		if(!array_key_exists($ruleName, $allRules)) throw new Exception("Rule $ruleName does not exists in allRules");
		
		return $rule = $allRules[$ruleName];
		
	}
	
	public static function getConjunct($conjunctId){
		// from Generics.php
		global $allConjuncts;
		
		if(!array_key_exists($conjunctId, $allConjuncts)) throw new Exception("Conjunct $conjunctId does not exists in allConjuncts");
		
		return $conjunct = $allConjuncts[$conjunctId];
		
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