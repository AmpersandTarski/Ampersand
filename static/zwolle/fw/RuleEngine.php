<?php

class RuleEngine {
	
	/*
	 * $conjunctViolations[$conjId][] = array('src' => $srcAtom, 'tgt' => $tgtAtom)
	 */
	static $conjunctViolations = array();
	
	/*
	 * variables to store rules that are included from rules.json
	 */
	static $allInvariantRules = null;
	static $allProcessRules = null;
	static $allRules = null; // associative (using 'name') merge of $allInvariantRules and $allProcessRules
	
	/* 
	 * $cacheConjuncts
	 * 		true: chache conjuncts
	 * 		false: don't cache conjuncts (is used by ExecEngine)
	 * 		default: true
	 */
	// TODO: function can be made simpler.
	public static function checkProcessRules($session = null, $cacheConjuncts = true){
		
		if(!is_null($session)){
			
			Notifications::addLog("-- Checking process rules maintained by active roles --", 'RuleEngine');
			foreach ($session->rulesToMaintain as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
				
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				foreach ((array)$violations as $violation) Notifications::addViolation($rule, $violation['src'], $violation['tgt']);
			}
		}else{
			Notifications::addLog("-- Checking ALL process rules --", 'RuleEngine');
			foreach(RuleEngine::getAllProcessRules() as $rule){				
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
	/**
	 * 
	 * @param Conjunct[] $invariantConjucts
	 * @param boolean $cacheConjuncts
	 * @return boolean
	 */
	public static function checkInvariantRules($invariantConjucts = null, $cacheConjuncts = true){		
		$invariantRulesHold = true; // default 
		
		// check invariant rules
		Notifications::addLog('------------------------- CHECKING INVARIANT RULES -------------------------', 'RuleEngine');
		
		// If $allInvariantConjuctsIds is provided (i.e. not null, which is something different than an empty array), check only those invariant conjuncts
		if(!is_null($invariantConjucts)) {
			Notifications::addLog("Checking provided conjuncts: " . implode(', ', array_column($invariantConjucts, 'id')), 'RuleEngine');
			foreach ($invariantConjucts as $conjunct){
				$violations = RuleEngine::checkConjunct($conjunct, $cacheConjuncts);
				
				foreach ((array)$violations as $violation){
					$invariantRulesHold = false;
					
					foreach ($conjunct->invRuleNames as $ruleName){
						$rule = RuleEngine::getRule($ruleName);
						Notifications::addInvariant($rule, $violation['src'], $violation['tgt']);
					}
					
				}
			}

		// Otherwise check all invariantConjuncts
		}else{
			Notifications::addLog("Checking all invariant rules", 'RuleEngine');
			foreach (RuleEngine::getAllInvariantRules() as $rule){			
				$violations = RuleEngine::checkRule($rule, $cacheConjuncts);
				
				foreach ((array)$violations as $violation){
					$invariantRulesHold = false;
					Notifications::addInvariant($rule, $violation['src'], $violation['tgt']);
					
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
		
		Notifications::addLog("Checking rule '" . $rule['name']."'", 'RuleEngine');
		try{
			foreach($rule['conjunctIds'] as $conjId){
			    $conjunct = Conjunct::getConjunct($conjId);
				$result = array_merge((array)$result, self::checkConjunct($conjunct, $cacheConjuncts));
			}
			
			if(count($result) == 0){
				Notifications::addInfo("Rule '".$rule['name']."' holds", 'RuleEngineRulesThatHold', 'Rules that hold');
				
			}else{
				$violations = $result;
			}
			return $violations;
		}catch (Exception $e){
			Notifications::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage());
		}
	}
	
	/**
	 * 
	 * @param Conjunct[] $conjuncts
	 * @param boolean $cacheConjuncts
	 *     true: chache conjuncts, i.e. store them locally in self::$conjunctViolations and, if there are violations, in the database
	 *     false: don't cache conjuncts (is used by ExecEngine)
	 * @return void
	 */
	public static function checkConjuncts($conjuncts, $cacheConjuncts = true){
		foreach((array)$conjuncts as $conjunct) RuleEngine::checkConjunct($conjunct);
	}
	
	/**
	 * 
	 * @param Conjunct $conjunct
	 * @param boolean $cacheConjuncts
	 *     true: chache conjuncts, i.e. store them locally in self::$conjunctViolations and, if there are violations, in the database
	 *     false: don't cache conjuncts (is used by ExecEngine)
	 * @return array
	 */
	public static function checkConjunct($conjunct, $cacheConjuncts = true){
		Notifications::addLog("Checking conjunct '{$conjunct->id}' cache:" . var_export($cacheConjuncts, true), 'RuleEngine');
		try{
			
			// If conjunct is already evaluated and conjunctCach may be used -> return violations
			if(array_key_exists($conjunct->id, self::$conjunctViolations) && $cacheConjuncts){
				Notifications::addLog("Conjunct is already evaluated, getting violations from cache", 'RuleEngine');
				return self::$conjunctViolations[$conjunct->id];
			
			// Otherwise evaluate conjunct, cache and return violations
			}else{
				$db = Database::singleton();
				$dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
				$violations = array();
				
				// Evaluate conjunct
				$violations = (array)$db->Exe($conjunct->query);
				
				// Cache violations
				if($cacheConjuncts) self::$conjunctViolations[$conjunct->id] = $violations;
				
				
				if(count($violations) == 0){
					Notifications::addLog("Conjunct '{$conjunct->id}' holds", 'RuleEngine');
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `$dbsignalTableName` WHERE `conjId` = '{$conjunct->id}'";
					$db->Exe($query);
					
				}elseif($cacheConjuncts){
					Notifications::addLog("Conjunct '{$conjunct->id}' broken, caching violations in database", 'RuleEngine');
					
					// Remove "old" conjunct violations from database
					$query = "DELETE FROM `$dbsignalTableName` WHERE `conjId` = '{$conjunct->id}'";
					$db->Exe($query);
					
					// Add new conjunct violation to database
					$query = "INSERT IGNORE INTO `$dbsignalTableName` (`conjId`, `src`, `tgt`) VALUES ";
					foreach ($violations as $violation) $values[] = "('{$conjunct->id}', '".$violation['src']."', '".$violation['tgt']."')";
					$query .= implode(',', $values);
					$db->Exe($query);
				}else{
					Notifications::addLog("Conjunct '{$conjunct->id}' broken", 'RuleEngine');
				}
				
				return $violations;
			}	
			
		}catch (Exception $e){
			Notifications::addError("While checking conjunct '{$conjunct->id}': " . $e->getMessage());
		}
	}
	
	public static function getSignalsFromDB($conjunctIds){
		$db = Database::singleton();
		$dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
		
		$result = array();
		
		$conjunctIds = array_unique($conjunctIds); // remove duplicates
		
		if (count($conjunctIds) > 0) {
			// TODO: DB Query can be changed to WHERE `conjId` IN (<conjId1>, <conjId2>, etc)
			$query = "SELECT * FROM `$dbsignalTableName` WHERE " . implode(' OR ', array_map( function($conjunctId) {return "`conjId` = '$conjunctId'";}, $conjunctIds));
			$result = $db->Exe($query);
		} else {
			Notifications::addLog("No conjuncts to check (it can be that this role does not maintain any rule)", 'RuleEngine');
		}
		
		return $result;
		
	}
	
	public static function getAllRules(){
		if(is_null(self::$allRules)){
			$rules = file_get_contents(__DIR__ . '/../generics/rules.json');
			$rules = json_decode($rules, true);
			
			self::$allInvariantRules = (array)$rules['invariants'];
			self::$allProcessRules = (array)$rules['signals'];
			
			$allRules = array();
			foreach (array_merge((array)$rules['invariants'], (array)$rules['signals']) as $r) $allRules[$r['name']] = $r; 
			self::$allRules = $allRules;
		}
		
		return self::$allRules;
	}
	
	public static function getAllInvariantRules(){
		RuleEngine::getAllRules();
	
		return self::$allInvariantRules;
	}
	
	public static function getAllProcessRules(){
		RuleEngine::getAllRules();
	
		return self::$allProcessRules;
	}
	
	public static function getRule($ruleName){
		$allRules = RuleEngine::getAllRules();

		if(!array_key_exists($ruleName, $allRules)) throw new Exception("Rule \'$ruleName\' does not exists in allRules", 500);
		
		return $rule = $allRules[$ruleName];
		
	}
	
	public static function getPairView($srcAtom, $srcConcept, $tgtAtom, $tgtConcept, $pairView){
		$database = Database::singleton();
		
		Notifications::addLog('Creating violation message', 'RuleEngine');
		$pairStrs = array();
		$interfaceIds = array();
		foreach ((array)$pairView as $segment){
			// interface segment
			if ($segment['segmentType'] == 'Ifc'){
				$interfaceIds = explode(';', $segment['Interfaces']);
					
			// text segment
			}elseif ($segment['segmentType'] == 'Text'){				
				$pairStrs[] = $segment['Text'];
					
			// expressie segment
			}elseif($segment['segmentType'] == 'Exp'){
				// select starting atom depending on whether the segment uses the src of tgt atom.
				$atom = $segment['srcOrTgt'] == 'Src' ? $srcAtom : $tgtAtom;
	
				// quering the expression
				$atomEsc = $database->escape($atom);
				$query = "SELECT DISTINCT `tgt` FROM ($segment[expSQL]) AS `results` WHERE `src` = '$atomEsc'"; // SRC of TGT kunnen door een expressie gevolgd worden
				$rows = $database->Exe($query);
	
				// returning the result
				if(count($row) > 1) throw new Exception("Expression of pairview results in more than one tgt atom", 501); // 501: Not implemented
				$pairStrs[] = $rows[0]['tgt'];
	
			// unknown segment
			}else{
				$errorMessage = "Unknown segmentType '" . $segment['segmentType'] . "' in pairview";
				throw new Exception($errorMessage, 501); // 501: Not implemented
			}
		}
		return array('violationMessage' => implode($pairStrs)
					,'interfaceIds'	=> $interfaceIds);
	}
	
	/**
	 * 
	 * @param Concept[] $affectedConcepts is expected to be already unique (i.e. no duplicate entries)
	 * @param Relation[] $affectedRelations is expected to be already unique (i.e. no duplicate entries)	 * 
	 * @return Conjunct[]
	 */
	public static function getAffectedSigConjuncts($affectedConcepts, $affectedRelations){
		
		$affectedConjuncts = array();
		foreach($affectedConcepts as $concept){
			$affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedSigConjuncts());
		}
		foreach($affectedRelations as $relation){
			$affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedSigConjuncts());
		}
		
		return array_unique($affectedConjuncts); // remove duplicate entries.
	}
	
	/**
	 * 
	 * @param Concept[] $affectedConcepts
	 * @param Relation[] $affectedRelations
	 * @return Conjunct[]
	 */
	public static function getAffectedInvConjuncts($affectedConcepts, $affectedRelations){
	
		$affectedConjuncts = array();
		foreach($affectedConcepts as $concept){
			$affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedInvConjuncts());
		}
		foreach($affectedRelations as $relation){
			$affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedInvConjuncts());
		}
	
		return array_unique($affectedConjuncts); // remove duplicate entries.
	}
	
	public static function getProcessViolationsFromDB($session){		
		$conjunctIds = array();
		$conjunctRuleMap = array();
		foreach ($session->rulesToMaintain as $ruleName){
			$rule = RuleEngine::getRule($ruleName);
			foreach($rule['conjunctIds'] as $conjunctId) $conjunctRuleMap[$conjunctId][] = $ruleName;
			$conjunctIds = array_merge($conjunctIds, $rule['conjunctIds']);
		}
		$signals = RuleEngine::getSignalsFromDB($conjunctIds);
		
		foreach ($signals as $signal){ // $signal[] = array('conjId' => , 'src' => , 'tgt' => )
			foreach($conjunctRuleMap[$signal['conjId']] as $ruleName){
				Notifications::addViolation(RuleEngine::getRule($ruleName), $signal['src'], $signal['tgt']);
			}
		}
	}

}

?>