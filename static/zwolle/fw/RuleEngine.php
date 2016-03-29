<?php

class RuleEngine {
    
    /**
     *
     * @param Conjunct[] $conjuncts
     * @param boolean $cacheConjuncts
     * @return boolean
     */
    public static function checkInvariantRules($conjuncts, $cacheConjuncts = true){
        $invariantRulesHold = true;
    
        // check invariant rules
        Notifications::addLog('------------------------- CHECKING INVARIANT RULES -------------------------', 'RuleEngine');
        Notifications::addLog("-- For provided conjuncts: " . implode(', ', array_column($conjuncts, 'id')), 'RuleEngine');
        Notifications::addLog('--------------------------------------------------------------------------', 'RuleEngine');
        
        foreach ($conjuncts as $conjunct){
            if($conjunct->isInvConj()){
                foreach ($conjunct->evaluateConjunct($cacheConjuncts) as $violation){
                    // If a conjunct is broken (i.e. returns 1 or more violation pairs) mark that invariant rules do not hold
                    $invariantRulesHold = false;
                    foreach ($conjunct->invRuleNames as $ruleName) Notifications::addInvariant(new Violation(Rule::getRule($ruleName), $violation['src'], $violation['tgt']));
                }
            }else{
                Notifications::addInfo("Skipping provided conjunct '{$conjunct->id}', because this is not an invariant conjunct");
            }
        }
        return $invariantRulesHold;
    }
    
	/**
	 * 
	 * @param boolean $cacheConjuncts
	 * @return void
	 */
	public static function checkProcessRules($cacheConjuncts = true){
		$session = Session::singleton();
		
		Notifications::addLog('------------------------- CHECKING PROCESS RULES -------------------------', 'RuleEngine');
		Notifications::addLog("-- For active roles: " . implode(', ', array_column($session->getActiveRoles(), 'label')), 'RuleEngine');
		Notifications::addLog('--------------------------------------------------------------------------', 'RuleEngine');
		
		foreach ($session->rulesToMaintain as $rule){
			$violations = $rule->getViolations($cacheConjuncts);
			foreach ($violations as $violation) Notifications::addSignal($violation);
		}	
	}
	
	/**
	 * 
	 * @param Concept[] $affectedConcepts
	 * @param Relation[] $affectedRelations
	 * @param string $ruleType
	 * @return Conjunct[]
	 */
	public static function getAffectedConjuncts($affectedConcepts, $affectedRelations, $ruleType = 'both'){
	    
	    $affectedConjuncts = array();
	    
	    // Get conjuncts for Concepts
        foreach($affectedConcepts as $concept){
            // Invariant conjuncts
            if($ruleType == 'inv' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedInvConjuncts());
            // Signal conjuncts
            if($ruleType == 'sig' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedSigConjuncts());
        }
        
        // Get conjuncts for Relations
        foreach($affectedRelations as $relation){
            // Invariant conjuncts
            if($ruleType == 'inv' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedInvConjuncts());
            // Signal conjuncts
            if($ruleType == 'sig' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedSigConjuncts());
        }
		
		return array_unique($affectedConjuncts); // remove duplicate entries.
	}
	
	/**
	 * 
	 * @return Violation[]
	 */
	public static function getProcessViolationsFromDB(){
	    $session = Session::singleton();
	    $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
	    
		$conjuncts = array();
		$conjunctRuleMap = array();
		foreach ($session->rulesToMaintain as $rule){
			foreach($rule->conjuncts as $conjunct) $conjunctRuleMap[$conjunct->id][] = $rule;
			$conjuncts = array_merge($conjuncts, $rule->conjuncts);
		}
		$conjuncts = array_unique($conjuncts); // remove duplicates
		
    	$violations = array();
    	if(count($conjuncts) > 0){
    	    $q = implode(',', array_map( function($conj){ return "'{$conj->id}'";}, $conjuncts)); // returns string "<conjId1>,<conjId2>,<etc>"
    	    $query = "SELECT * FROM `{$dbsignalTableName}` WHERE `conjId` IN ({$q})";
    	    $result = $session->database->Exe($query); // array(array('conjId' => '<conjId>', 'src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'))
    	    foreach ($result as $row){
    	        foreach($conjunctRuleMap[$row['conjId']] as $rule){
    	           $violations[] = new Violation($rule, $row['src'], $row['tgt']);
    	        }
    	    }
    	}else{
    	    Notifications::addLog("No conjuncts to check (it can be that this role does not maintain any rule)", 'RuleEngine');
    	}
    	return $violations;
	}
}

?>