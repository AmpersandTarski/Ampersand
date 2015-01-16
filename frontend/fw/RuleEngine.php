<?php

class RuleEngine {
	
	public static function checkRules($roleId){
		
		RuleEngine::checkProcessRules($roleId);
		
		RuleEngine::checkInvariantRules();
		
	}
	
	public static function checkProcessRules($roleId){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkProcessRules'] as $hook) call_user_func($hook); // Hook functions
		$role = new Role($roleId);
		
		foreach ($role->maintains as $ruleName){
			$rule = RuleEngine::getRule($ruleName);
			
			$violations = RuleEngine::checkRule($rule);
			foreach ((array)$violations as $violation) ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
		}
	
	}
	
	public static function checkInvariantRules($interfaceInvariantConjunctNames = null){
		global $allConjuncts; // from Generics.php
		global $allRules; // from Generics.php
		
		$invariantRulesHold = true; // default
		
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkInvariantRules'] as $hook) call_user_func($hook); // Hook functions
		
		// TODO: if $interfaceInvariantConjunctNames is not provided (something different than empty array) then check all invariantRules, otherwise only the provided conjucts
		if(is_null($interfaceInvariantConjunctNames)) {
			
		}else{
			
		}
		
		// check invariant rules
		foreach (RuleEngine::getAllInvariantRulesNames() as $ruleName){
			$rule = RuleEngine::getRule($ruleName);
				
			$violations = RuleEngine::checkRule($rule);
			if(!empty($violations)) {
				$invariantRulesHold = false;
				ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
			}
		}
		
		return $invariantRulesHold;
		
	}
	
	public static function checkRule($rule){
		$db = Database::singleton();
		$violations = array();
		
		try{
			foreach($rule['conjuncts'] as $conjunct){
				$result = array_merge((array)$result, $db->Exe($conjunct['violationsSQL']));
			}
			
			if(count($result) == 0){
				ErrorHandling::addInfo("Rule '".$rule['name']."' holds");
			}else{				
				foreach($result as $violation) {
					$violations[] = array('src' => $violation['src'], 'tgt' => $violation['tgt']);
				}
			}
			return $violations;
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}
	
	public static function getRule($ruleName){
		// from Generics.php
		global $allRules;
		global $allConjuncts;
		
		$rule = $allRules[$ruleName];
		$rule['conjuncts'] = array();
		foreach($rule['conjunctIds'] as $conjunctId){
			$rule['conjuncts'][] = $allConjuncts[$conjunctId];
		}		
		
		return $rule;
	}
	
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

}

?>