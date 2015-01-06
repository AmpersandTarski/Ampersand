<?php

class RuleEngine {
	
	public static function checkRules($roleId){
		
		RuleEngine::checkProcessRules($roleId);
		
		RuleEngine::checkInvariantRules();
		
	}
	
	public static function checkProcessRules($roleId){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkProcessRules'] as $hook) call_user_func($hook); // Hook functions
		$role = new Role($roleId);
		
		foreach ($role->getRules() as $rule){ 
			$violations = RuleEngine::checkProcessRule($rule);
			foreach ((array)$violations as $violation) ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
		}
		
		// TODO: return function
	
	}
	
	public static function checkInvariantRules($interfaceInvariantConjunctNames = null){
		global $allConjuncts; // from Generics.php
		global $allRules; // from Generics.php
		
		$invariantRulesHold = true; // default
		
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkInvariantRules'] as $hook) call_user_func($hook); // Hook functions
		
		// if $interfaceInvariantConjunctNames is not provided (something different than empty array) then check allConjuncts
		if(is_null($interfaceInvariantConjunctNames)) $interfaceInvariantConjunctNames = array_keys($allConjuncts);
		
		// check conjunct one by one
		foreach((array)$interfaceInvariantConjunctNames as $conjunct){
			$rule = RuleEngine::getRule($allConjuncts[$conjunct]['ruleName']); // get conjunct rule from $allRules
			if(!RuleEngine::checkInvariantRule($rule)) $invariantRulesHold = false;
		}
		
		return $invariantRulesHold;
		
	}
	
	public static function checkProcessRule($rule){
		$db = Database::singleton();
		$violations = array();
		
		try{
			$result = $db->Exe($rule['violationsSQL']);
			if(count($result) == 0){
				// ErrorHandling::addInfo("Rule '".$rule['name']."' holds");
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
	
	public static function checkInvariantRule($rule){
		$db = Database::singleton();
		try{
			$result = $db->Exe($rule['violationsSQL']);
			if(count($result) == 0){
				// ErrorHandling::addInfo("Rule '".$rule['name']."' holds");
				return true;
			}else{
				ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
				return false;
			}
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}
	
	public static function getRule($ruleName){
		global $allRules; // from Generics.php
		
		return $allRules[$ruleName];
	}

}

?>