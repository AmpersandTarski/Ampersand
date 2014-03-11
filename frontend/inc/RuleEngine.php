<?php

class RuleEngine {
	
	public static function checkRules($roleId){
		
		RuleEngine::checkProcessRules($roleId);
		
		RuleEngine::checkInvariantRules();
		
	}
	
	public static function checkProcessRules($roleId){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkProcessRules'] as $hook) call_user_func($hook); // Hook functions
		$role = new Role($roleId);
		
		foreach ($role->getRules() as $rule) RuleEngine::checkProcessRule($rule);
		
		// TODO: return function
	
	}
	
	public static function checkInvariantRules(){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkInvariantRules'] as $hook) call_user_func($hook); // Hook functions
		$invariantRulesHold = true;
		
		foreach (Session::getInvariantRules() as $rule) if(!RuleEngine::checkInvariantRule($rule)) $invariantRulesHold = false;
		
		return $invariantRulesHold;
		
	}
	
	public static function checkProcessRule($rule){
		$db = Database::singleton();
		$violations = array();
		
		try{
			$result = $db->Exe($rule['violationsSQL']);
			if(count($result) == 0){
				ErrorHandling::addNotification("Rule '".$rule['name']."' holds");
			}else{				
				foreach($result as $violation) {
					ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
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
				// ErrorHandling::addNotification("Rule '".$rule['name']."' holds");
				return true;
			}else{
				ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
				return false;
			}
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}

}

?>