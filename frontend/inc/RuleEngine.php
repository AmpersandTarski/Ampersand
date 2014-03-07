<?php

require_once (__DIR__.'/Session.php');

class RuleEngine {

	public static function checkRules($roleId){
		$role = new Role($roleId);
		
		// check process rules
		foreach ($role->getRules() as $rule) RuleEngine::checkProcessRule($rule);
		
		// check invariant rules
		foreach (Session::getInvariantRules() as $rule) RuleEngine::checkInvariantRule($rule);
		
	}
	
	public static function checkProcessRule($rule){
		$db = Database::singleton();
		try{
			$result = $db->Exe($rule['violationsSQL']);
			if(count($result) == 0){
				ErrorHandling::addNotification("Rule '".$rule['name']."' holds");
			}else{				
				foreach($result as $violation) {
					ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
				}
			}
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