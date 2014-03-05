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
				ErrorHandling::addViolation("Violation of rule '".$rule['name']."'");
			}
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->message);
		}
	}
	public static function checkInvariantRule($rule){
		$db = Database::singleton();
		try{
			$result = $db->Exe($rule['violationsSQL']);
			if(count($result) == 0){
				ErrorHandling::addNotification("Rule '".$rule['name']."' holds");
			}else{
				ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
			}
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->message);
		}
	}

}

?>