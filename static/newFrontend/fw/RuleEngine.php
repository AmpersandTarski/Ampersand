<?php

class RuleEngine {
	
	public static function checkRules($roleId){
		
		RuleEngine::checkProcessRules($roleId);
		
		RuleEngine::checkInvariantRules();
		
	}
	
	public static function checkProcessRules($roleId){
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkProcessRules'] as $hook) call_user_func($hook); // Hook functions
		$role = new Role($roleId);
		
		ErrorHandling::addLog('------------------------- CHECKING PROCESS RULES -------------------------');
		foreach ($role->maintains as $ruleName){
			$rule = RuleEngine::getRule($ruleName);
			
			$violations = RuleEngine::checkRule($rule);
			foreach ((array)$violations as $violation) ErrorHandling::addViolation($rule, $violation['src'], $violation['tgt']);
		}
	
	}
	
	public static function checkInvariantRules($allInvariantConjuctsIds = null){
		global $allConjuncts; // from Generics.php
		global $allRules; // from Generics.php
		
		$invariantRulesHold = true; // default
		
		foreach ((array)$GLOBALS['hooks']['before_RuleEngine_checkInvariantRules'] as $hook) call_user_func($hook); // Hook functions 
		
		// check invariant rules
		ErrorHandling::addLog('------------------------- CHECKING INVARIANT RULES -------------------------');
		
		// If $allInvariantConjuctsIds is provided (i.e. not null, which is something different than an empty array), check only those invariant conjuncts
		if(!is_null($allInvariantConjuctsIds)) {
			foreach ((array)$allInvariantConjuctsIds as $conjunctId){
				$violations = RuleEngine::checkConjunct($conjunctId);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					ErrorHandling::addLog("Violation of conjunct '".$conjunctId."'");
					
					foreach ($allConjuncts[$conjunctId]['invariantRuleNames'] as $ruleName){
						$rule = RuleEngine::getRule($ruleName);
						ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
					}
					
				}
			}

		// Otherwise check all invariantConjuncts
		}else{
			foreach (RuleEngine::getAllInvariantRulesNames() as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
			
				$violations = RuleEngine::checkRule($rule);
				if(!empty($violations)) {
					$invariantRulesHold = false;
					ErrorHandling::addInvariant("Violation of rule '".$rule['name']."'");
				}
			}
		}
		
		return $invariantRulesHold;
		
	}
	
	public static function checkRule($rule){
		$db = Database::singleton();
		$violations = array();
		
		ErrorHandling::addLog("Checking rule '" . $rule['name']."'");
		try{
			foreach($rule['conjuncts'] as $conjunct){
				$result = array_merge((array)$result, $db->Exe($conjunct['violationsSQL']));
			}
			
			if(count($result) == 0){
				ErrorHandling::addInfo("Rule '".$rule['name']."' holds");
				
			}else{
				$violations = $result;
			}
			return $violations;
		}catch (Exception $e){
			ErrorHandling::addError("While evaluating rule '".$rule['name']."': ".$e->getMessage);
		}
	}
	
	private function checkConjunct($conjunctId){
		ErrorHandling::addLog("Checking conjunct '" . $conjunctId."'");
		try{
			$db = Database::singleton();
			
			global $allConjuncts; // from Generics;
			
			$result = $db->Exe($allConjuncts[$conjunctId]['violationsSQL']);
			
			if(count($result) == 0){
				ErrorHandling::addInfo("Conjunct '".$conjunctId."' holds");
			}else{
				$violations = $result;
			}
			
			return $violations;			
			
		}catch (Exception $e){
			ErrorHandling::addError("While checking conjunct '".$conjunctId."': ".$e->getMessage);
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