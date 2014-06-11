<?php

// Define hooks
$hooks['before_Database_transaction_checkInvariantRules'][] = 'ExecEngine::run';

class ExecEngine {
	
	public static function run(){
		// 
		$roleName = isset($GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']) ? $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName'] : 'ExecEngine'; // default ExecEngine roleName
		
		// loop over all roles to select ExecEngine role
		foreach(Role::getAllRoles() as $role){
			
			// if current role is the ExecEngine
			if($role->name == $roleName){
				
				// get all rules that are maintained by the ExecEngine
				foreach ($role->getRules() as $rule){
					
					// fix violations for every rule
					ExecEngine::fixViolations($rule, RuleEngine::checkProcessRule($rule));
				}
			}
		}
	}
	
	public static function fixViolations($rule, $violations){
		
		if(!empty($violations)){
		
			foreach ($violations as $violation){
				$theMessage = ExecEngine::execPair($violation['src'], $rule['srcConcept'], $violation['tgt'], $rule['tgtConcept'], $rule['pairView']);

				$theCleanMessage = strip_tags($theMessage);
				$functionsToBeCalled = explode('{EX}', $theCleanMessage);

				foreach ($functionsToBeCalled as $functionToBeCalled) { 
				
					$params = explode(';',$functionToBeCalled); // Split off variables
					foreach ($params as $param) $cleanparams[] = trim($param);
					$params = $cleanparams;
					unset($cleanparams);
					
					$func = array_shift($params); // First parameter is function name
					if (function_exists($func)){ 
						$successMessages[] = call_user_func_array($func,$params);
					} else { 
						throw new Exception ("Function '$func' does not exists. Create function $func with ".count($params)." parameters");
					}
				}
			}
		
			// provide ErrorHandling class a success message for every fixed rule
			$id = ErrorHandling::addSuccess('ExecEngine fixed rule: '.$rule['name']);
			foreach ((array)$successMessages as $successMessage) ErrorHandling::addSuccess($successMessage, $id);			
			
		}		
	}

	public static function execPair($srcAtom, $srcConcept, $tgtAtom, $tgtConcept, $pairView){ 
		$database = Database::singleton();
		
		$pairStrs = array();
		foreach ($pairView as $segment){ 
			if ($segment['segmentType'] == 'Text'){
				$pairStrs[] = $segment['Text'];
			}else{ 
				$atom    = $segment['srcOrTgt'] == 'Src' ? $srcAtom : $tgtAtom;
				// $concept = $segment['srcOrTgt'] == 'Src' ? $srcConcept : $tgtConcept;
				$query = "SELECT DISTINCT `tgt` FROM (".$segment['expSQL'].") AS results WHERE src='".addslashes($atom)."'"; // SRC of TGT kunnen door een expressie gevolgd worden
				$rows = $database->Exe($query);
				$pairStrs[] = $rows[0]['tgt']; // Even er van uit gaan dat we maar 1 atoom kunnen behandelen...
			}
		}
		return implode($pairStrs);
	}
}


// Load the  from the functions folder: (security hazard :P)
$files = getDirectoryList(__DIR__.'/functions');
foreach ($files as $file){ 
	if (substr($file,-3) !== 'php') continue;
	require_once __DIR__.'/functions/'.$file;
	ErrorHandling::addLog('Included file: '.__DIR__ .'/functions/'.$file); 
}

?>