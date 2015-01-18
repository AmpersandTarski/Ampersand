<?php

// Define hooks
$hooks['before_Database_transaction_checkInvariantRules'][] = 'ExecEngine::run';

class ExecEngine {
	
	private $defaultRoleName = 'ExecEngine'; // Can be set in localSettings.php using $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']
	
	public static function run(){
		
		$roleName = isset($GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']) ? $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName'] : $this->defaultRoleName;
		
		// Load the execEngine functions (security hazard :P)
		$files = getDirectoryList(__DIR__ . '/functions');
		foreach ($files as $file){
			if (substr($file,-3) !== 'php') continue;
			require_once __DIR__.'/functions/'.$file;
			ErrorHandling::addLog('Included file: '.__DIR__ .'/functions/'.$file);
		}
		
		$role = Role::getRole($roleName);
		if($role){
			// get all rules that are maintained by the ExecEngine
			foreach ($role->maintains as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
					
				// fix violations for every rule
				ExecEngine::fixViolations($rule, RuleEngine::checkRule($rule));
			}
		}else{
			ErrorHandling::addError("ExecEngine role '" . $roleName . "'not found.");
		}
				
	}
	
	public static function fixViolations($rule, $violations){
		
		foreach ($violations as $violation){
			$theMessage = ExecEngine::execPair($violation['src'], $rule['srcConcept'], $violation['tgt'], $rule['tgtConcept'], $rule['pairView']);
			
			// This function tries to return a string with all NULL bytes, HTML and PHP tags stripped from a given str. Strip_tags() is binary safe.
			$theCleanMessage = strip_tags($theMessage);
			
			// Determine actions/functions to be taken
			$functionsToBeCalled = explode('{EX}', $theCleanMessage);
			
			// Execute actions/functions
			foreach ($functionsToBeCalled as $functionToBeCalled) { 
			
				$params = explode(';', $functionToBeCalled); // Split off variables
				$params = array_map('trim', $params); // Trim all params
				
				$function = array_shift($params); // First parameter is function name
				
				if (function_exists($function)){ 
					$successMessages[] = call_user_func_array($function,$params);
				}else{
					$errorMessage = "Function '" . $function . "' does not exists. Create function with " . count($params) . " parameters";
					throw new Exception($errorMessage);
				}
			}
		}
		
		if(count($violations)){
			// provide success message for every fixed violation
			$id = ErrorHandling::addSuccess('ExecEngine fixed rule ' . $rule['name']);
			foreach ((array)$successMessages as $successMessage) ErrorHandling::addSuccess($successMessage, $id);
		}
	}

	public static function execPair($srcAtom, $srcConcept, $tgtAtom, $tgtConcept, $pairView){ 
		$database = Database::singleton();
		
		$pairStrs = array();
		foreach ($pairView as $segment){ 
			// text segment		
			if ($segment['segmentType'] == 'Text'){
				$pairStrs[] = $segment['Text'];
			
			// expressie segment
			}elseif($segment['segmentType'] == 'Exp'){
				// select starting atom depending on whether the segment uses the src of tgt atom.
				$atom = $segment['srcOrTgt'] == 'Src' ? $srcAtom : $tgtAtom;
				
				// quering the expression
				$query = "SELECT DISTINCT `tgt` FROM (".$segment['expSQL'].") AS results WHERE src='".addslashes($atom)."'"; // SRC of TGT kunnen door een expressie gevolgd worden
				$rows = $database->Exe($query);
				
				// returning the result
				if(count($row) > 1) throw new Exception('Expression of pairview results in more than one tgt atom');
				$pairStrs[] = $rows[0]['tgt'];

			// unknown segment
			}else{
				$errorMessage = "Unknown segmentType '" . $segment['segmentType'] . "' in pairview";
				throw new Exception($errorMessage);
			}
		}
		return implode($pairStrs);
	}
}

?>