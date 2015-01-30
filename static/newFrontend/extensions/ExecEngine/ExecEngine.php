<?php

// Define hooks
$hooks['before_Database_transaction_checkInvariantRules'][] = 'ExecEngine::run';

class ExecEngine {
	
	private $defaultRoleName = 'ExecEngine'; // Can be set in localSettings.php using $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']
	
	public static function run(){
		
		ErrorHandling::addLog('------------------------- EXEC ENGINE STARTED -------------------------');
		
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
			// Get all rules that are maintained by the ExecEngine
			foreach ($role->maintains as $ruleName){
				$rule = RuleEngine::getRule($ruleName);
					
				// Fix violations for every rule
				ExecEngine::fixViolations($rule, RuleEngine::checkRule($rule, false)); // Conjunct violations are not cached, because they are fixed by the ExecEngine 
			}
		}else{
			ErrorHandling::addError("ExecEngine role '" . $roleName . "'not found.");
		}
		
		ErrorHandling::addLog('------------------------- END OF EXEC ENGINE -------------------------');
				
	}
	
	public static function fixViolations($rule, $violations){
		if(count($violations)){
			ErrorHandling::addLog('ExecEngine fixing rule ' . $rule['name']);
			
			foreach ($violations as $violation){
				$theMessage = ExecEngine::getPairView($violation['src'], $rule['srcConcept'], $violation['tgt'], $rule['tgtConcept'], $rule['pairView']);
				
				// This function tries to return a string with all NULL bytes, HTML and PHP tags stripped from a given str. Strip_tags() is binary safe.
				$theCleanMessage = strip_tags($theMessage);
				
				// Determine actions/functions to be taken
				$functionsToBeCalled = explode('{EX}', $theCleanMessage);
				
				// Execute actions/functions
				foreach ($functionsToBeCalled as $functionToBeCalled) { 
				
					$params = explode(';', $functionToBeCalled); // Split off variables
					$params = array_map('trim', $params); // Trim all params
					$params = array_map('phpArgumentInterpreter', $params); // Evaluate phpArguments, using phpArgumentInterpreter function
					
					$function = array_shift($params); // First parameter is function name
					
					if (function_exists($function)){
						$successMessage = call_user_func_array($function,$params);
						ErrorHandling::addLog($successMessage);
						
					}else{
						$errorMessage = "Function '" . $function . "' does not exists. Create function with " . count($params) . " parameters";
						throw new Exception($errorMessage);
					}
				}
			}
			ErrorHandling::addSuccess('ExecEngine fixed rule ' . $rule['name']);
		}
	}

	// Almost a copy of RuleEngine::getPairView()
	public static function getPairView($srcAtom, $srcConcept, $tgtAtom, $tgtConcept, $pairView){ 
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