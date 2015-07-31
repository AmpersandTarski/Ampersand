<?php

// Define hooks
$GLOBALS['hooks']['before_Database_transaction_checkInvariantRules'][] = 'ExecEngine::run';
$GLOBALS['hooks']['before_API_getAllNotifications_getViolations'][] = 'ExecEngine::run';
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/ExecEngine/ui/js/ExecEngine.js';

// Zet extension in applications menu
$apps[] = array ( 'url' =>	'extensions/ExecEngine/ui/views/MenuItem.html');

class ExecEngine {
	
	private static $defaultRoleName = 'ExecEngine'; // Can be set in localSettings.php using $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']
	private static $roleName;
	private static $role;
	public static $doRun = true;
	
	public static function init(){
		
		// Load the execEngine functions (security hazard :P)
		$files = getDirectoryList(__DIR__ . '/functions');
		foreach ($files as $file){
			if (substr($file,-3) !== 'php') continue;
			require_once __DIR__.'/functions/'.$file;
			Notifications::addLog('Included file: '.__DIR__ .'/functions/'.$file);
		}
		
		self::$roleName = isset($GLOBALS['ext']['ExecEngine']['ExecEngineRoleName']) ? $GLOBALS['ext']['ExecEngine']['ExecEngineRoleName'] : self::defaultRoleName;
		self::$role = Role::getRoleByName(self::$roleName);
	}
	
	public static function run(){
		
		Notifications::addLog('------------------------- EXEC ENGINE STARTED -------------------------');
		
		if(self::$role){
			Notifications::addLog("For role '" . self::$roleName . "'");
			// Get all rules that are maintained by the ExecEngine
			while(self::$doRun){
				self::$doRun = false;
				
				Notifications::addLog("ExecEngine run");
				foreach (self::$role->maintains as $ruleName){
					$rule = RuleEngine::getRule($ruleName);
						
					// Fix violations for every rule
					ExecEngine::fixViolations($rule, RuleEngine::checkRule($rule, false)); // Conjunct violations are not cached, because they are fixed by the ExecEngine 
				}
			}
			
		}else{
			Notifications::addInfo("ExecEngine role '" . self::$roleName . "' not found.");
		}
		
		Notifications::addLog('------------------------- END OF EXEC ENGINE -------------------------');
				
	}
	
	public static function fixViolations($rule, $violations){
		if(count($violations)){
			Notifications::addLog('ExecEngine fixing violations for rule: ' . $rule['name']);
			
			foreach ($violations as $violation){
				$theMessage = ExecEngine::getPairView($violation['src'], $rule['srcConcept'], $violation['tgt'], $rule['tgtConcept'], $rule['pairView']);
				
				// This function tries to return a string with all NULL bytes, HTML and PHP tags stripped from a given str. Strip_tags() is binary safe.
				$theCleanMessage = strip_tags($theMessage);
				
				// Determine actions/functions to be taken
				$functionsToBeCalled = explode('{EX}', $theCleanMessage);
				
				// Execute actions/functions
				foreach ($functionsToBeCalled as $functionToBeCalled) { 
					
					if(empty($functionToBeCalled)) continue; // skips to the next iteration if $functionToBeCalled is empty. This is the case when violation text starts with delimiter {EX}
					
					$params = explode(';', $functionToBeCalled); // Split off variables
					$params = array_map('trim', $params); // Trim all params
					$params = array_map('phpArgumentInterpreter', $params); // Evaluate phpArguments, using phpArgumentInterpreter function
					
					$function = array_shift($params); // First parameter is function name
					
					if (function_exists($function)){
						$successMessage = call_user_func_array($function,$params);
						Notifications::addLog($successMessage);
						
					}else{
						$errorMessage = "Function '" . $function . "' does not exists. Create function with " . count($params) . " parameters";
						throw new Exception($errorMessage, 500);
					}
				}
			}
			Notifications::addInfo(self::$roleName . ' fixed violations for rule: ' . $rule['name'], 'ExecEngineSuccessMessage', self::$roleName . ' fixed violations');
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
				$atomEsc = $database->escape($atom);
				$query = "SELECT DISTINCT `tgt` FROM ($segment[expSQL]) AS `results` WHERE `src` = '$atomEsc'"; // SRC of TGT kunnen door een expressie gevolgd worden
				$rows = $database->Exe($query);
				
				// returning the result
				if(count($row) > 1) throw new Exception('Expression of pairview results in more than one tgt atom', 501); // 501: Not implemented
				$pairStrs[] = $rows[0]['tgt'];

			// unknown segment
			}else{
				$errorMessage = "Unknown segmentType '" . $segment['segmentType'] . "' in pairview";
				throw new Exception($errorMessage, 501); // 501: Not implemented
			}
		}
		return implode($pairStrs);
	}

}

ExecEngine::init();

?>