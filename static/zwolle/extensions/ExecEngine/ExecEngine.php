<?php
// Define hooks
$hook1 = array('class' => 'ExecEngine', 'function' => 'run', 'filename' => 'ExecEngine.php', 'filepath' => 'extensions/ExecEngine', 'params' => array());
Hooks::addHook('preDatabaseCloseTransaction', $hook1);
$hook2 = array('class' => 'ExecEngine', 'function' => 'run', 'filename' => 'ExecEngine.php', 'filepath' => 'extensions/ExecEngine', 'params' => array(true));
Hooks::addHook('postDatabaseReinstallDB', $hook2);

// UI
$GLOBALS['navBar']['refreshMenu'][] = array ( 'url' =>	'extensions/ExecEngine/ui/views/MenuItem.html');
AngularApp::addJS('extensions/ExecEngine/ui/js/ExecEngine.js');

// Config (can be overwritten in localSettings.php)
Config::set('execEngineRoleName', 'execEngine', 'ExecEngine');
Config::set('autoRerun', 'execEngine', true);
Config::set('maxRunCount', 'execEngine', 10);

class ExecEngine {
	
	private static $roleName;
	public static $doRun = true;
	public static $autoRerun;
	public static $runCount;
	
	public static function run($allRules = false){
		$database = Database::singleton();
		
		Notifications::addLog('------------------------- EXEC ENGINE STARTED -------------------------', 'ExecEngine');
		
		// Load the execEngine functions (security hazard :P)
		$files = getDirectoryList(__DIR__ . '/functions');
		foreach ($files as $file){
			if (substr($file,-3) !== 'php') continue;
			require_once __DIR__.'/functions/'.$file;
			Notifications::addLog('Included file: '.__DIR__ .'/functions/'.$file, 'ExecEngine');
		}
		
		self::$roleName = Config::get('execEngineRoleName', 'execEngine');
		$role = Role::getRoleByName(self::$roleName);
		
		$maxRunCount = Config::get('maxRunCount', 'execEngine');
		self::$runCount = 0;
		self::$autoRerun = Config::get('autoRerun', 'execEngine');
		
		if($role){
			// Get all rules that are maintained by the ExecEngine
			while(self::$doRun){
				self::$doRun = false;
				self::$runCount++;
				if(self::$runCount > $maxRunCount) throw new Exception('Maximum reruns exceeded for ExecEngine (rules with violations:' . implode(', ', $rulesThatHaveViolations). ')', 500);
				
				Notifications::addLog("ExecEngine run #" . self::$runCount . " (auto rerun: " . var_export(self::$autoRerun, true) . ") for role '" . $role->label . "'", 'ExecEngine');
				
				// Determine affected rules that must be checked by the exec engine
				$affectedConjuncts = (array)RuleEngine::getAffectedInvConjuncts($database->getAffectedConcepts(), $database->getAffectedRelations());
				$affectedConjuncts = array_merge($affectedConjuncts, (array)RuleEngine::getAffectedSigConjuncts($database->getAffectedConcepts(), $database->getAffectedRelations()));
				
				$affectedRules = array();
				foreach($affectedConjuncts as $conjunctId){
					$conjunct = RuleEngine::getConjunct($conjunctId);
					foreach ($conjunct['invariantRuleNames'] as $ruleName) $affectedRules[] = $ruleName;
					foreach ($conjunct['signalRuleNames'] as $ruleName) $affectedRules[] = $ruleName;
				}
				
				// Check rules
				$rulesThatHaveViolations = array();
				foreach ($role->maintains() as $ruleName){
					if(!in_array($ruleName, $affectedRules) && !$allRules) continue; // skip this rule
					
					$rule = RuleEngine::getRule($ruleName);
					$violations = RuleEngine::checkRule($rule, false);
					
					if(count($violations)){
						$rulesThatHaveViolations[] = $rule['name'];
						
						// Fix violations for every rule
						ExecEngine::fixViolations($rule, $violations); // Conjunct violations are not cached, because they are fixed by the ExecEngine
						
						// If $autoRerun, set $doRun to true because violations have been fixed (this may fire other execEngine rules)
						if(self::$autoRerun) self::$doRun = true;
					}
					 
				}
				
				
			}
			
		}else{
			Notifications::addInfo("ExecEngine extension included but role '" . self::$roleName . "' not found.");
		}
		
		Notifications::addLog('------------------------- END OF EXEC ENGINE -------------------------', 'ExecEngine');
				
	}
	
	public static function fixViolations($rule, $violations){
		if(count($violations)){
			Notifications::addLog('ExecEngine fixing violations for rule: ' . $rule['name'], 'ExecEngine');
			
			foreach ($violations as $violation){
				$theMessage = ExecEngine::getPairView($violation['src'], $rule['srcConcept'], $violation['tgt'], $rule['tgtConcept'], $rule['pairView']);
				
				// This function tries to return a string with all NULL bytes, HTML and PHP tags stripped from a given str. Strip_tags() is binary safe.
				// $theCleanMessage = strip_tags($theMessage);
				
				// Determine actions/functions to be taken
				$functionsToBeCalled = explode('{EX}', $theMessage);
				
				// Execute actions/functions
				foreach ($functionsToBeCalled as $functionToBeCalled) {
					if(empty($functionToBeCalled)) continue; // skips to the next iteration if $functionToBeCalled is empty. This is the case when violation text starts with delimiter {EX}
					
					// Determine delimiter
					if(substr($functionToBeCalled, 0, 2) == '_;'){
						$delimiter = '_;';
						$functionToBeCalled = substr($functionToBeCalled, 2);
					}else{
						$delimiter = ';';
					}
					
					$params = explode($delimiter, $functionToBeCalled); // Split off variables
					$params = array_map('trim', $params); // Trim all params
					$params = array_map('phpArgumentInterpreter', $params); // Evaluate phpArguments, using phpArgumentInterpreter function
					
					$function = array_shift($params); // First parameter is function name
					$classMethod = (array)explode('::', $function);
					
					if (function_exists($function) || method_exists($classMethod[0], $classMethod[1])){
						$successMessage = call_user_func_array($function,$params);
						Notifications::addLog($successMessage, 'ExecEngine');
						
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
		foreach ((array)$pairView as $segment){ 
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
				//if(count($rows) > 1) throw new Exception('Expression of pairview results in more than one tgt atom', 501); // 501: Not implemented
				if(count($rows) == 0) $pairStrs[] = '_NULL';
				else{
					$str = '';
					foreach ($rows as $row){
						$str .= $row['tgt'] . '_AND';
					}
					$str = substr($str, 0, -4); // strip the last _AND
					$pairStrs[] = str_replace(array('{EX}','{php}'), '', $str); // prevent php interpreter by user input
				}
				// else $pairStrs[] = str_replace(array('{EX}','{php}'), '', $rows[0]['tgt']); // prevent php interpreter by user input

			// unknown segment
			}else{
				$errorMessage = "Unknown segmentType '" . $segment['segmentType'] . "' in pairview";
				throw new Exception($errorMessage, 501); // 501: Not implemented
			}
		}
		return implode($pairStrs);
	}

}
?>