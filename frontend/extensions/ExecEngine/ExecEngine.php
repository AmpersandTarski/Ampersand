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
				// $theCleanMessage = substr($theCleanMessage,4); // Strip {EX} tag
				
				// TODO: enable multiple function calls again
				
				// $functionsToBeCalled = explode('{EX}',$theCleanMessage); // Split off subsequent function calls 
				// if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[START]]");

				$functionToBeCalled = $theCleanMessage; // foreach ($functionsToBeCalled as $functionToBeCalled) { 
				
					$params = explode(';',$functionToBeCalled); // Split off variables
					$cleanparams = array();
					foreach ($params as $param) $cleanparams[] = trim($param);
					$params = $cleanparams;
					unset($cleanparams);
					
					// ExecEngineWhispers($functionToBeCalled);
					
					$func = array_shift($params); // First parameter is function name
					if (function_exists($func)){ 
						call_user_func_array($func,$params);
					} else { 
						die ("Function does not exists"); // TODO: proper ErrorHandling
						// ExecEngineSHOUTS("TODO: Create function $func with " . count($params) . " parameters.");
					}
				// }
				
				// if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[DONE]]");
			
			}
		
			// provide ErrorHandling class a success message for every fixed rule
			ErrorHandling::addSuccess('ExecEngine fixed rule: '.$rule['name']);
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



// Load the functions from the functions folder:
// (security hazard :P)
function getDirectoryList ($directory) 
{   // create an array to hold directory list
    $results = array();
    // create a handler for the directory
    $handler = opendir($directory);
    // open directory and walk through the filenames
    while ($file = readdir($handler))
    {   // if file isn't this directory or its parent, add it to the results
        if ($file != "." && $file != "..")
        {   $results[] = $file;
        }
    }
    // tidy up: close the handler
    closedir($handler);
    // done!
    return $results;
}

$files = getDirectoryList(__DIR__.'/functions');
foreach ($files as $file)
{ if (substr($file,-3) !== 'php') continue;
  require_once __DIR__.'/functions/'.$file;
  //echo "Included file: " . __DIR__ . '/functions/' . $file . "\n<br/>";
}

?>