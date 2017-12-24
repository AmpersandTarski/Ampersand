<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Config;
use Ampersand\Role;
use Ampersand\Log\Logger;
use Ampersand\Rule\Rule;
use Ampersand\Rule\RuleEngine;
use Ampersand\Interfacing\Transaction;

class ExecEngine {
    
    private static $roleName;
    public static $doRun = true;
    public static $autoRerun;
    public static $runCount;

    /**
     * List of callables (functions) that can used by the ExecEngine
     *
     * @var array
     */
    private static $callables = [];
    
	/**
	 * @var \Ampersand\Core\Atom $_NEW specifies latest atom created by a newstruct function call. Can be (re)used within the scope of one violation statement. 
	 */
	public static $_NEW = null;
	
    public static function run($allRules = false){
        $logger = Logger::getLogger('EXECENGINE');
        
        $logger->info("ExecEngine run started");
        
        self::$roleName = Config::get('execEngineRoleName', 'execEngine');
        try{
            $role = Role::getRoleByName(self::$roleName);
        }catch (Exception $e){
            $logger->warning("ExecEngine extension included but role '" . self::$roleName . "' not used/defined in &-script.");
            self::$doRun = false; // prevent exec engine execution
        }
        
        $maxRunCount = Config::get('maxRunCount', 'execEngine');
        self::$runCount = 0;
        self::$autoRerun = Config::get('autoRerun', 'execEngine');
        
        // Get all rules that are maintained by the ExecEngine
        $rulesThatHaveViolations = array();
        while(self::$doRun){
            self::$doRun = false;
            self::$runCount++;

            // Prevent infinite loop in ExecEngine reruns                 
            if(self::$runCount > $maxRunCount){
                Logger::getUserLogger()->error('Maximum reruns exceeded for ExecEngine (rules with violations:' . implode(', ', $rulesThatHaveViolations). ')');
                break;
            }
            
            $logger->notice("ExecEngine run #" . self::$runCount . " (auto rerun: " . var_export(self::$autoRerun, true) . ") for role '{$role->label}'");
            
            // Determine affected rules that must be checked by the exec engine
            $affectedConjuncts = RuleEngine::getAffectedConjuncts(Transaction::getCurrentTransaction()->getAffectedConcepts(), Transaction::getCurrentTransaction()->getAffectedRelations(), 'sig');
            
            $affectedRules = array();
            foreach($affectedConjuncts as $conjunct) $affectedRules = array_merge($affectedRules, $conjunct->sigRuleNames);
            
            // Check rules
            $rulesThatHaveViolations = array();
            foreach ($role->maintains() as $ruleName){
                if(!in_array($ruleName, $affectedRules) && !$allRules) continue; // skip this rule
                
                $rule = Rule::getRule($ruleName);
                $violations = $rule->getViolations(false);
                
                if(count($violations)){
                    $rulesThatHaveViolations[] = $rule->id;
                    
                    // Fix violations for every rule
                    $logger->notice("ExecEngine fixing " . count($violations) . " violations for rule '{$rule->id}'");
                    self::fixViolations($violations); // Conjunct violations are not cached, because they are fixed by the ExecEngine
                    $logger->debug("Fixed " . count($violations) . " violations for rule '{$rule}'");
                    
                    // If $autoRerun, set $doRun to true because violations have been fixed (this may fire other execEngine rules)
                    if(self::$autoRerun) self::$doRun = true;
                }
            }    
        }
        
        $logger->info("ExecEngine run completed");    
    }
    
    /**
     * 
     * @param Violation[] $violations
     * @throws Exception
     * @return void
     */
    public static function fixViolations($violations){
        $logger = Logger::getLogger('EXECENGINE');
        $total = count($violations);
        
        foreach ($violations as $key => $violation){
            $num = $key + 1;
            $logger->info("Fixing violation {$num}/{$total}: ({$violation->src},{$violation->tgt})");
            
            $theMessage = $violation->getExecEngineViolationMessage();
            
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
                //$params = array_map('trim', $params); // Trim all params // Commented out, because atoms can have spaces in them
                
                // Evaluate php statement if provided as param
                $params = array_map(function($param){
                    // If php function is provided, evaluate this. Note! security hazard.
                    // Only 1 php statement can be executed, due to semicolon issue: PHP statements must be properly terminated using a semicolon, but the semicolon is already used to seperate the parameters
                    // e.g. {php}date(DATE_ISO8601) returns the current datetime in ISO 8601 date format
                    if(substr($param, 0, 5) == '{php}') {
                        $code = 'return('.substr($param, 5).');';
                        $param = eval($code);
                    }
                    return $param;
                }, $params);
                
                $function = trim(array_shift($params)); // First parameter is function name
                
                if(array_key_exists($function, self::$callables)){
                    call_user_func_array(self::$callables[$function], $params);                    
                }else{
                    throw new Exception("Function '{$function}' does not exists. Register ExecEngine function.", 500);
                }
            }
			
			self::$_NEW = null; // The newly created atom cannot be (re)used outside the scope of the violation in which it was created.
        }        
    }

    public static function registerFunction(string $name, callable $callable){
        if(empty($name)) throw new Exception("ExecEngine function must be given a name. Empty string/0/null provided")
        if(array_key_exists($name, self::$callables)) throw new Exception("ExecEngine function '{$name}' already exists", 500);
        self::$callables[$name] = $callable;
        Logger::getLogger('EXECENGINE')->debug("ExecEngine function '{$name}' registered");
    }
}

?>