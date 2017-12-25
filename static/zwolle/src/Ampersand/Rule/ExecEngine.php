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
use Ampersand\Rule\Violation;

class ExecEngine {
    
    /**
     * Specifies if ExecEngine should run or not. Can be used to halt the ExecEngine at some point
     *
     * @var boolean
     */
    public static $doRun;

    /**
     * Specifies if ExecEngine should automatically run a second time (to check for new violations)
     *
     * @var boolean
     */
    public static $autoRerun;

    /**
     * Maximum number of ExecEngine runs (within a single transaction)
     *
     * @var int
     */
    public static $maxRunCount;
    
    /**
     * Number of executed ExecEngine runs
     *
     * @var int
     */
    public static $runCount;

    /**
     * List of callables (functions) that can used by the ExecEngine
     *
     * @var array
     */
    private static $callables = [];
    
	/**
     * Specifies latest atom created by a newstruct function call. 
     * Can be (re)used within the scope of one violation statement
     * 
	 * @var \Ampersand\Core\Atom
	 */
	public static $_NEW = null;
    
    /**
     * Undocumented function
     *
     * @param bool $allRules
     * @return void
     */
    public static function run(bool $allRules = false){
        $logger = Logger::getLogger('EXECENGINE');

        self::$maxRunCount = Config::get('maxRunCount', 'execEngine');
        self::$autoRerun = Config::get('autoRerun', 'execEngine');
        
        foreach((array) Config::get('execEngineRoleNames', 'execEngine') as $roleName){
            self::$runCount = 0;
            self::$doRun = true;
            try{
                $role = Role::getRoleByName($roleName);
            }catch (Exception $e){
                $logger->warning("ExecEngine role '{$roleName}' configured, but role is not used/defined in &-script.");
                continue;
            }
            self::runForRole($role, $allRules);
        }
    }

    /**
     * Undocumented function
     *
     * @param \Ampersand\Role $role
     * @param bool $allRules
     * @return void
     */
    public static function runForRole(Role $role, bool $allRules){
        $logger = Logger::getLogger('EXECENGINE');
        $logger->notice("ExecEngine '{$role}' started");

        // Get all rules that are maintained by the ExecEngine role
        $rulesThatHaveViolations = [];
        while(self::$doRun){
            self::$doRun = false;
            self::$runCount++;

            $logger->info("ExecEngine '{$role}' run #" . self::$runCount . " (auto rerun: " . var_export(self::$autoRerun, true) . ")");

            // Prevent infinite loop in ExecEngine reruns                 
            if(self::$runCount > self::$maxRunCount){
                Logger::getUserLogger()->error('Maximum reruns exceeded for ExecEngine (rules with violations:' . implode(', ', $rulesThatHaveViolations). ')');
                break;
            }
            
            // Determine which rules to check/fix
            if($allRules){
                $ruleNames = $role->maintains();
            }else{
                // Determine affected rules that must be checked by the exec engine
                $affectedConjuncts = RuleEngine::getAffectedConjuncts(Transaction::getCurrentTransaction()->getAffectedConcepts(), Transaction::getCurrentTransaction()->getAffectedRelations(), 'sig');
                $ruleNames = [];
                foreach($affectedConjuncts as $conjunct) $ruleNames = array_merge($ruleNames, $conjunct->sigRuleNames);
            }
            
            // Check rules
            $rulesThatHaveViolations = [];
            foreach ($ruleNames as $ruleName){
                $rule = Rule::getRule($ruleName);
                $violations = $rule->getViolations(false); // param false to force (re)evaluation of conjuncts
                
                if(empty($violations)) continue; // continue to next rule when no violation
                
                $rulesThatHaveViolations[] = $rule->id;
                
                // Fix violations
                $total = count($violations);
                $logger->debug("ExecEngine fixing {$total} violations for rule '{$rule}'");
                foreach($violations as $key => $violation){
                    $num = $key + 1;
                    $logger->debug("Fixing violation {$num}/{$total}: ({$violation->src},{$violation->tgt})");
                    self::fixViolation($violation);
                }
                $logger->info("ExecEngine fixed {$total} violations for rule '{$rule}'");
                
                // If $autoRerun, then set $doRun to true because violations that are fixed may fire other ExecEngine rules
                if(self::$autoRerun) self::$doRun = true;
            }    
        }
        
        $logger->notice("ExecEngine '{$role}' completed");
    }
    
    /**
     * Fix violation
     * 
     * @param \Ampersand\Rule\Violation $violation
     * @return void
     */
    public static function fixViolation(Violation $violation){
        $logger = Logger::getLogger('EXECENGINE');
        
        // Newly created atom (e.g. by NewStruct function) can be (re)used inside the scope of the violation in which it was created.     
        self::$_NEW = null;

        // Determine actions/functions to be taken
        $actions = explode('{EX}', $violation->getExecEngineViolationMessage());
        
        // Execute actions/functions to fix this violation
        foreach ($actions as $action) {
            if(empty($action)) continue; // skips to the next iteration if $action is empty. This is the case when violation text starts with delimiter {EX}
            
            // Determine delimiter
            if(substr($action, 0, 2) == '_;'){
                $delimiter = '_;';
                $action = substr($action, 2);
            }else{
                $delimiter = ';';
            }
            
            // Split off variables
            $params = explode($delimiter, $action);
            //$params = array_map('trim', $params); // trim all params // commented out, because atoms can have spaces in them
            
            // Evaluate php statement if provided as param
            $params = array_map(function($param){
                // If php function is provided, evaluate this.
                // Limited security issue, because '{php}' can only be specified in &-script. '{php}' in user input is filtered out when getting violation message
                // Only 1 php statement can be executed, due to semicolon issue: PHP statements must be properly terminated using a semicolon, but the semicolon is already used to seperate the parameters
                // e.g. {php}date(DATE_ISO8601) returns the current datetime in ISO 8601 date format
                if(substr($param, 0, 5) == '{php}') {
                    $code = 'return('.substr($param, 5).');';
                    $param = eval($code);
                }
                return $param;
            }, $params);
            
            $functionName = trim(array_shift($params)); // first parameter is function name
            
            if(array_key_exists($functionName, self::$callables)){
                call_user_func_array(self::$callables[$functionName], $params);                    
            }else{
                throw new Exception("Function '{$functionName}' does not exists. Register ExecEngine function.", 500);
            }
        }
    }

    public static function registerFunction(string $name, callable $callable){
        if(empty($name)) throw new Exception("ExecEngine function must be given a name. Empty string/0/null provided");
        if(array_key_exists($name, self::$callables)) throw new Exception("ExecEngine function '{$name}' already exists", 500);
        self::$callables[$name] = $callable;
        Logger::getLogger('EXECENGINE')->debug("ExecEngine function '{$name}' registered");
    }
}

?>