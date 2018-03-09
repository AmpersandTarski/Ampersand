<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Misc\Config;
use Ampersand\Role;
use Ampersand\Log\Logger;
use Ampersand\Transaction;
use Ampersand\Rule\Violation;

class ExecEngine extends RuleEngine
{
    
    /**
     * Specifies if ExecEngine should run or not. Can be used to halt the ExecEngine at some point
     * Public variable, because it can also be set to false by an ExecEngine function using 'ExecEngine::$doRun = false;'
     *
     * @var bool
     */
    public static $doRun = true;

    /**
     * Specifies if ExecEngine should automatically run a second time (to check for new violations)
     *
     * @var bool
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
    public static $runCount = 0;

    /**
     * List of callables (functions) that can used by the ExecEngine
     *
     * @var array
     */
    protected static $callables = [];
    
    /**
     * Specifies latest atom created by a newstruct function call.
     * Can be (re)used within the scope of one violation statement
     *
     * @var \Ampersand\Core\Atom
     */
    public static $_NEW = null;
    
    /**
     * Get logger for ExecEngine
     *
     * @return \Psr\Log\LoggerInterface
     */
    public static function getLogger()
    {
        return Logger::getLogger('EXECENGINE');
    }
    
    /**
     * Run all ExecEngine roles
     * Default/standard role used in Ampersand scripts is 'ExecEngine', but other roles can be configured
     *
     * @param bool $allRules
     * @return void
     */
    public static function run(bool $allRules = false)
    {
        $logger = self::getLogger();

        self::$maxRunCount = Config::get('maxRunCount', 'execEngine');
        self::$autoRerun = Config::get('autoRerun', 'execEngine');

        $logger->info("ExecEngine started");
        
        $roles = [];
        foreach ((array) Config::get('execEngineRoleNames', 'execEngine') as $roleName) {
            try {
                $roles[] = Role::getRoleByName($roleName);
            } catch (Exception $e) {
                $logger->debug("ExecEngine role '{$roleName}' configured, but role is not used/defined in &-script.");
            }
        }

        $rulesFixed = [];
        do {
            foreach ($roles as $role) {
                self::$runCount++;

                // Prevent infinite loop in ExecEngine reruns
                if (self::$runCount > self::$maxRunCount) {
                    $logger->error("Maximum reruns exceeded (hint! rules fixed in last run:" . implode(', ', $rulesFixed) . ")");
                    Logger::getUserLogger()->error("Maximum reruns exceeded for ExecEngine");
                    break 2; // break foreach and do-while loop
                }

                $logger->info("Run #" . self::$runCount . " using role '{$role}' (auto rerun: " . var_export(self::$autoRerun, true) . ")");
                
                $rulesFixed = array_merge($rulesFixed, self::runForRole($role, $allRules));
            }

            // If no rules fixed (i.e. no violations) in this loop: stop ExecEngine
            if (empty($rulesFixed)) {
                self::$doRun = false;
            }
        } while (self::$doRun && self::$autoRerun); // self::$doRun can also be set to false by some ExecEngine function

        $logger->info("ExecEngine finished");
    }

    /**
     * Single run for a given ExecEngine role
     *
     * @param \Ampersand\Role $role
     * @param bool $allRules
     * @return string[]
     */
    protected static function runForRole(Role $role, bool $allRules): array
    {
        $logger = self::getLogger();
        
        $rulesFixed = [];
        $rulesToCheck = $allRules ? $role->maintains() : Transaction::getCurrentTransaction()->getAffectedRules($role->maintains());
        foreach ($rulesToCheck as $rule) {
            $violations = $rule->checkRule(false); // param false to force (re)evaluation of conjuncts
            
            if (empty($violations)) {
                continue; // continue to next rule when no violation
            }
            
            // Fix violations
            $total = count($violations);
            $logger->debug("ExecEngine fixing {$total} violations for rule '{$rule}'");
            foreach ($violations as $key => $violation) {
                $num = $key + 1;
                $logger->debug("Fixing violation {$num}/{$total}: ({$violation->src},{$violation->tgt})");
                self::fixViolation($violation);
            }
            $rulesFixed[] = $rule->id;
            $logger->info("ExecEngine fixed {$total} violations for rule '{$rule}'");
        }

        return $rulesFixed;
    }
    
    /**
     * Fix violations
     *
     * @param \Ampersand\Rule\Violation $violation
     * @return void
     */
    protected static function fixViolation(Violation $violation)
    {
        $logger = self::getLogger();
        
        // Newly created atom (e.g. by NewStruct function) can be (re)used inside the scope of the violation in which it was created.
        self::$_NEW = null;

        // Determine actions/functions to be taken
        $actions = explode('{EX}', $violation->getExecEngineViolationMessage());
        
        // Execute actions/functions to fix this violation
        foreach ($actions as $action) {
            if (empty($action)) {
                continue; // skips to the next iteration if $action is empty. This is the case when violation text starts with delimiter {EX}
            }
            
            // Determine delimiter
            if (substr($action, 0, 2) == '_;') {
                $delimiter = '_;';
                $action = substr($action, 2);
            } else {
                $delimiter = ';';
            }
            
            // Split off variables
            $params = explode($delimiter, $action);
            //$params = array_map('trim', $params); // trim all params // commented out, because atoms can have spaces in them
            
            // Evaluate php statement if provided as param
            $params = array_map(function ($param) {
                // If php function is provided, evaluate this.
                // Limited security issue, because '{php}' can only be specified in &-script. '{php}' in user input is filtered out when getting violation message
                // Only 1 php statement can be executed, due to semicolon issue: PHP statements must be properly terminated using a semicolon, but the semicolon is already used to seperate the parameters
                // e.g. {php}date(DATE_ISO8601) returns the current datetime in ISO 8601 date format
                if (substr($param, 0, 5) == '{php}') {
                    $code = 'return('.substr($param, 5).');';
                    $param = eval($code);
                }
                return $param;
            }, $params);
            
            $functionName = trim(array_shift($params)); // first parameter is function name
            $callable = self::getFunction($functionName);
            try {
                $logger->info("{$functionName}(" . implode(',', $params) . ")");
                call_user_func_array($callable, $params);
            
            // Catch exceptions from ExecEngine functions and log to user
            } catch (Exception $e) {
                Logger::getUserLogger()->error("{$functionName}: {$e->getMessage()}");
            }
        }
    }

    /**
     * Get registered ExecEngine function
     *
     * @param string $functionName
     * @return callable
     */
    public static function getFunction(string $functionName): callable
    {
        if (array_key_exists($functionName, self::$callables)) {
            return self::$callables[$functionName];
        } else {
            throw new Exception("Function '{$functionName}' does not exist. Register ExecEngine function.", 500);
        }
    }

    /**
     * Register functions that can be used by the ExecEngine to fix violations
     *
     * @param string $name
     * @param callable $callable
     * @return void
     */
    public static function registerFunction(string $name, callable $callable)
    {
        if (empty($name)) {
            throw new Exception("ExecEngine function must be given a name. Empty string/0/null provided", 500);
        }
        if (array_key_exists($name, self::$callables)) {
            throw new Exception("ExecEngine function '{$name}' already exists", 500);
        }
        self::$callables[$name] = $callable;
        self::getLogger()->debug("ExecEngine function '{$name}' registered");
    }
}
