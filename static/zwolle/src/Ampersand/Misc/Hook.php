<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Misc;

use Ampersand\Log\Logger;
use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Hook
{
    
    /**
     * List of all hookpoints that have hooks registered
     *
     * Structure:
     * ['hookpoint a' => [Hook1, Hook2, etc], 'hookpoint b' => [Hook2, Hook3, etc]]
     *
     * @var array[]
     */
    protected static $hooks = [];

    /**
     * List of all hookpoints that are defined/used by the backend framework
     *
     * Extensions that want to use the Hook class must register the hookpoints using Hook::registerHookpoint()
     *
     * @var string[]
     */
    protected static $hookpoints = ['preCloseTransaction', 'postCloseTransaction'];

    /**
     * Callable
     *
     * @var callable
     */
    protected $callable;

    /**
     * Ordered list of parameters that must be provided to the callable
     * If string '$...' is specified, then that variable from the calling scope is used
     *
     * @var array
     */
    protected $paramMap = [];
    
    /**
     * Constructor
     * Hook is registered to run at provided hookpoints
     *
     * @param string[] $hookpoints
     * @param callable $callable
     * @param array $paramMap
     */
    public function __construct(array $hookpoints, callable $callable, array $paramMap = [])
    {
        if (empty($hookpoints)) {
            throw new Exception("At least one hookpoint must be specified to register a Hook", 500);
        }

        $this->callable = $callable;
        $this->paramMap = $paramMap;

        foreach ($hookpoints as $hookpoint) {
            $this->addToHookpoint($hookpoint);
        }
        
        Logger::getLogger('HOOKS')->debug("Hook '{$this}' registered for '" . implode(', ', $hookpoints) . "'");
    }

    /**
     * Printable string of this hook
     *
     * @return string
     */
    public function __toString(): string
    {
        // See http://php.net/manual/en/language.types.callable.php

        // Callable passed by its name as string
        if (is_string($this->callable)) {
            return trim((string) $this->callable);
        } // Callable passed as array containing object/class and mathed
        elseif (is_array($this->callable)) {
            $arr = (array) $this->callable; // explicit cast to array prevents static analyzer to signal 'suspicious array access'
            
            // Callable is $object->method(), return Class::method
            if (is_object($arr[0])) {
                return sprintf("%s::%s", get_class($arr[0]), trim($arr[1]));
            } // Callable is static method Class::method
            else {
                return sprintf("%s::%s", trim($arr[0]), trim($arr[1]));
            }
        } // Callable passed as Closure (aka anonymous function)
        elseif ($this->callable instanceof \Closure) {
            return 'Anonymous function';
        } // Unknown
        else {
            return 'unknown callable type';
        }
    }

    /**
     * Add this hook to a certain hookpoint
     *
     * @param string $hookpoint
     * @return void
     */
    protected function addToHookpoint(string $hookpoint)
    {
        // Warning for unused/unregistered hookpoints
        if (!in_array($hookpoint, self::$hookpoints)) {
            Logger::getLogger('HOOKS')->warning("Hookpoint '{$hookpoint}' specified in hook '{$this}', but this hookpoint is nowhere used");
        }

        // Add hook to hookpoint
        self::$hooks[$hookpoint][] = $this;
    }

    /**
     * Undocumented function
     *
     * @param string $hookpoint
     * @param array $callingScopeVariables
     * @return void
     */
    public static function callHooks(string $hookpoint, array $callingScopeVariables)
    {
        foreach (self::getHooks($hookpoint) as $hook) {
            // Construct params
            $params = [];
            foreach ($hook->paramMap as $param) {
                // Variable
                if (substr((string)$param, 0, 1) == '$') {
                    $varName = substr($param, 1);
                    if (in_array($varName, $callingScopeVariables)) {
                        $params[] = $callingScopeVariables[$varName];
                    } else {
                        throw new Exception("Variable '{$varName}' required for hook '{$hook}', but not provided by calling scope of hookpoint '{$hookpoint}'", 500);
                    }
                } // Non-variable (e.g. string, int, null, etc)
                else {
                    $params[] = $param;
                }
            }

            // Call callable
            Logger::getLogger('HOOKS')->debug("Call hook '{$hook}(" . implode(', ', $params) . ")' for hookpoint '{$hookpoint}'");
            call_user_func_array($hook->callable, $params);
        }
    }
    
    /**
     * Get list of hooks for a certain hookpoint
     *
     * @param string $hookpoint
     * @return \Ampersand\Misc\Hook[]
     */
    public static function getHooks(string $hookpoint)
    {
        if (array_key_exists($hookpoint, self::$hooks)) {
            return (array) self::$hooks[$hookpoint];
        } else {
            return []; // return empty array if $hookpoint is not defined
        }
    }


    /**
     * Register use of a specific hookpoint
     *
     * @param string $hookpoint
     * @return void
     */
    public static function registerHookpoint(string $hookpoint)
    {
        if (in_array($hookpoint, self::$hookpoints)) {
            throw new Exception("Hookpoint name '{$hookpoint}' already defined/used", 500);
        }
        
        self::$hookpoints[] = $hookpoint;
    }
}
