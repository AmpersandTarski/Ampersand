<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Ampersand\Log\Logger;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Hooks {
	
	private static $hooks = array();
	
	public static function callHooks($hookpoint, $hookpointParams){
		
		Logger::getLogger('HOOKS')->debug("Hook '{$hookpoint}' called");
		
		foreach (Hooks::getHooks($hookpoint) as $hook){
			// Determine funtioncall
			if($hook['class']) $callback = $hook['class'] . '::' . $hook['function'];
			else $callback = $hook['function'];
			
			// Construct params
			$callBackParams = array();
			foreach((array)$hook['params'] as $param){
				if(substr((string)$param, 0, 1) == '$') $callBackParams[] = $hookpointParams[substr($param, 1)];
				else $callBackParams[] = $param;
			}
			
			$log = $callback . '(' . implode(', ', $callBackParams) . ')';
			Logger::getLogger('HOOKS')->debug("Call hook {$log}");
			
			call_user_func_array ($callback, $callBackParams);
			
		}
	}
	
	public static function getHooks($hookpoint){
		
		return (array)Hooks::$hooks[$hookpoint];
		
	}
	
	
	/*
	 * $hook = array(
	 * 'class'    => 'MyClass', // can be empty/null
	 * 'function' => 'Myfunction',
	 * 'filename' => 'Myclass.php',
	 * 'filepath' => 'extensions/path',
	 * 'params'   => array('$var', 'string', 'etc') // can be empty/null
	 * )
	 */
	public static function addHook($hookpoint, $hook){
		
		self::$hooks[$hookpoint][] = $hook;
		
		if($hook['class']) $log = $hook['class'] . '::';
		$log .= $hook['function'] . '(';
		$log .= implode(', ', $hook['params']) . ')'; 
		
		Logger::getLogger('HOOKS')->debug("Hook '{$log}' added to '{$hookpoint}'");
		
	}
}

