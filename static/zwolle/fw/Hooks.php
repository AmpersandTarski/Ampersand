<?php

Class Hooks {
	
	private static $hooks = array();
	
	public static function callHooks($hookpoint, $hookpointParams){
		
		Notifications::addLog("Hook $hookpoint called", 'Hooks');
		
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
			Notifications::addLog("Call hook $log", 'Hooks');
			
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
		
		Hooks::$hooks[$hookpoint][] = $hook;
		
		if($hook['class']) $log = $hook['class'] . '::';
		$log .= $hook['function'] . '(';
		$log .= implode(', ', $hook['params']) . ')'; 
		
		Notifications::addLog("Hook $log added to $hookpoint", 'Hooks');
		
	}
}

