<?php

Class Hooks {
	
	private static $hooks = array();
	
	public static function callHooks($hookpoint, $hookpointParams){
		
		foreach (Hooks::getHooks($hookpoint) as $hook){
			// Determine funtioncall
			if($hook['class']) $callback = $hook['class'] . '::' . $hook['function'];
			else $callback = $hook['function'];
			
			// Construct params
			foreach((array)$hook['params'] as $param){
				if(substr((string)$param, 0, 1) == '$') $callBackParams[] = $hookpointParams[substr($param, 1)];
				else $callBackParams[] = $param;
			}
			
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
		
	}
}

