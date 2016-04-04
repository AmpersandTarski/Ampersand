<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Config {
    
    /**
     * Contains all config variables
     * @var array
     */
    private static $configVars = array();
	
    /**
     * Config variable getter
     * @param string $configVar
     * @param string $scope
     * @return mixed|NULL
     */
	public static function get($configVar, $scope = 'global'){
		if (!isset(self::$configVars[$scope][$configVar])) return null; //throw new Exception("Variable $configVar in scope $scope does not exists", 500);
		
		return self::$configVars[$scope][$configVar];
	}
	
	/**
	 * Config variable setter
	 * @param string $configVar
	 * @param string $scope
	 * @param mixed $value
	 * @return void
	 */
	public static function set($configVar, $scope, $value){
		self::$configVars[$scope][$configVar] = $value;
	}
}

?>