<?php

class Config {
	
	public static function get($configVar, $scope = 'global'){
		if (!isset($GLOBALS[$scope][$configVar])) return null; //throw new Exception("Variable $configVar in scope $scope does not exists", 500);
		
		return $GLOBALS[$scope][$configVar];
	}
	
	public static function set($configVar, $scope, $value){
		$GLOBALS[$scope][$configVar] = $value;
		
		return true;
	}
}

// Default configuration
Config::set('contextName', 'global', $contextName); // set the name of the application context

Config::set('serverURL', 'global', 'http://localhost/' . Config::get('contextName')); // set the base url for the application
Config::set('apiPath', 'global', '/api/v1'); // relative path to api

Config::set('sessionExpirationTime', 'global', 60*60); // expiration time in seconds
Config::set('productionEnv', 'global', false); // set environment as production deployment (or not = default)

Config::set('uploadPath', 'global', __DIR__ . '/../uploads'); // absolute path to folder, without trailing slash
Config::set('allowedMimeTypes', 'global', array('application/vnd.ms-excel'
	                                           ,'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
	                                           ,'application/excel'
	                                           ,'application/pdf'
	                                           ,'text/xml'
	                                           ));

Config::set('loginEnabled', 'global', false); // enable/disable login functionality (requires Ampersand script, see localSettings.php)


Config::set('checkDefaultPopulation', 'transactions', true); // For debugging set to false (commits the initial population, regardless if it has invariant violations)
Config::set('ignoreInvariantViolations', 'transactions', false); // for debugging can be set to true (transactions will be committed regardless off invariant violations)

// Default CRUD rights for interfaces
Config::set('defaultCrudC', 'transactions', true);
Config::set('defaultCrudR', 'transactions', true);
Config::set('defaultCrudU', 'transactions', true);
Config::set('defaultCrudD', 'transactions', true);

?>