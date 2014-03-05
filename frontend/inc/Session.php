<?php

require_once (__DIR__.'/../Generics.php'); // loading the Ampersand model
require_once (__DIR__.'/../db/Database.php');
require_once (__DIR__.'/Role.php');


class Session {
	
	/******* Roles *******/
	
	public $role;
	public $newAtomInterfaces;
	
	public function __construct($roleId = null, $interfaceName = null){
		
		if($roleId != null){
			$this->role = new Role($roleId);
			
		}
		
		// TODO interface aanmaken
	}
	
	public static function getRoles(){
		$roles = array();
		global $allRoles; // from Generics.php
		
		foreach((array)$allRoles as $key => $arr){
			$roles[$key] = new Role($key);
		}
		
		return $roles;
	}
	
	/******* Rules *******/
	
	public static function getInvariantRules(){
		$rules = array();
		global $invariantRuleNames; // from Generics.php
		
		foreach((array)$invariantRuleNames as $ruleName){
			$rules[$ruleName] = Session::getRule($ruleName);		
		}
		
		return $rules;
		
	}
	
	public static function getRule($ruleName){
		global $allRulesSql; // from Generics.php
		
		return $allRulesSql[$ruleName];
	}
	
	/******* Interfaces *******/
	
	public function getInterfaces($srcConceptONE = null){ // $srcConceptONE: true, false, null (=all)
		global $allInterfaceObjects; // from Generics.php
		
		if($this->role){
			return $this->role->getInterfaces($srcConceptONE);
		}else{			
			return $allInterfaceObjects;
		}	
	}
	
	public static function getInterface($interfaceName){
		global $allInterfaceObjects; // from Generics.php
		
		return $allInterfaceObjects[$interfaceName];
	
	}
	
	

}



?>