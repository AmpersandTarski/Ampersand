<?php

require_once (__DIR__.'/../Generics.php'); // loading the Ampersand model
require_once (__DIR__.'/../db/Database.php');
require_once (__DIR__.'/Role.php');
require_once (__DIR__.'/Viewer.php');

define("EXPIRATION_TIME", 60*60 ); // expiration time in seconds


class Session {
	
	/******* Roles *******/
	
	private $database;
	public $role;
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){
		global $conceptTableInfo;
		
		// PHP SESSION : Start a new, or resume the existing, PHP session
		session_start(); 
		
		// Database connection for within this class
		$this->database = Database::singleton();
		
		// AMPERSAND SESSION
		if (array_key_exists('SESSION', $conceptTableInfo)){ // Only execute following code when concept SESSION is used by adl script

			try {
				$database->Exe("SELECT * FROM `__SessionTimeout__` WHERE false");
			} catch (Exception $e) {
				return;
			}
			
			// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
			$expiredSessionsAtoms = array_column($database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE lastAccess < ".time() - EXPIRATION_TIME), 1);
			foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->deleteAmpersandSession($expiredSessionAtom);
			
			// Create a new Ampersand session if $_SESSION['sessionAtom'] is not set (browser started a new session or Ampersand session was expired
			if (!Concept::isAtomInConcept($_SESSION['sessionAtom'], 'SESSION')){ 
				$_SESSION['sessionAtom']  = $database->createNewAtom('SESSION'); // TODO: change to PHP SESSION ID??
			}

			$database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".$_SESSION[sessionAtom]."', '".time()."')".
			"ON DUPLICATE KEY UPDATE `lastAccess` = '".$time()."'"); //TODO move to Database::Exe function
		}
		
		// ROLE
		if(isset($_REQUEST['role'])){	// new role selected
			 $roleId = $_REQUEST['role'];
		}elseif(isset($_SESSION['role'])){	// role already selected
			$roleId = $_SESSION['role'];
		}else{	// default role
			$roleId = -1;	// TODO: how to handle no role selected
		}
		
		$_SESSION['role'] = $roleId;	// store roleId in $_SESSION['role'] 
		$this->role = new Role($roleId);
		
		
		// TODO interface aanmaken
	}
	
	// Prevent any copy of this object
	private function __clone()
	{
		
	}
	
	public static function singleton()
	{
		if(!is_object (self::$_instance) ) self::$_instance = new Session();
		return self::$_instance;
	}
	
	public function destroySession(){
		global $conceptTableInfo;
		
		if (array_key_exists('SESSION', $conceptTableInfo)){
			$this->deleteAmpersandSession($_SESSION['sessionAtom']);
		}
		
		$_SESSION = array(); // empty all $_SESSION variables
		
		session_destroy(); // session_destroy() destroys all of the data associated with the current session. It does not unset any of the global variables associated with the session, or unset the session cookie.
		
	}
	
	private function deleteAmpersandSession($sessionAtom){
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom($sessionAtom, 'SESSION');
	
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
	
	
	// TODO: make separate class for Interface
	public static function getInterface($interfaceName){
		global $allInterfaceObjects; // from Generics.php
		
		return $allInterfaceObjects[$interfaceName];
	
	}
	
	

}



?>