<?php

define("EXPIRATION_TIME", 60*60 ); // expiration time in seconds

class Session {
	
	public $database;
	public $role;
	public $interface;
	public $viewer;
	public $atom;
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){
		global $conceptTableInfo;
		
		// PHP SESSION : Start a new, or resume the existing, PHP session
		session_start(); 
		
		// Database connection for within this class
		try {
			$this->database = Database::singleton();
			
			// AMPERSAND SESSION
			if (array_key_exists('SESSION', $conceptTableInfo)){ // Only execute following code when concept SESSION is used by adl script

				try {
					$this->database->Exe("SELECT * FROM `__SessionTimeout__` WHERE false");
				} catch (Exception $e) {
					ErrorHandling::addError('Cannot access database. Make sure the MySQL server is running, or <a href="installer/" class="alert-link">create a new database</a>');
					return;
				}
				
				// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
				$expiredSessionsAtoms = array_column($this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - EXPIRATION_TIME)), 'SESSION');
				foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->deleteAmpersandSession($expiredSessionAtom);
				
				// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
				if (!Concept::isAtomInConcept(session_id(), 'SESSION')){ 
					$this->database->addAtomToConcept(session_id(), 'SESSION');					
				}

				$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".session_id()."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
			}else{
				ErrorHandling::addError('Script does not contain SESSION concept!');
				return;
			}
	
		} catch (Exception $e){
		  	return;
		}	
	}
	
	// Prevent any copy of this object
	private function __clone()
	{
		
	}
	
	public static function singleton()
	{
		if(is_null (self::$_instance) ) self::$_instance = new Session();
		return self::$_instance;
	}
	
	public function destroySession(){
		global $conceptTableInfo;
		
		$this->deleteAmpersandSession(session_id());
		
		$_SESSION = array(); // empty all $_SESSION variables
		
		session_destroy(); // session_destroy() destroys all of the data associated with the current session. It does not unset any of the global variables associated with the session, or unset the session cookie.
		
		self::$_instance = null;
	}
	
	public function setRole($roleId = null){
		try{
			if(isset($roleId)){
				$this->role = new Role($roleId);	
			}elseif(isset($_SESSION['role'])){
				$this->role = new Role($_SESSION['role']);
			}else{
				$this->role = new Role();
			}
			
			ErrorHandling::addLog("Role $role->name selected");
			$_SESSION['role'] = $this->role->id;	// store roleId in $_SESSION['role']
	
			RuleEngine::checkRules($this->role->id); // TODO: ergens anders plaatsen?
			return $this->role->id;
		}catch(Exception $e){
			throw $e;
		}
	}
	
	public function setInterface($interfaceName = null){
		
		try{
			if(isset($interfaceName)) {
				$this->interface = new ObjectInterface($interfaceName);
				ErrorHandling::addLog("Interface $interfaceName selected");
			}else{
				$this->interface = null;
				ErrorHandling::addNotification("No interface selected");
			}
			$_SESSION['interface'] = $interfaceName; // store interfaceName in $_SESSION['interface']
		}catch (Exception $e){
			throw $e;
		}
		
		return $interfaceName;
	}
	
	public function setAtom($atomId = null){
		
		if(isset($atomId)){
			$this->atom = $atomId;
			ErrorHandling::addLog("Atom $atomId selected");
		}else{
			$this->atom = session_id();
		}
		$_SESSION['atom'] = $atomId; // store atomId in $_SESSION['atom]
		
		return $atomId;
	}
	
	public function setViewer($viewerName = null){ 
		if(!isset($viewerName)) $viewerName = 'AmpersandViewer'; // TODO: config voor default viewer maken
		
		$_SESSION['viewer'] = $viewerName; // store viewerName in $_SESSION['viewer']
		
		try{
			$viewerClass = $GLOBALS['viewers'][$viewerName]['class'];
			if(!class_exists($viewerClass)) throw new Exception("Specified viewer: $viewerName does not exists");
			$this->viewer = new $viewerClass($this->interface, $this->atom);
		}catch (Exception $e){
			ErrorHandling::addError($e->getMessage);
		}
		return $viewerName;
	}
	
	
	private function deleteAmpersandSession($sessionAtom){
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom($sessionAtom, 'SESSION');
	
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
	
}

?>