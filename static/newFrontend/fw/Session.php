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
	private function __construct($sessionId){
		global $allConcepts;
		
		if(!is_null($sessionId)) session_id($sessionId); // set php session_id, must be done before session_star()t; 
			
		// PHP SESSION : Start a new, or resume the existing, PHP session
		session_start(); 
		
		// Database connection for within this class
		try {
			
			$this->database = Database::singleton();
			
			// AMPERSAND SESSION
			if (array_key_exists('SESSION', $allConcepts)){ // Only execute following code when concept SESSION is used by adl script
				
				try {
					$this->database->Exe("SELECT * FROM `__SessionTimeout__` WHERE false");
				} catch (Exception $e) {
					ErrorHandling::addError('Cannot access database. Make sure the MySQL server is running, or <a href="installer/" class="alert-link">create a new database</a>');
					return;
				}
				
				// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
				$expiredSessionsAtoms = array_column($this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - EXPIRATION_TIME)), 'SESSION');
				foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->destroySession($expiredSessionAtom);

				// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
				if (!Concept::isAtomInConcept(session_id(), 'SESSION')){ 
					$this->database->addAtomToConcept(session_id(), 'SESSION');
					$this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
				}

				$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".session_id()."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
				
			}else{
				ErrorHandling::addError('Script does not contain SESSION concept!');
				throw new Exception('Script does not contain SESSION concept!');
				return;
			}
			
		} catch (Exception $e){
		  	throw $e;
		}
		
	}
	
	// Prevent any copy of this object
	private function __clone()
	{
		
	}
	
	public static function singleton($sessionId = null)
	{
		if(is_null (self::$_instance) ) self::$_instance = new Session($sessionId);
		return self::$_instance;
	}
	
	public function destroySession($sessionAtom){
		
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom($sessionAtom, 'SESSION');
		$this->database->closeTransaction('Session deleted', false);
		session_regenerate_id(true);
		
	}
	
	public function setRole($roleId = null){
		try{
			if(isset($roleId)){
				$this->role = new Role($roleId);	
			}else{
				$this->role = new Role();
			}
			
			ErrorHandling::addLog("Role " . $this->role->name . " selected");

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
				ErrorHandling::addInfo("No interface selected");
			}
		}catch (Exception $e){
			throw $e;
		}
		
		return $interfaceName;
	}
	
	public function setAtom($atomId = null){
		
		if(isset($atomId)){
			$this->atom = $atomId;
		}elseif(is_null($atomId)){
			$this->atom = session_id();
			$atomId = session_id();
		}else{
			$this->atom = session_id();
			$atomId = session_id();
		}
		ErrorHandling::addLog("Atom $atomId selected");
		
		return $atomId;
	}
}

?>