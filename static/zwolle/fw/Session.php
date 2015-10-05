<?php

define("EXPIRATION_TIME", 60*60 ); // expiration time in seconds

// PHP SESSION : Start a new, or resume the existing, PHP session
session_start();
Notifications::addLog('Session id: ' . session_id(), 'SESSION');

class Session {
	
	public $database;
	public $role;
	public $interface;
	public $viewer;
	public $atom;
	public $accessibleInterfaces = array();
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){
		global $allConcepts;
		
		// if(!is_null($sessionId)) session_id($sessionId); // set php session_id, must be done before session_start(); 
		
		try {
			$this->database = Database::singleton();
			
			// AMPERSAND SESSION
			if (array_key_exists('SESSION', $allConcepts)){ // Only execute following code when concept SESSION is used by adl script
				
				// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
				$expiredSessionsAtoms = array_column($this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - EXPIRATION_TIME)), 'SESSION');
				foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->destroySession($expiredSessionAtom);

				// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
				$sessionAtom = new Atom(session_id(), 'SESSION');
				if (!$sessionAtom->atomExists()){ 
					$this->database->addAtomToConcept(session_id(), 'SESSION');
					$this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
				}

				$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".session_id()."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
				
			}else{
				throw new Exception('Script does not contain SESSION concept!', 500);
			}
			
		} catch (Exception $e){
		  	throw $e;
		}
		
	}
	
	// Prevent any copy of this object
	private function __clone(){}
	
	public static function singleton($sessionId = null){
		if(is_null (self::$_instance) ) self::$_instance = new Session();
		return self::$_instance;
	}
	
	public function destroySession($sessionAtom){
		
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom($sessionAtom, 'SESSION');
		$this->database->closeTransaction('Session deleted', false);
		session_regenerate_id(true);
		
	}
	
	public function setRole($roleId = null){
		$roles = LOGIN_ENABLED ? Role::getAllSessionRoles(session_id()) : Role::getAllRoleObjects();
		if(empty($roles) || $roleId == 0) $this->role = new Role(0); // select role 0, no role
		elseif(is_null($roleId)) $this->role = current($roles); // select first of $roles
		elseif(isset($roleId)){
			if(!is_int($roleId)) throw new Exception ("roleId must be an integer", 400);
			foreach($roles as $role){
				if($role->id == $roleId) $this->role = $role;
			}
			if(!isset($this->role)) throw new Exception("You do not have access to the selected role", 401);
		}else{
			throw new Exception("No role could be selected", 500);
		}
		
		if(LOGIN_ENABLED){
			$arr = array();
			foreach($roles as $role){
				$arr = array_merge($arr, $role->interfaces);
			}
			$this->accessibleInterfaces = array_unique($arr);
			
		}
		
		Notifications::addLog("Role " . $this->role->name . " selected", 'SESSION');

		return $this->role->id;
	}
	
	public function setInterface($interfaceId){
		
		if(isset($interfaceId)) {
			if(!$this->role->isInterfaceForRole($interfaceId)) throw new Exception('Interface is not accessible for specified role: '.$this->role->name.' (roleId:' . $this->role->id .')', 403); // 403: Forbidden
			
			$this->interface = new InterfaceObject($interfaceId);
			Notifications::addLog("Interface '". $this->interface->label . "' selected", 'SESSION');
				
		}else{
			throw new Exception('No interface specified', 404);
		}
	}
}
?>