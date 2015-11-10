<?php

// PHP SESSION : Start a new, or resume the existing, PHP session
session_start();
Notifications::addLog('Session id: ' . session_id(), 'SESSION');

class Session {
	
	public $id;
	public $database;
	public $activeRoles = array();
	public $interface;
	public $viewer;
	public $atom;
	public $accessibleInterfaces = array();
	public static $sessionUser;
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){		
		try {
			$this->id = session_id();
			
			$this->database = Database::singleton();
			
			// AMPERSAND SESSION
			Concept::getConcept('SESSION');
			
			// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
			$expiredSessionsAtoms = array_column($this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - Config::get('sessionExpirationTime'))), 'SESSION');
			foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->destroyAmpersandSession($expiredSessionAtom);

			// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
			$sessionAtom = new Atom($this->id, 'SESSION');
			if (!$sessionAtom->atomExists()){ 
				$this->database->addAtomToConcept($this->id, 'SESSION');
				$this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
			}

			$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".$this->id."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
			
		} catch (Exception $e){
		  	throw $e;
		}
	}
	
	// Prevent any copy of this object
	private function __clone(){}
	
	public static function singleton(){
		if(is_null (self::$_instance) ) self::$_instance = new Session();
		return self::$_instance;
	}
	
	private function destroyAmpersandSession($sessionAtom){
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom($sessionAtom, 'SESSION');
		$this->database->commitTransaction();
	}
	
	public function destroySession(){
		$this->destroyAmpersandSession(session_id());
		session_regenerate_id(true);
		$this->id = session_id();
		
	}
	
	public function activateRoles($roleIds = null){
		$roles = Config::get('loginEnabled') ? Role::getAllSessionRoles() : Role::getAllRoleObjects();
		if(empty($roles)){
			Notifications::addLog("No roles available to activate", 'SESSION');	
		}elseif(is_null($roleIds)){
			// TODO: insert default roles here
			Notifications::addLog("No activate roles", 'SESSION');
		}elseif(empty($roleIds)){
			Notifications::addLog("No activate roles", 'SESSION');
		}else{
			if(!is_array($roleIds)) throw new Exception ('$roleIds must be an array', 500);
			foreach($roles as $role){
				if(in_array($role->id, $roleIds)) $this->activeRoles[] = $role;
				Notifications::addLog("Role $role->id is activate", 'SESSION');
			}
			if(!isset($this->role)) throw new Exception("You do not have access to the selected role", 401);
		}
		
		if(Config::get('loginEnabled')){
			$arr = array();
			foreach($roles as $role){
				$arr = array_merge($arr, $role->interfaces);
			}
			$this->accessibleInterfaces = array_unique($arr);
		}

		return $this->activeRoles;
	}
	
	public function setInterface($interfaceId){
		
		if(isset($interfaceId)) {
			if(!$this->role->isInterfaceForRole($interfaceId)) throw new Exception('Interface is not accessible for specified role: '.$this->role->name.' (roleId:' . $this->role->id .')', 401); // 401: Unauthorized
			
			$this->interface = new InterfaceObject($interfaceId);
			Notifications::addLog("Interface '". $this->interface->label . "' selected", 'SESSION');
				
		}else{
			throw new Exception('No interface specified', 404);
		}
	}
	
	private static function setSessionUser(){
		// Set sessionUser
		if(!Config::get('loginEnabled')){
			Session::$sessionUser = false;
		
		}else{
			$ifc = new InterfaceObject('SessionUser');
			$session = new Atom(session_id(), 'SESSION');
			$sessionUsers = array_keys((array)$session->getContent($ifc, true));
				
			if(count($sessionUsers) > 1) throw new Exception('Multiple session users found. This is not allowed.', 500);
			if(empty($sessionUsers)){
				Session::$sessionUser = false;
			}else{
				Session::$sessionUser = current($sessionUsers);
				Notifications::addLog("Session user set to '$sessionUser'", 'SESSION');
			}
		}		
	}
	
	public static function getSessionUserId(){
		if(!Config::get('loginEnabled')){
			return 'SYSTEM';
		
		}else{
			if(!isset(Session::$sessionUser)) Session::setSessionUser();
			
			if(Session::$sessionUser === false){
				return $_SERVER['REMOTE_ADDR'];
			}else{
				return Session::$sessionUser;
			}
		}
	}
	
	public static function sessionUserLoggedIn(){
		if(!Config::get('loginEnabled')){
			return false;
		
		}else{
			if(!isset(Session::$sessionUser)) Session::setSessionUser();
				
			if(Session::$sessionUser === false){
				return false;
			}else{
				return true;
			}
		}
	}
	
	public static function getSessionVars(){
		if(!Config::get('loginEnabled')){
			return false;
		
		}else{
			try {
				$ifc = new InterfaceObject('SessionVars');
				$session = new Atom(session_id(), 'SESSION');
				return $session->getContent($ifc, false, null, false, 'num', false); // $rootElement = false => this will return a single object instead of array.
			}catch (Exception $e){
				return false;
			}		
			
		}
	}
}
?>