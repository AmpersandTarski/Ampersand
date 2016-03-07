<?php

// PHP SESSION : Start a new, or resume the existing, PHP session
session_start();
Notifications::addLog('Session id: ' . session_id(), 'SESSION');

class Session {
	
	public $id;
	public $database;
	public $interface;
	public $viewer;
	public $atom;
	
	private $sessionRoles; // when login enabled: all roles for loggedin user, otherwise all roles
	
	private $accessibleInterfaces = array(); // when login enabled: all interfaces for sessionRoles, otherwise: interfaces for active roles
	private $ifcsOfActiveRoles = array(); // interfaces for active roles
	public $rulesToMaintain = array(); // rules that are maintained by active roles 
	
	public static $sessionAccountId;
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){		
		try {
			$this->id = session_id();
			
			$this->database = Database::singleton();
			
			// AMPERSAND SESSION
			Concept::getConcept('SESSION');
			
			// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
			$expiredSessionsAtoms = array_column((array)$this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - Config::get('sessionExpirationTime'))), 'SESSION');
			foreach ($expiredSessionsAtoms as $expiredSessionAtom) $this->destroyAmpersandSession($expiredSessionAtom);

			// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
			$sessionAtom = new Atom($this->id, 'SESSION');
			if (!$sessionAtom->atomExists()){ 
				$this->database->addAtomToConcept($this->id, 'SESSION');
				$this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
			}

			$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".$this->id."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
			
			// Add public interfaces
			$this->accessibleInterfaces = array_merge($this->accessibleInterfaces, InterfaceObject::getPublicInterfaces());
			
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
		$roles = $this->getSessionRoles();
		if(empty($roles)){
			Notifications::addLog("No roles available to activate", 'SESSION');	
		}elseif(is_null($roleIds)){
			Notifications::addLog("Activate default roles", 'SESSION');
			foreach($this->sessionRoles as &$role) $this->activateRole($role);
		}elseif(empty($roleIds)){
			Notifications::addLog("No roles provided to activate", 'SESSION');
		}else{
			if(!is_array($roleIds)) throw new Exception ('$roleIds must be an array', 500);
			foreach($this->sessionRoles as &$role){
				if(in_array($role->id, $roleIds)) $this->activateRole($role);
			}
		}
		
		// Add public interfaces
		$this->ifcsOfActiveRoles = array_merge($this->ifcsOfActiveRoles, InterfaceObject::getPublicInterfaces());
		
		// If login enabled, add also the other interfaces of the sessionRoles (incl. not activated roles) to the accesible interfaces
		if(Config::get('loginEnabled')){
			foreach($roles as $role){
				$this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
			}
		}
		
		// Filter duplicate values
		$this->ifcsOfActiveRoles = array_unique($this->ifcsOfActiveRoles);
		$this->accessibleInterfaces = array_unique($this->accessibleInterfaces);
		$this->rulesToMaintain = array_unique($this->rulesToMaintain);
	}
	
	private function activateRole(&$role){
	    $role->active = true;
	    Notifications::addLog("Role $role->id is active", 'SESSION');
	    $this->ifcsOfActiveRoles = array_merge($this->ifcsOfActiveRoles, $role->interfaces());
	    $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
	    $this->rulesToMaintain = array_merge($this->rulesToMaintain, $role->maintains());
	}
	
	public function setInterface($interfaceId){
		
		if(isset($interfaceId)) {
			if(!$this->isAccessibleIfc($interfaceId)) throw new Exception("Interface is not accessible for active roles or accessible roles (login)", 401); // 401: Unauthorized
			
			$this->interface = new InterfaceObject($interfaceId);
			Notifications::addLog("Interface '". $this->interface->label . "' selected", 'SESSION');
				
		}else{
			throw new Exception('No interface specified', 404);
		}
	}
	
	public function getSessionRoles(){
		if(isset($this->sessionRoles)){
			return $this->sessionRoles;
		}else{
			if(Config::get('loginEnabled')){
				$sessionRoleLabels = array();
				$sessionRoles = array();
				
				$interface = new InterfaceObject('SessionRoles');
				$session = new Atom(session_id(), 'SESSION');
				$options = array('metaData' => false, 'navIfc' => true);
				$sessionRoleLabels = array_column((array)$session->getContent($interface, $interface->id, null, $options), '_id_');
				
				foreach(Role::getAllRoleObjects() as $role){
					if(in_array($role->label, $sessionRoleLabels)) $sessionRoles[] = $role;
				}
			}else{
				$sessionRoles = Role::getAllRoleObjects();
			}
			
			return $this->sessionRoles = $sessionRoles;
		}
	}
	
	private static function setSessionAccount(){
		// Set $sessionAccountId
		if(!Config::get('loginEnabled')){
			self::$sessionAccountId = false;
		
		}else{
			$ifc = new InterfaceObject('SessionAccount');
			$session = new Atom(session_id(), 'SESSION');
			$sessionAccounts = array_column((array)$session->getContent($ifc, $ifc->id), '_id_');
				
			if(count($sessionAccounts) > 1) throw new Exception('Multiple session users found. This is not allowed.', 500);
			if(empty($sessionAccounts)){
				self::$sessionAccountId = false;
			}else{
				self::$sessionAccountId = current($sessionAccounts);
				Notifications::addLog("Session user set to '" . self::$sessionAccountId . "'", 'SESSION');
			}
		}		
	}
	
	public static function getSessionAccountId(){
		if(!Config::get('loginEnabled')){
			return 'SYSTEM';
		
		}else{
			if(!isset(self::$sessionAccountId)) Session::setSessionAccount();
			
			if(self::$sessionAccountId === false){
				return $_SERVER['REMOTE_ADDR'];
			}else{
				return self::$sessionAccountId;
			}
		}
	}
	
	public static function sessionUserLoggedIn(){
		if(!Config::get('loginEnabled')){
			return false;
		
		}else{
			if(!isset(self::$sessionAccountId)) Session::setSessionAccount();
				
			if(self::$sessionAccountId === false){
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
				$options = array('metaData' => false, 'navIfc' => false);
				return $session->getContent($ifc, $ifc->id, $session->id, $options);
			}catch (Exception $e){
				return false;
			}		
			
		}
	}
	
	public function getInterfacesForNavBar(){
		$interfaces = array();
		foreach($this->ifcsOfActiveRoles as $interface){
			if(($interface->srcConcept == 'SESSION' || $interface->srcConcept == 'ONE') && $interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToCreateAtom(){
		$interfaces = array();
		foreach($this->ifcsOfActiveRoles as $interface){
			//if($interface->srcConcept != 'SESSION' && $interface->srcConcept != 'ONE') $interfaces[] = $interface;
			if($interface->crudC && $interface->isIdent) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToReadConcept($concept){
		$interfaces = array();
		foreach($this->accessibleInterfaces as $interface){
			if(($interface->srcConcept == $concept || in_array($concept, Concept::getSpecializations($interface->srcConcept)))	&& $interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getEditableConcepts(){
		$editableConcepts = array();
		foreach($this->accessibleInterfaces as $ifc) $editableConcepts = array_merge($editableConcepts, $ifc->editableConcepts);
		return $editableConcepts;
	}
	
	public function isAccessibleIfc($interfaceId){
		return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->accessibleInterfaces));
	}
}
?>