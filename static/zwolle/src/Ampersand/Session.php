<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Concept;
use Ampersand\Core\Atom;
use Ampersand\Rule\Rule;
use Ampersand\Log\Logger;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Session {
	
    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
	public $id;
	
	/**
	 * 
	 * @var Atom
	 */
	public $sessionAtom;
	
	public $database;
	public $interface;
	public $viewer;
	
	/**
	 * Specifies if session is expired
	 * @var boolean
	 */
	private $isExpired = false;
	
	private $sessionRoles; // when login enabled: all roles for loggedin user, otherwise all roles
	
	/**
	 * 
	 * @var InterfaceObject[]
	 */
	private $accessibleInterfaces = array(); // when login enabled: all interfaces for sessionRoles, otherwise: interfaces for active roles
	private $ifcsOfActiveRoles = array(); // interfaces for active roles
	public $rulesToMaintain = array(); // rules that are maintained by active roles 
	
	private $sessionAccountId;
	
	private static $_instance = null; // Needed for singleton() pattern of Session class
	
	// prevent any outside instantiation of this object
	private function __construct(){
	    try {
    	    $this->logger = Logger::getLogger('FW');
    		
    	    $this->id = session_id();
    	    $this->sessionAtom = new Atom($this->id, 'SESSION');
    		
    		$this->logger->debug("Session id: {$this->id}");
    		
    		$this->database = Database::singleton();
    		
    		// Check if 'SESSION' is defined as concept in Ampersand script
    		Concept::getConcept('SESSION');
    		
    		// Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
    		$expiredSessionsAtoms = array_column((array)$this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - Config::get('sessionExpirationTime'))), 'SESSION');
    		foreach ($expiredSessionsAtoms as $expiredSessionAtom){
    		    if($expiredSessionAtom == $this->id) $this->isExpired = true; 
    		    $this->destroyAmpersandSession($expiredSessionAtom);
    		}
    		
    		// Throw exception when session is expired AND login functionality is enabled 
    		if($this->isExpired && Config::get('loginEnabled')) throw new Exception ("Your session has expired, please login again", 440); // 440 Login Timeout -> is redirected by frontend to login page
    
    		// Create a new Ampersand session if session_id() is not in SESSION table (browser started a new session or Ampersand session was expired
    		$sessionAtom = new Atom($this->id, 'SESSION');
    		if (!$sessionAtom->atomExists()){ 
    			$sessionAtom->addAtom();
    			$this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
    		}
    
    		$this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".$this->id."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
    		
    		// Add public interfaces
    		$this->accessibleInterfaces = InterfaceObject::getPublicInterfaces();
	    }catch (Exception $e){
	        Logger::getUserLogger()->error("Session not initialized: {$e->getMessage()}");
	    }
	}
	
	// Prevent any copy of this object
	private function __clone(){}
	
	public static function singleton(){
		if(is_null (self::$_instance) ) self::$_instance = new Session();
		return self::$_instance;
	}
	
	public static function reInit(){
	    return self::$_instance = new Session();
	}
	
	private function destroyAmpersandSession($sessionAtom){
		$this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '".$sessionAtom."'");
		$this->database->deleteAtom(new Atom($sessionAtom, 'SESSION'));
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
			$this->logger->debug("No roles available to activate");	
		}elseif(is_null($roleIds)){
			$this->logger->debug("Activate default roles");
			foreach($this->sessionRoles as &$role) $this->activateRole($role);
		}elseif(empty($roleIds)){
			$this->logger->debug("No roles provided to activate");
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
	
	/**
	 * 
	 * @param Role $role
	 * @return void
	 */
	private function activateRole(&$role){
	    $role->active = true;
	    $this->ifcsOfActiveRoles = array_merge($this->ifcsOfActiveRoles, $role->interfaces());
	    $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
	    
	    foreach($role->maintains() as $ruleName){
	        $this->rulesToMaintain[] = Rule::getRule($ruleName);
	    }
	    
	    $this->logger->info("Role '{$role->id}' is activated");
	}
	
	public function getSessionRoles(){
		if(isset($this->sessionRoles)){
			return $this->sessionRoles;
		}else{
			if(Config::get('loginEnabled')){
				$sessionRoleLabels = array();
				$sessionRoles = array();
				
				$session = new Atom(session_id(), 'SESSION');
				$options = array('metaData' => false, 'navIfc' => true);
				$sessionRoleLabels = array_column((array)$session->ifc('SessionRoles')->getContent($options), '_id_');
				
				foreach(Role::getAllRoles() as $role){
					if(in_array($role->label, $sessionRoleLabels)) $sessionRoles[] = $role;
				}
			}else{
				$sessionRoles = Role::getAllRoles();
			}
			
			return $this->sessionRoles = $sessionRoles;
		}
	}
	
	/**
	 * 
	 * @return Role[]
	 */
	public function getActiveRoles(){
	    $activeRoles = array();
	    foreach ($this->getSessionRoles() as $role){
	        if($role->active) $activeRoles[] = $role; 
	    }
	    return $activeRoles;
	}
	
	private function setSessionAccount(){
		if(!Config::get('loginEnabled')){
			$this->sessionAccountId = false;
		
		}else{
			$session = new Atom(session_id(), 'SESSION');
			$sessionAccounts = array_column((array)$session->ifc('SessionAccount')->getContent(), '_id_');
				
			if(count($sessionAccounts) > 1) throw new Exception('Multiple session users found. This is not allowed.', 500);
			if(empty($sessionAccounts)){
				$this->sessionAccountId = false;
			}else{
				$this->sessionAccountId = current($sessionAccounts);
				$this->logger->debug("Session user set to '{$this->sessionAccountId}'");
			}
		}		
	}
	
	public function getSessionAccountId(){
		if(!Config::get('loginEnabled')){
			return 'SYSTEM';
		
		}else{
			if(!isset($this->sessionAccountId)) $this->setSessionAccount();
			
			if($this->sessionAccountId === false){
				return $_SERVER['REMOTE_ADDR'];
			}else{
				return $this->sessionAccountId;
			}
		}
	}
	
	public function sessionUserLoggedIn(){
		if(!Config::get('loginEnabled')){
			return false;
		
		}else{
			if(!isset($this->sessionAccountId)) $this->setSessionAccount();
				
			if($this->sessionAccountId === false){
				return false;
			}else{
				return true;
			}
		}
	}
	
	public function getSessionVars(){
		if(!Config::get('loginEnabled')){
			return false;
		
		}else{
			try {
				$options = array('metaData' => false, 'navIfc' => false);
				return $this->sessionAtom->ifc('SessionVars')->getContent($options);
			}catch (Exception $e){
				return false;
			}		
			
		}
	}
	
	public function getInterfacesForNavBar(){
		$interfaces = array();
		foreach($this->ifcsOfActiveRoles as $interface){
			if(($interface->srcConcept->name == 'SESSION' || $interface->srcConcept->name == 'ONE') && $interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToCreateAtom(){
		$interfaces = array();
		foreach($this->ifcsOfActiveRoles as $interface){
			//if($interface->srcConcept->name != 'SESSION' && $interface->srcConcept->name != 'ONE') $interfaces[] = $interface;
			if($interface->crudC && $interface->isIdent) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToReadConcept($conceptName){
		$interfaces = array();
		foreach($this->accessibleInterfaces as $interface){
			if(($interface->srcConcept->name == $conceptName || $interface->srcConcept->hasSpecialization($conceptName)) 
			        && $interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	/**
	 * 
	 * @param Concept $concept
	 * @return boolean
	 */
	public function isEditableConcept($concept){
		foreach($this->accessibleInterfaces as $ifc) 
		    if (in_array($concept, $ifc->editableConcepts)) return true;
		
		// Else
		return false;
	}
	
	public function isAccessibleIfc($interfaceId){
		return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->accessibleInterfaces));
	}
}
?>