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
 * Class of session objects
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Session {
    
    /**
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * @var string $id session identifier
     */
    public $id;
    
    /**
     * @var Atom $sessionAtom reference to corresponding session object (Atom) in &-domain
     */
    public $sessionAtom;
    
    /**
     * @var Database $database reference to database object
     */
    public $database;
    
    /**
     * @var Role[] $sessionRoles contains roles for loggedin user when login is enabled, otherwise all roles
     */
    private $sessionRoles; 
    
    /**
     * @var InterfaceObject[] $accessibleInterfaces contains interfaces for sessionRoles when login is enabled, otherwise interfaces for active roles
     */
    private $accessibleInterfaces = array();
    
    /**
     * @var array $rulesToMaintain
     */
    public $rulesToMaintain = array(); // rules that are maintained by active roles 
    
    /**
     * @var Atom|false $sessionAccount
     */
    private $sessionAccount;
    
    /**
     * @var boolean $sessionVarAffected flag that is set when session variable is changed
     * a session variable is a relation with SESSION as src or tgt)
     * this flag is returned to frontend to trigger a navigation bar refresh (e.g. after a user login)
     */
    private $sessionVarAffected;
    
    /**
     * @var Session $_instance needed for singleton() pattern of Session class
     */
    private static $_instance = null;
    
    public $navToOnCommit = null;
    public $navToOnRollback = null;
    
    /**
     * Constructor of Session class
     * private to prevent any outside instantiation of this object
     */
    private function __construct(){
        $this->logger = Logger::getLogger('FW');
        $this->database = Database::singleton();
        
        $conceptSession = Concept::getConceptByLabel('SESSION'); // Also checks if 'SESSION' is defined as concept in Ampersand script
        
        $this->id = session_id();
        $this->sessionAtom = new Atom($this->id, $conceptSession);
        
        $this->logger->debug("Session id: {$this->id}");
        
        // Remove expired Ampersand sessions from __SessionTimeout__ and all concept tables and relations where it appears.
        $expiredSessionsAtoms = array_column((array)$this->database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE `lastAccess` < ".(time() - Config::get('sessionExpirationTime'))), 'SESSION');
        foreach ($expiredSessionsAtoms as $expiredSessionAtom){
            if($expiredSessionAtom == $this->id){
                // Notify user that session is expired when login functionality is enabled 
                if(Config::get('loginEnabled')) Logger::getUserLogger()->warning("Your session has expired, please login again"); // 440 Login Timeout -> is redirected by frontend to login page
            }
            $this->destroyAmpersandSession($expiredSessionAtom);
        }
        
        // Create a new Ampersand session atom if not yet in SESSION table (browser started a new session or Ampersand session was expired)
        $sessionAtom = new Atom($this->id, $conceptSession);
        if (!$sessionAtom->atomExists()){ 
            $sessionAtom->addAtom();
            $this->database->commitTransaction(); //TODO: ook door Database->closeTransaction() laten doen, maar die verwijst terug naar Session class voor de checkrules. Oneindige loop
        }

        $this->database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('".$this->id."', '".time()."') ON DUPLICATE KEY UPDATE `lastAccess` = '".time()."'");
        
        // Add public interfaces
        $this->accessibleInterfaces = InterfaceObject::getPublicInterfaces();
    }
    
    /**
     * private method to prevent any copy of this object
     */
    private function __clone(){}
    
    /**
     * @return Session
     */
    public static function singleton(){
        if(is_null (self::$_instance) ) self::$_instance = new Session();
        return self::$_instance;
    }
    
    /**
     * Function reinitializes the Session object (calls constructor again)
     * @return Session
     */
    public static function reInit(){
        return self::$_instance = new Session();
    }
    
    /**
     * Delete provided Ampersand session atom from database
     * @param string $sessionAtomId ampersand session atom id
     * @return void
     */
    private function destroyAmpersandSession($sessionAtomId){
        $this->database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '{$sessionAtomId}'");
        $atom = new Atom($sessionAtomId, Concept::getConceptByLabel('SESSION'));
        $atom->deleteAtom();
        $this->database->commitTransaction();
    }
    
    /**
     * Destroy php session, delete Ampersand session atom from db and reinitialize Session object
     * @return void
     */
    public function destroySession(){
        $this->destroyAmpersandSession($this->id);
        session_regenerate_id(true);
        $this->reInit();
    }
    
    /**
     * Activatie provided roles (if allowed)
     * @param array $roleIds list of role ids that must be activated
     * @return void
     */
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
        
        // If login enabled, add also the other interfaces of the sessionRoles (incl. not activated roles) to the accesible interfaces
        if(Config::get('loginEnabled')){
            foreach($roles as $role){
                $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
            }
        }
        
        // Filter duplicate values
        $this->accessibleInterfaces = array_unique($this->accessibleInterfaces);
        $this->rulesToMaintain = array_unique($this->rulesToMaintain);
    }
    
    /**
     * Activate provided role
     * @param Role $role
     * @return void
     */
    private function activateRole(&$role){
        $role->active = true;
        $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
        
        foreach($role->maintains() as $ruleName){
            $this->rulesToMaintain[] = Rule::getRule($ruleName);
        }
        
        $this->logger->info("Role '{$role->id}' is activated");
    }
    
    /**
     * Get session roles (i.e. allowed roles for the current loggedin user (if login is enabled) or all roles otherwise)
     * @return Role[]
     */
    public function getSessionRoles(){
        if(!isset($this->sessionRoles)){
            $sessionRoles = array();
            if(Config::get('loginEnabled')){
                $this->logger->debug("Getting interface 'SessionRoles' for {$this->sessionAtom->__toString()}");
                $sessionRoleLabels = $this->getSessionRoleLabels();
                foreach(Role::getAllRoles() as $role){
                    if(in_array($role->label, $sessionRoleLabels)) $sessionRoles[] = $role;
                }
            }else{
                $sessionRoles = Role::getAllRoles();
            }
            
            $this->sessionRoles = $sessionRoles;
        }
        return $this->sessionRoles;
    }
    
    /**
     * Returns the labels of the session roles
     * @return string[]
     */
    public function getSessionRoleLabels(){
        return array_map(function($o){
                return $o->id;
            }, $this->sessionAtom->ifc('SessionRoles')->getTgtAtoms());
    }
    
    /**
     * Returns true if the session roles contain any of the provided roles or provided roles are NULL. Returns false otherwise
     * @param array|null $roleLabels that have access rigths
     * @return boolean
     * @throws Exception 500 when $roles param is not an array or NULL
     */
    public function hasAccess($roleLabels = []){
        if(is_null($roleLabels)) return true;
        if(Config::get('loginEnabled', 'global') === false) return true;
        if(!is_array($roleLabels)) throw new Exception("Array (or null) expected to check access rights. Non-array provided.", 500);
        
        foreach($this->getSessionRoleLabels() as $sRole){
            if(in_array($sRole, $roleLabels)) return true;
        }
        
        return false;
    }
    
    /**
     * Get active roles
     * @return Role[]
     */
    public function getActiveRoles(){
        $activeRoles = array();
        foreach ($this->getSessionRoles() as $role){
            if($role->active) $activeRoles[] = $role; 
        }
        return $activeRoles;
    }
    
    /**
     * Get session account
     * @return Atom|false returns Ampersand account atom when there is a session account or false otherwise
     */
    public function getSessionAccount(){
        if(!isset($this->sessionAccount)){
            if(!Config::get('loginEnabled')){
                $this->sessionAccount = false;
                $this->logger->debug("Set sessionAccount: login not enabled");
            }else{
                $this->logger->debug("Getting interface 'SessionAccount' for {$this->sessionAtom->__toString()}");
                $sessionAccounts = $this->sessionAtom->ifc('SessionAccount')->getTgtAtoms();
                
                if(count($sessionAccounts) > 1) throw new Exception('Multiple session users found. This is not allowed.', 500);
                if(empty($sessionAccounts)){
                    $this->sessionAccount = false;
                    $this->logger->debug("Set sessionAccount: no session account");
                }else{
                    $this->sessionAccount = current($sessionAccounts);
                    $this->logger->debug("Set sessionAccount: '{$this->sessionAccount->id}'");
                }
            }
        }
        return $this->sessionAccount;
    }
    
    /**
     * Determine is there is a loggedin user (account)
     * @return boolean
     */
    public function sessionUserLoggedIn(){
        if(!Config::get('loginEnabled')){
            return false;
        }elseif($this->getSessionAccount() !== false){
            return true;
        }else{
            return false;
        }
    }
    
    /**
     * Get session variables (from 'SessionVars' interface)
     * @return mixed|false session variables (if interface 'SessionVars' is defined in &-script) or false otherwise
     */
    public function getSessionVars(){
        if(InterfaceObject::interfaceExists('SessionVars')){
            try {
                $this->logger->debug("Getting interface 'SessionVars' for {$this->sessionAtom->__toString()}");
                return $this->sessionAtom->ifc('SessionVars')->read(['metaData' => false, 'navIfc' => false]);
            }catch (Exception $e){
                $this->logger->warning("Error while getting SessionVars interface: " . $e->getMessage());
                return false;
            }
        }else{
            return false;
        }
    }    
    
    /**
     * Get interfaces that are accessible in the current session to 'Read' a certain concept
     * @param Concept $concept
     * @return InterfaceObject[]
     */
    public function getInterfacesToReadConcept($concept){
        $interfaces = array();
        foreach($this->accessibleInterfaces as $interface){
            if(($interface->srcConcept == $concept || $interface->srcConcept->hasSpecialization($concept)) 
                    && $interface->crudR
                    && (!$interface->crudC or ($interface->crudU or $interface->crudD))
                    ) $interfaces[] = $interface;
        }
        return $interfaces;
    }
    
    /**
     * Determine if provided concept is editable concept in one of the accessible interfaces in the current session
     * @param Concept $concept
     * @return boolean
     */
    public function isEditableConcept($concept){
        return array_reduce($this->accessibleInterfaces, function($carry, $ifc) use ($concept){
            return ($carry || in_array($concept, $ifc->editableConcepts));
        }, false);
    }
    
    /**
     * Determine if provided interface is accessible in the current session
     * @param string $interfaceId
     * @return boolean
     */
    public function isAccessibleIfc($interfaceId){
        return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->accessibleInterfaces));
    }
    
    /**
     * Flag session variable as affected
     * @param boolean $bool
     * @return void
     */
    public function setSessionVarAffected($bool = true){
        $this->sessionVarAffected = $bool;
    }
    
    /**
     * Returns if flag for session var affected is set
     * @return boolean
     */
    public function getSessionVarAffected(){
        return $this->sessionVarAffected;
    }
    
    /**
     * Returns if session refresh is adviced in frontend
     * True when
     * - session variable is affected (otherwise nothing to update)
     * - AND transaction request is 'promise' (otherwise rollback)
     * - AND invariant rules hold (otherwise rollback)
     * False otherwise
     * @return boolean
     */
    public function getSessionRefreshAdvice(){
        return $this->getSessionVarAffected() 
                && ($this->database->getRequestType() == 'promise')
                && $this->database->getInvariantRulesHold();
    }
    
    public function getNavToResponse(){
    	return $this->database->getInvariantRulesHold() ? $this->navToOnCommit : $this->navToOnRollback;
    }
    
    public function setNavToOnCommit($navTo){
    	$this->navToOnCommit = $navTo;
    }
    
    public function setNavToOnRollback($navTo){
    	$this->navToOnRollback = $navTo;
    }
}
?>