<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Interfacing\Resource;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Concept;
use Ampersand\Core\Atom;
use Ampersand\Rule\Rule;
use Ampersand\Log\Logger;
use Ampersand\Interfacing\Transaction;

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
     * @var Resource $sessionResource reference to corresponding session object which can be used with interfaces
     */
    public $sessionResource;
    
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
    
    /**
     * Constructor of Session class
     * private to prevent any outside instantiation of this object
     */
    private function __construct(){
        $this->logger = Logger::getLogger('SESSION');
        $this->id = session_id();
        $this->logger->debug("Session id set to: {$this->id}");
        
        /* Don't put anymore logic here, to prevent multiple Session objects
         * Instead use initSession(), called by singleton() right after initiation
         * E.g. within initSession, a Transaction close is requested, 
         * which kicks in the RuleEngine, which instantiatates a 
         * Session with Session::singleton(). If this code is placed
         * in this constructor, the constructor is not yet finished
         * and singleton() creates another Session object
         */
    }
    
    private function initSession(){
        $this->sessionAtom = new Atom($this->id, Concept::getSessionConcept());
        $this->sessionResource = new Resource($this->id, Concept::getSessionConcept());
        
        // Create a new Ampersand session atom if not yet in SESSION table (i.e. new php session)
        if (!$this->sessionAtom->exists()){ 
            $this->sessionAtom->add();
        }else{
            $experationTimeStamp = time() - Config::get('sessionExpirationTime');
            $lastAccessTime = $this->sessionAtom->getLinks('lastAccess[SESSION*DateTime]'); // lastAccess is UNI, therefore we expect max one DateTime from getLinks()
            
            // strtotime() returns Unix timestamp of lastAccessTime (in UTC). time() does also. Those can be compared
            if(count($lastAccessTime) && strtotime(current($lastAccessTime)->tgt()->getLabel()) < $experationTimeStamp){
                $this->logger->debug("Session expired");
                $this->destroySession();
                
                if(Config::get('loginEnabled')) Logger::getUserLogger()->warning("Your session has expired, please login again");
                
                self::$_instance = new Session();
                self::$_instance->initSession();
                return;
            }
        }
        
        // Set lastAccess time
        $this->sessionAtom->link(date(DATE_ATOM), 'lastAccess[SESSION*DateTime]', false)->add(); 
        
        Transaction::getCurrentTransaction()->close(true);
        
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
        if(is_null(self::$_instance)){
            self::$_instance = new Session();
            self::$_instance->initSession();
        }
        return self::$_instance;
    }
    
    /**
     * Delete Ampersand session atom (transaction is not automatically closed), destroy php session
     * @return void
     */
    public function destroySession(){
        $this->sessionAtom->delete(); // Delete Ampersand representation of session
        
        session_regenerate_id(); // Create new php session identifier
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
                $this->logger->debug("Getting interface 'SessionRoles' for {$this->sessionResource}");
                $sessionRoleLabels = array_map(
                    function($role){
                        return $role->id;
                    }, $this->sessionResource->all('SessionRoles')->get()
                );
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
                $this->logger->debug("Getting interface 'SessionAccount' for {$this->sessionResource}");
                $sessionAccounts = $this->sessionResource->all('SessionAccount')->get();
                
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
                $this->logger->debug("Getting interface 'SessionVars' for {$this->sessionResource}");
                return $this->sessionResource->all('SessionVars')->get();
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
                    && $interface->crudR()
                    && (!$interface->crudC() or ($interface->crudU() or $interface->crudD()))
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
     * @param InterfaceObject $ifc
     * @return boolean
     */
    public function isAccessibleIfc($ifc){
        return in_array($ifc, $this->accessibleInterfaces, true);
    }
    
/**********************************************************************************************
 * 
 * Static functions
 * 
 *********************************************************************************************/
     
    public static function deleteExpiredSessions(){
        $experationTimeStamp = time() - Config::get('sessionExpirationTime');
        
        $links = Relation::getRelation('lastAccess[SESSION*DateTime]')->getAllLinks();
        foreach ($links as $link){
            if(strtotime($link->tgt()->getLabel()) < $experationTimeStamp){
                $link->src()->delete();
            }
        }
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
}
?>