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
use Ampersand\Log\Logger;
use Ampersand\Transaction;
use Ampersand\Misc\Config;

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
    private $id;
    
    /**
     * @var Atom $sessionAtom reference to corresponding session object (Atom) in &-domain
     */
    public $sessionAtom;
    
    /**
     * @var Resource $sessionResource reference to corresponding session object which can be used with interfaces
     */
    public $sessionResource;
    
    /**
     * @var Atom|false $sessionAccount
     */
    private $sessionAccount;
    
    /**
     * Constructor of Session class
     * private to prevent any outside instantiation of this object
     */
    public function __construct(){
        $this->logger = Logger::getLogger('SESSION');
       
        $this->setId();
    }

    /**
     * Get session identifier
     *
     * @return string
     */
    public function getId(){
        return $this->id;
    }

    private function setId(){
        $this->id = session_id();
        $this->logger->debug("Session id set to: {$this->id}");
    }

    private function resetId(){
        session_regenerate_id(); // Create new php session identifier
        $this->setId();
    }

    public function reset(){
        $this->sessionAtom->delete(); // Delete Ampersand representation of session
        $this->resetId();
        $this->initSessionAtom();
    }
    
    public function initSessionAtom(){
        $this->sessionAtom = new Atom($this->id, Concept::getSessionConcept());
        $this->sessionResource = new Resource($this->id, Concept::getSessionConcept()->name);
        
        // Create a new Ampersand session atom if not yet in SESSION table (i.e. new php session)
        if (!$this->sessionAtom->exists()){ 
            $this->sessionAtom->add();
        }else{
            $experationTimeStamp = time() - Config::get('sessionExpirationTime');
            $lastAccessTime = $this->sessionAtom->getLinks('lastAccess[SESSION*DateTime]'); // lastAccess is UNI, therefore we expect max one DateTime from getLinks()
            
            // strtotime() returns Unix timestamp of lastAccessTime (in UTC). time() does also. Those can be compared
            if(count($lastAccessTime) && strtotime(current($lastAccessTime)->tgt()->getLabel()) < $experationTimeStamp){
                $this->logger->debug("Session expired");
                if(Config::get('loginEnabled')) Logger::getUserLogger()->warning("Your session has expired, please login again");
                $this->reset();
                return;
            }
        }
        
        // Set lastAccess time
        $this->sessionAtom->link(date(DATE_ATOM), 'lastAccess[SESSION*DateTime]', false)->add(); 
        
        Transaction::getCurrentTransaction()->close(true);
    }
    
    /**
     * Get session roles
     * 
     * @return string[]
     */
    public function getSessionRoleLabels(){
        return array_map(
            function($role){
                return $role->id;
            }, $this->sessionResource->all('SessionRoles')->get()
        );
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
        Transaction::getCurrentTransaction()->close(true);
    }
}
