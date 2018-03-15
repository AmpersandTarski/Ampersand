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
use Ampersand\Transaction;
use Ampersand\Misc\Config;
use Psr\Log\LoggerInterface;
use Ampersand\Core\Link;
use Ampersand\Core\Relation;

/**
 * Class of session objects
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Session
{
    
    /**
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * @var string $id session identifier
     */
    protected $id;
    
    /**
     * Reference to corresponding session object (Atom) in &-domain
     *
     * @var \Ampersand\Core\Atom $sessionAtom
     */
    protected $sessionAtom;
    
    /**
     * Reference to corresponding session object which can be used with interfaces
     *
     * @var \Ampersand\Interfacing\Resource $sessionResource
     */
    protected $sessionResource;
    
    /**
     * Constructor of Session class
     *
     * @param \Psr\Log\LoggerInterface $logger
     */
    public function __construct(LoggerInterface $logger)
    {
        $this->logger = $logger;
       
        $this->setId();
        $this->initSessionAtom();
    }

    /**
     * Get session identifier
     *
     * @return string
     */
    public function getId()
    {
        return $this->id;
    }

    private function setId()
    {
        $this->id = session_id();
        $this->logger->debug("Session id set to: {$this->id}");
    }

    public function reset()
    {
        $this->sessionAtom->delete(); // Delete Ampersand representation of session
        session_regenerate_id(); // Create new php session identifier
        $this->setId();
        $this->initSessionAtom();
    }
    
    protected function initSessionAtom()
    {
        $this->sessionAtom = Concept::makeSessionAtom($this->id);
        $this->sessionResource = Resource::makeResourceFromAtom($this->sessionAtom);
        
        // Create a new Ampersand session atom if not yet in SESSION table (i.e. new php session)
        if (!$this->sessionAtom->exists()) {
            $this->sessionAtom->add();

            // If login functionality is not enabled, add all defined roles as allowed roles
            // TODO: can be removed when meat-grinder populates this meta-relation by itself
            if (!Config::get('loginEnabled')) {
                foreach (Role::getAllRoles() as $role) {
                    $this->sessionAtom->link(Concept::makeRoleAtom($role->label), 'sessionAllowedRoles[SESSION*Role]')->add();
                }
            }

            // Activate all allowed roles by default
            foreach ($this->getSessionAllowedRoles() as $atom) {
                $this->toggleActiveRole($atom, true);
            }
        } else {
            $experationTimeStamp = time() - Config::get('sessionExpirationTime');
            $lastAccessTime = $this->sessionAtom->getLinks('lastAccess[SESSION*DateTime]'); // lastAccess is UNI, therefore we expect max one DateTime from getLinks()
            
            // strtotime() returns Unix timestamp of lastAccessTime (in UTC). time() does also. Those can be compared
            if (count($lastAccessTime) && strtotime(current($lastAccessTime)->tgt()->getLabel()) < $experationTimeStamp) {
                $this->logger->debug("Session expired");
                // if(Config::get('loginEnabled')) \Ampersand\Log\Logger::getUserLogger()->warning("Your session has expired, please login again");
                $this->reset();
                return;
            }
        }
        
        // Set lastAccess time
        $this->sessionAtom->link(date(DATE_ATOM), 'lastAccess[SESSION*DateTime]', false)->add();
        
        Transaction::getCurrentTransaction()->close(true);
    }

    /**
     * Get session object which can be used with interfaces
     *
     * @return \Ampersand\Interfacing\Resource
     */
    public function getSessionResource()
    {
        return $this->sessionResource;
    }

    /**
     * (De)activate a session role
     *
     * This function to (de)activate roles depends on the invariant as defined in SystemContext.adl
     * RULE sessionActiveRole |- sessionAllowedRole
     *
     * @param \Ampersand\Core\Atom $roleAtom
     * @param bool|null $setActive
     * @return \Ampersand\Core\Atom
     */
    public function toggleActiveRole(Atom $roleAtom, bool $setActive = null): Atom
    {
        // Check/prevent unexisting role atoms
        if (!$roleAtom->exists()) {
            throw new Exception("Role {$roleAtom} is not defined", 500);
        }

        $link = $this->sessionAtom->link($roleAtom, 'sessionActiveRoles[SESSION*Role]');
        switch ($setActive) {
            case true:
                $link->add();
                break;
            case false:
                $link->delete();
                break;
            case null:
                if ($link->exists()) {
                    $link->delete();
                } else {
                    $link->add();
                }
                break;
        }

        return $roleAtom;
    }
    
    /**
     * Get allowed roles for this session
     *
     * @return \Ampersand\Core\Atom[]
     */
    public function getSessionAllowedRoles()
    {
        return array_map(function (Link $link) {
            return $link->tgt();
        }, $this->sessionAtom->getLinks('sessionAllowedRoles[SESSION*Role]'));
    }

    /**
     * Get active roles for this session
     *
     * @return \Ampersand\Core\Atom[]
     */
    public function getSessionActiveRoles()
    {
        return array_map(function (Link $link) {
            return $link->tgt();
        }, $this->sessionAtom->getLinks('sessionActiveRoles[SESSION*Role]'));
    }
    
    /**
     * Get session account or false
     *
     * @return Atom|false returns Ampersand account atom when there is a session account or false otherwise
     */
    public function getSessionAccount()
    {
        $this->logger->debug("Getting sessionAccount");

        if (!Config::get('loginEnabled')) {
            $this->logger->debug("No session account, because login functionality is not enabled");
            return false;
        } else {
            $sessionAccounts = $this->sessionAtom->getLinks('sessionAccount[SESSION*Account]');
            
            // Relation sessionAccount is UNI
            if (empty($sessionAccounts)) {
                $this->logger->debug("No session account, because user is not logged in");
                return false;
            } else {
                $account = current($sessionAccounts);
                $this->logger->debug("Session account is: '{$account}'");
                return $account;
            }
        }
    }

    /**
     * Set session account and register login timestamps
     *
     * @param \Ampersand\Core\Atom $accountAtom
     * @return \Ampersand\Core\Atom
     */
    public function setSessionAccount(Atom $accountAtom): Atom
    {
        if (!$accountAtom->exists()) {
            throw new Exception("Account does not exist", 500);
        }

        $this->sessionAtom->link($accountAtom, 'sessionAccount[SESSION*Account]')->add();
        
        // Login timestamps
        $ts = date(DATE_ISO8601);
        $accountAtom->link($ts, 'accMostRecentLogin[Account*DateTime]')->add();
        $accountAtom->link($ts, 'accLoginTimestamps[Account*DateTime]')->add();

        return $accountAtom;
    }
    
    /**
     * Determine is there is a loggedin user (account)
     * @return boolean
     */
    public function sessionUserLoggedIn()
    {
        if (!Config::get('loginEnabled')) {
            return false;
        } elseif ($this->getSessionAccount() !== false) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Get session variables (from 'SessionVars' interface)
     * @return mixed|false session variables (if interface 'SessionVars' is defined in &-script) or false otherwise
     */
    public function getSessionVars()
    {
        if (InterfaceObject::interfaceExists('SessionVars')) {
            try {
                $this->logger->debug("Getting interface 'SessionVars' for {$this->sessionResource}");
                return $this->sessionResource->all('SessionVars')->get();
            } catch (Exception $e) {
                $this->logger->error("Error while getting SessionVars interface: " . $e->getMessage());
                return false;
            }
        } else {
            return false;
        }
    }
    
/**********************************************************************************************
 *
 * Static functions
 *
 *********************************************************************************************/
     
    public static function deleteExpiredSessions()
    {
        $experationTimeStamp = time() - Config::get('sessionExpirationTime');
        
        $links = Relation::getRelation('lastAccess[SESSION*DateTime]')->getAllLinks();
        foreach ($links as $link) {
            if (strtotime($link->tgt()->getLabel()) < $experationTimeStamp) {
                $link->src()->delete();
            }
        }
        Transaction::getCurrentTransaction()->close(true);
    }
}
