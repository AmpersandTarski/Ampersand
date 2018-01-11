<?php

namespace Ampersand;

use Ampersand\Misc\Config;
use Ampersand\IO\Importer;
use Ampersand\Transaction;
use Ampersand\Plugs\StorageInterface;
use Ampersand\Rule\Conjunct;
use Ampersand\Log\Logger;
use Ampersand\Session;
use Ampersand\Core\Atom;
use Exception;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Concept;
use Ampersand\Role;
use Ampersand\Rule\RuleEngine;
use Ampersand\Log\Notifications;
use Ampersand\IO\JSONReader;
use Ampersand\Interfacing\View;
use Ampersand\Rule\Rule;
use Ampersand\Core\Relation;

class AmpersandApp
{
    /**
     * Specifies the required version of the localsettings file that
     * @const float
     */
    const REQ_LOCALSETTINGS_VERSION = 1.6;

    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;

    /**
     * List with storages that are registered for this application
     * @var \Ampersand\Plugs\StorageInterface[] $storages
     */
    protected $storages = [];

    /**
     * The session between AmpersandApp and user
     * 
     * @var Session
     */
    protected $session = null;

    /**
     * List of accessible interfaces for the user of this Ampersand application
     * 
     * @var \Ampersand\Interfacing\InterfaceObject[] $accessibleInterfaces
     */
    protected $accessibleInterfaces = [];
    
    /**
     * List with rules that are maintained by the activated roles in this Ampersand application
     * 
     * @var \Ampersand\Rule\Rule[] $rulesToMaintain
     */
    protected $rulesToMaintain = []; // rules that are maintained by active roles

    /**
     * List of allowed roles for the user of this Ampersand application
     *
     * @var \Ampersand\Role[]
     */
    protected $allowedRoles = [];

    /**
     * @var AmpersandApp $_instance needed for singleton() pattern of this class
     */
    private static $_instance = null;
    
    /**
     * Constructor
     * 
     * @param \Ampersand\Plugs\StorageInterface $defaultPlug
     */
    public function __construct(StorageInterface $defaultPlug){
        $this->logger = Logger::getLogger('APPLICATION');

        $genericsFolder = Config::get('pathToGeneratedFiles');

        // Instantiate object definitions from generated files
        Conjunct::setAllConjuncts($genericsFolder . 'conjuncts.json');
        View::setAllViews($genericsFolder . 'views.json', $defaultPlug);
        Concept::setAllConcepts($genericsFolder . 'concepts.json', $defaultPlug);
        Relation::setAllRelations($genericsFolder . 'relations.json', $defaultPlug);
        InterfaceObject::setAllInterfaces($genericsFolder . 'interfaces.json', $defaultPlug);
        Rule::setAllRules($genericsFolder . 'rules.json', $defaultPlug);
        Role::setAllRoles($genericsFolder . 'roles.json');

        // Register storages
        $this->registerStorage($defaultPlug);

        // Initiate session
        $this->setSession();

        // Add public interfaces
        $this->accessibleInterfaces = InterfaceObject::getPublicInterfaces();
    }
    
    public function registerStorage(StorageInterface $storage){
        $this->logger->debug("Add storage: " . $storage->getLabel());
        $this->storages[] = $storage;
    }

    protected function setSession(){
        $this->session = new Session();
        $this->session->initSessionAtom();

        if(Config::get('loginEnabled')){
            $sessionRoleLabels = $this->session->getSessionRoleLabels();

            $sessionRoles = [];
            foreach(Role::getAllRoles() as $role){
                if(in_array($role->label, $sessionRoleLabels)) $sessionRoles[] = $role;
            }
            $this->allowedRoles = $sessionRoles;
        } 
        else $this->allowedRoles = Role::getAllRoles();
    }

    /**
     * Get the session object for this instance of the ampersand application
     *
     * @return Session
     */
    public function getSession(){
        return $this->session;
    }

    /**
     * Get list of accessible interfaces for the user of this Ampersand application
     *
     * @return \Ampersand\Interfacing\InterfaceObject[]
     */
    public function getAccessibleInterfaces(){
        return $this->accessibleInterfaces;
    }

    /**
     * Get the rules that are maintained by the active roles of this Ampersand application
     *
     * @return \Ampersand\Rule\Rule[]
     */
    public function getRulesToMaintain(){
        return $this->rulesToMaintain;
    }

    /**
     * Login user and commit transaction
     *
     * @return void
     */
    public function login(Atom $account){
        // Set sessionAccount
        $this->session->sessionAtom->link($account, 'sessionAccount[SESSION*Account]')->add();
        
        // Login timestamps
        $ts = date(DATE_ISO8601);
        $account->link($ts, 'accMostRecentLogin[Account*DateTime]')->add();
        $account->link($ts, 'accLoginTimestamps[Account*DateTime]')->add();

        Transaction::getCurrentTransaction()->close(true);

        $this->setSession();
    }

    /**
     * Logout user, destroy and reset session
     *
     * @return void
     */
    public function logout(){
        $this->session->reset();
    }

    /**
     * Function to reinstall the application. This includes database structure and load default population
     * 
     * @param boolean $installDefaultPop specifies whether or not to install the default population
     * @return \Ampersand\Transaction in which application is reinstalled
     */
    public function reinstall($installDefaultPop = true): Transaction {
        $this->logger->info("Start application reinstall");

        foreach($this->storages as $storage) $storage->reinstallStorage();

        // Clear atom cache
        foreach(Concept::getAllConcepts() as $cpt) $cpt->clearAtomCache();

        if($installDefaultPop){
            $this->logger->info("Install default population");

            $reader = new JSONReader();
            $reader->loadFile(Config::get('pathToGeneratedFiles') . 'populations.json');
            $importer = new Importer($reader, Logger::getLogger('IO'));
            $importer->importPopulation();
        }else{
            $this->logger->info("Skip default population");
        }

        // Close transaction
        $transaction = Transaction::getCurrentTransaction()->close(true);
        $this->logger->info("End application reinstall");

        // Initial conjunct evaluation
        $this->logger->info("Initial evaluation of all conjuncts after application reinstallation");
        foreach(Conjunct::getAllConjuncts() as $conjunct) $conjunct->evaluateConjunct(true); // Evaluate, cache and store all conjuncts

        $this->setSession(); // Initiate session again

        return $transaction;
    }

    /**
     * Activatie provided roles (if allowed)
     * @param array $roleIds list of role ids that must be activated
     * @return void
     */
    public function activateRoles($roleIds = null){
        if(empty($this->allowedRoles)){
            $this->logger->debug("No roles available to activate");    
        }elseif(is_null($roleIds)){
            $this->logger->debug("Activate default roles");
            foreach($this->allowedRoles as &$role) $this->activateRole($role);
        }elseif(empty($roleIds)){
            $this->logger->debug("No roles provided to activate");
        }else{
            if(!is_array($roleIds)) throw new Exception ('$roleIds must be an array', 500);
            foreach($this->allowedRoles as &$role){
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
     * @param \Ampersand\Role $role
     * @return void
     */
    protected function activateRole(Role &$role){
        $role->active = true;
        $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
        $this->rulesToMaintain = array_merge($this->rulesToMaintain, $role->maintains());
        
        $this->logger->info("Role '{$role->id}' is activated");
    }

    /**
     * Get allowed roles
     * 
     * @return \Ampersand\Role[]
     */
    public function getAllowedRoles(){
        return $this->allowedRoles;
    }

    /**
     * Get active roles
     * @return \Ampersand\Role[]
     */
    public function getActiveRoles(){
        $activeRoles = [];
        foreach ($this->allowedRoles as $role){
            if($role->active) $activeRoles[] = $role; 
        }
        return $activeRoles;
    }

    public function hasRole(array $roles = null){
        return (!empty(array_intersect($this->getAllowedRoles(), (array)$roles)) || is_null($roles));
    }

    public function hasActiveRole(array $roles = null){
        return (!empty(array_intersect($this->getActiveRoles(), (array)$roles)) || is_null($roles));
    }

    /**
     * Get interfaces that are accessible in the current session to 'Read' a certain concept
     * @param \Ampersand\Core\Concept[] $concepts
     * @return \Ampersand\Interfacing\InterfaceObject[]
     */
    public function getInterfacesToReadConcepts($concepts){
        return array_values(
            array_filter($this->accessibleInterfaces, function($ifc) use ($concepts) {
                foreach($concepts as $cpt){
                    if($ifc->srcConcept->hasSpecialization($cpt, true)
                        && $ifc->crudR()
                        && (!$ifc->crudC() or ($ifc->crudU() or $ifc->crudD()))
                        ) return true;
                }
                return false;
            })
        );
    }

    /**
     * Determine if provided concept is editable concept in one of the accessible interfaces in the current session
     * @param \Ampersand\Core\Concept $concept
     * @return boolean
     */
    public function isEditableConcept(Concept $concept){
        return array_reduce($this->accessibleInterfaces, function($carry, $ifc) use ($concept){
            return ($carry || in_array($concept, $ifc->getEditableConcepts()));
        }, false);
    }
    
    /**
     * Determine if provided interface is accessible in the current session
     * @param \Ampersand\Interfacing\InterfaceObject $ifc
     * @return boolean
     */
    public function isAccessibleIfc(InterfaceObject $ifc){
        return in_array($ifc, $this->accessibleInterfaces, true);
    }

    /**
     * Evaluate and signal violations for all rules that are maintained by the activated roles
     * 
     * @return void
     */
    public function checkProcessRules(){
        $this->logger->debug("Checking process rules for active roles: " . implode(', ', array_column($this->getActiveRoles(), 'label')));
        
        // Check rules and signal notifications for all violations
        foreach (RuleEngine::checkRules($this->getRulesToMaintain(), false) as $violation) Notifications::addSignal($violation);
    }
}