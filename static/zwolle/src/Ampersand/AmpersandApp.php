<?php

namespace Ampersand;

use Ampersand\Config;
use Ampersand\Import\JSONPopulationImporter;
use Ampersand\Interfacing\Transaction;
use Ampersand\Plugs\StorageInterface;
use Ampersand\Rule\Conjunct;
use Ampersand\Log\Logger;
use Ampersand\Session;

class AmpersandApp
{
    /**
     * Specifies the required version of the localsettings file that
     * @const float
     */
    const REQ_LOCALSETTINGS_VERSION = 1.5;

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
     * @var AmpersandApp $_instance needed for singleton() pattern of this class
     */
    private static $_instance = null;
    
    /**
     * 
     * @param array $depInj dependency injection for Ampersand application
     */
    private function __construct(array $depInj = []){
        $this->logger = Logger::getLogger('APPLICATION');

        // Register storages
        if(isset($depInj['storages'])) foreach($depInj['storages'] as $storage) $this->registerStorage($storage);

        // Initiate session
        $this->setSession();
    }

    /**
     * private method to prevent any copy of this object
     */
    private function __clone(){}
        
    /**
     * @param array $depInj dependency injection for Ampersand application
     * @return AmpersandApp
     */
    public static function singleton(array $depInj = []){
        if(is_null(self::$_instance)) self::$_instance = new AmpersandApp($depInj);
        return self::$_instance;
    }
    

    public function registerStorage(StorageInterface $storage){
        $this->logger->debug("Add storage: " . $storage->getLabel());
        $this->storages[] = $storage;
    }

    private function setSession(){
        $this->session = new Session();
        $this->session->initSessionAtom();
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
     * Function to reinstall the application. This includes database structure and load default population
     * 
     * @param boolean $installDefaultPop specifies whether or not to install the default population
     * @return void
     */
    public function reinstall($installDefaultPop = true){
        $this->logger->info("Start application reinstall");

        foreach($this->storages as $storage) $storage->reinstallStorage();

        if($installDefaultPop){
            $this->logger->info("Install default population");
            
            set_time_limit ((int) ini_get('max_execution_time')); // reset time limit counter to handle large amounts of default population queries.

            $importer = new JSONPopulationImporter();
            $importer->loadFile(Config::get('pathToGeneratedFiles') . 'populations.json');
            $importer->importPopulation();

            set_time_limit ((int) ini_get('max_execution_time')); // reset time limit counter to handle large amounts of default population queries.
        }else{
            $this->logger->info("Skip default population");
        }

        // Close transaction
        $transaction = Transaction::getCurrentTransaction()->close(true);
        $this->logger->info("End application reinstall");
        if($transaction->isCommitted()) Logger::getUserLogger()->notice("Application successfully reinstalled");

        // Initial conjunct evaluation
        $this->logger->info("Initial evaluation of all conjuncts after application reinstallation");
        Conjunct::evaluateConjuncts(null, true); // Evaluate, cache and store all conjuncts

        Session::singleton(); // Initiate new session
    }
}