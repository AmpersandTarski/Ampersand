<?php

namespace Ampersand;

use Ampersand\Config;
use Ampersand\Import\JSONPopulationImporter;
use Ampersand\Interfacing\Transaction;
use Ampersand\Plugs\StorageInterface;
use Ampersand\Rule\Conjunct;
use Ampersand\Log\Logger;

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

    
    public function __construct(array $options = []){
        $this->logger = Logger::getLogger('APPLICATION');

        // Register storages
        if(isset($options['storages'])) foreach($options['storages'] as $storage) $this->registerStorage($storage);
    }

    public function registerStorage(StorageInterface $storage){
        $this->storages[] = $storage;
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
    }
}