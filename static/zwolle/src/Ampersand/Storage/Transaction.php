<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;

use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Log\Logger;
use Ampersand\Rule\RuleEngine;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Transaction {
    
    /**
     * Contains the current/active transaction
     * @param Transaction $currentTransaction
     */
    private static $_currentTransaction = null;
    
    /**
     * Specifies transaction number (random int)
     * @var int
     */
    private $id;
    
    /**
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * Contains all affected Concepts during a transaction
     * @var Concept[]
     */
    private $affectedConcepts = [];
    
    /**
     * Contains all affected relations during a transaction
     * Relations are specified with their 'fullRelationSignature' (i.e. 'rel_<relationName>_<srcConcept>_<tgtConcept>')
     * @var array
     */
    private $affectedRelations = [];
    
    /**
     * Specifies if invariant rules hold. Null if no transaction has occurred (yet)
     * @var boolean|NULL
     */
    private $invariantRulesHold = null;
    
    /**
     * Specifies requested transaction type (i.e. 'feedback' or 'promise')
     * @var string
     */
    private $requestType = 'feedback';
    
    /**
     * Specifies if the transaction is committed or rolled back
     * @var boolean
     */
    private $isCommitted = null;
    
    /**
     * List with storages that are registered in this transaction
     * @var StorageInterface[] $storages
     */
    private $storages = [];
    
    public function __construct(){
        $this->logger = Logger::getLogger('TRANSACTION');
        $this->id = rand();
        $this->logger->info("Opening transaction: {$this->id}");
    }
    
    /**
     * Close transaction
     * @param boolean $commit specifies to commit (true) or rollback (false) when all invariants hold
     * @return Transaction $this
     */
    public function close($commit = null){
        $this->logger->info("Request to close transaction: {$this->id}");
        
        Hooks::callHooks('preCloseTransaction', get_defined_vars());
        
        $this->logger->debug("Checking all affected conjuncts");
        
        // Check invariant rules (we only have to check the affected invariant rules)
        $affectedConjuncts = RuleEngine::getAffectedConjuncts($this->getAffectedConcepts, $this->getAffectedRelations, 'inv'); // Get affected invariant conjuncts
        $this->invariantRulesHold = RuleEngine::checkInvariantRules($affectedConjuncts, true);
        
        // Check all process rules that are relevant for the activate roles
        RuleEngine::checkProcessRules();
        
        // Determine if transaction should be committed or not when all invariant rules hold based on $requestType
        if(!isset($commit)) $commit = $this->requestType == 'promise';
        
        if($this->invariantRulesHold && $commit){
            $this->logger->debug("Commit transaction");
            foreach($this->storages as $storage) $storage->commitTransaction(); // Commit transaction for each registered storage
            $this->isCommitted = true;
            
        }elseif(Config::get('ignoreInvariantViolations', 'transactions') && $commit){
            $this->logger->warning("Commit transaction with invariant violations");
            foreach($this->storages as $storage) $storage->commitTransaction(); // Commit transaction for each registered storage
            $this->isCommitted = true;
            
        }elseif($this->invariantRulesHold){
            $this->logger->info("Rollback transaction, invariant rules do hold, but no commit requested");
            foreach($this->storages as $storage) $storage->rollbackTransaction(); // Rollback transaction for each registered storage
            $this->isCommitted = false;
            
        }else{
            $this->logger->info("Rollback transaction, invariant rules do not hold");
            foreach($this->storages as $storage) $storage->rollbackTransaction(); // Rollback transaction for each registered storage
            $this->isCommitted = false;
        }
        
        Hooks::callHooks('postCloseTransaction', get_defined_vars());
        
        return $this;
        
    }
    
    /**
     * Add storage implementation to this transaction
     * @param StorageInterface $storage
     * @return void
     */
    public function addStorage(StorageInterface $storge){
        if(!in_array($storage, $this->storages)){
            $this->logger->debug("Add storage: " . $storage->getLabel());
            $this->$storages[] = $storage;
        }
    }
    
    public function getRequestType(){
        return $this->requestType;
    }
    
    public function setRequestType($requestType){
        switch($requestType){
            case 'feedback':
            case 'promise':
                $this->requestType = $requestType;
                break;
            default : throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
        }
    }
    
    public function getAffectedConcepts(){
        return $this->affectedConcepts;	
    }
    
    public function getAffectedRelations(){
        return $this->affectedRelations;
    }
    
    /**
     * Mark a concept as affected within the open transaction
     * @param Concept $concept
     * @return void
     */
    public function addAffectedConcept(Concept $concept){
        if(!in_array($concept, $this->affectedConcepts)){
            $this->logger->debug("Mark concept '{$concept}' as affected concept");
            $this->affectedConcepts[] = $concept;
        }
    }
    
    /**
     * Mark a relation as affected within the open transaction
     * @param Relation $relation
     * @return void
     */
    public function addAffectedRelations(Relation $relation){
        if(!in_array($relation, $this->affectedRelations)){
            $this->logger->debug("Mark relation '{$relation}' as affected relation");
            $this->affectedRelations[] = $relation;
        }
    }
    
    public function invariantRulesHold(){
        return $this->invariantRulesHold;
    }
    
    public function isCommitted(){
        return $this->isCommitted === true;
    }
    
    public function isRolledBack(){
        return $this->isCommitted === false;
    }
    
    public function isOpen(){
        return $this->isComitted === null;
    }
    
    public function isClosed(){
        return $this->isCommitted !== null;
    }
    
/**********************************************************************************************
 * 
 * Static functions
 * 
 *********************************************************************************************/
    
    public static function getCurrentTransaction(){
        if(!isset(self::$_currentTransaction)) self::$_currentTransaction = new Transaction();
        return self::$_currentTransaction;
    }
    
    /**
     * Register storage implementation in current transaction
     * Used to close all open storage transactions lateron
     * @param StorageInterface $storage
     * @return Transaction
     */
    public static function registerStorageTransaction(StorageInterface $storage){
        $transaction = self::getCurrentTransaction();
        $transaction->addStorage($storage);
        return $transaction;
    }
}

?>