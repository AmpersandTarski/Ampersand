<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Exception;
use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Log\Logger;
use Ampersand\Plugs\StorageInterface;
use Ampersand\Rule\RuleEngine;
use Ampersand\Rule\ExecEngine;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Rule;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Transaction {
    
    /**
     * Points to the current/active transaction
     * 
     * @var \Ampersand\Interfacing\Transaction
     */
    private static $_currentTransaction = null;
    
    /**
     * Transaction number (random int)
     * 
     * @var int
     */
    private $id;
    
    /**
     * Logger
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * Contains all affected Concepts during a transaction
     * 
     * @var \Ampersand\Core\Concept[]
     */
    private $affectedConcepts = [];
    
    /**
     * Contains all affected relations during a transaction
     * 
     * @var \Ampersand\Core\Relation[]
     */
    private $affectedRelations = [];
    
    /**
     * Flag that is set when session variable is changed
     * A session variable is a relation with SESSION as src or tgt concept
     * This flag is e.g. used to trigger a navigation bar refresh in the frontend application (e.g. after a user login)
     * 
     * @var bool
     */
    private $sessionVarAffected = false;
    
    /**
     * Specifies if invariant rules hold. Null if no transaction has occurred (yet)
     * 
     * @var bool|NULL
     */
    private $invariantRulesHold = null;
    
    /**
     * Specifies if the transaction is committed or rolled back
     * 
     * @var bool
     */
    private $isCommitted = null;
    
    /**
     * List with storages that are affected in this transaction
     * Used to commit/rollback all storages when this transaction is closed
     * 
     * @var \Ampersand\Plugs\StorageInterface[] $storages
     */
    private $storages = [];
    
    /**
     * Constructor
     * 
     */
    private function __construct(){
        $this->logger = Logger::getLogger('TRANSACTION');
        $this->id = rand();
        $this->logger->info("Opening transaction: {$this->id}");
    }

    /**
     * Get current or new transaction
     *
     * @return Transaction
     */
    public static function getCurrentTransaction(){
        if(!isset(self::$_currentTransaction)) self::$_currentTransaction = new Transaction();
        return self::$_currentTransaction;
    }

    /**
     * Function is called when object is treated as a string
     * 
     * @return string
     */
    public function __toString(): string{
        return 'Transaction ' . $this->id;
    }
    
    /**
     * Close transaction
     * 
     * @param boolean $commit specifies to commit (true) or rollback (false) when all invariants hold
     * @return Transaction $this
     */
    public function close($commit = true): Transaction{
        $this->logger->info("Request to close transaction: {$this->id}");
        
        if($this->isClosed()) throw new Exception("Cannot close transaction, because transaction is already closed", 500);
        
        Hooks::callHooks('preCloseTransaction', get_defined_vars());

        // Run ExecEngine
        ExecEngine::run();
        
        // Check invariant rules
        $violations = $this->getInvariantViolations();
        $this->invariantRulesHold = empty($violations) ? true : false;
        foreach($violations as $violation) Notifications::addInvariant($violation); // notify user of broken invariant rules
        
        // Decide action (commit or rollback)
        if($this->invariantRulesHold && $commit){
            $this->logger->info("Commit transaction");
            foreach($this->storages as $storage) $storage->commitTransaction($this); // Commit transaction for each registered storage
            $this->isCommitted = true;
            
        }elseif(Config::get('ignoreInvariantViolations', 'transactions') && $commit){
            $this->logger->warning("Commit transaction with invariant violations");
            foreach($this->storages as $storage) $storage->commitTransaction($this); // Commit transaction for each registered storage
            $this->isCommitted = true;
            
        }elseif($this->invariantRulesHold){
            $this->logger->info("Rollback transaction, invariant rules do hold, but no commit requested");
            foreach($this->storages as $storage) $storage->rollbackTransaction($this); // Rollback transaction for each registered storage
            $this->isCommitted = false;
            
        }else{
            $this->logger->info("Rollback transaction, invariant rules do not hold");
            foreach($this->storages as $storage) $storage->rollbackTransaction($this); // Rollback transaction for each registered storage
            $this->isCommitted = false;
        }
        
        Hooks::callHooks('postCloseTransaction', get_defined_vars());
        
        self::$_currentTransaction = null; // unset currentTransaction
        return $this;
    }
    
    /**
     * Add storage implementation to this transaction
     * 
     * @param \Ampersand\Plugs\StorageInterface $storage
     * @return void
     */
    private function addAffectedStorage(StorageInterface $storage){
        if(!in_array($storage, $this->storages)){
            $this->logger->debug("Add storage: " . $storage->getLabel());
            $this->storages[] = $storage;
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
            
            foreach($concept->getPlugs() as $plug){
                $this->addAffectedStorage($plug); // Register storage in this transaction
                $plug->startTransaction($this); // Start transaction for this storage
            }

            $this->affectedConcepts[] = $concept;
        }
    }
    
    /**
     * Mark a relation as affected within the open transaction
     * @param Relation $relation
     * @return void
     */
    public function addAffectedRelations(Relation $relation){
        static $skipRels = ['lastAccess[SESSION*DateTime]']; // these relations do not result in a session refresh advice
        
        if(!in_array($relation, $this->affectedRelations)){
            $this->logger->debug("Mark relation '{$relation}' as affected relation");

            foreach($relation->getPlugs() as $plug){
                $this->addAffectedStorage($plug); // Register storage in this transaction
                $plug->startTransaction($this); // Start transaction for this storage
            }

            $this->affectedRelations[] = $relation;
        }
        
        // Flag session var as affected when src or tgt concept of this relation is SESSION
        if(($relation->srcConcept->isSession() || $relation->tgtConcept->isSession())
            && !in_array($relation->getSignature(), $skipRels)) $this->sessionVarAffected = true;
    }

    /**
     * Return list of affected conjuncts in this transaction
     * 
     * @return \Ampersand\Rule\Conjunct[]
     */
    public function getAffectedConjuncts(){
        $affectedConjuncts = [];
        
        // Get conjuncts for affected concepts and relations
        foreach($this->affectedConcepts as $concept) $affectedConjuncts = array_merge($affectedConjuncts, $concept->getRelatedConjuncts());
        foreach($this->affectedRelations as $relation) $affectedConjuncts = array_merge($affectedConjuncts, $relation->getRelatedConjuncts());
        
        // Remove duplicates and return conjuncts
        return array_unique($affectedConjuncts);
    }

    /**
     * Get list of rules that are affected in this transaction
     * If set of rules is provided, function will return affected subset
     *
     * @param \Ampersand\Rule\Rule[] $rules
     * @return \Ampersand\Rule\Rule[]
     */
    public function getAffectedRules(array $rules = null): array {
        $ruleNames = [];
        foreach($this->getAffectedConjuncts() as $conjunct) $ruleNames = array_merge($ruleNames, $conjunct->getRuleNames());
        $ruleNames = array_unique($ruleNames);
        
        // Return all affected rules
        if(is_null($rules)){
            return array_map(function($ruleName){
                return Rule::getRule($ruleName);
            }, $ruleNames);
        }

        // Return filtered affected rules
        else{ 
            return array_filter($rules, function($rule) use ($ruleNames){
                return in_array($rule->id, $ruleNames);
            });
        }
    }

    /**
     * Get violations of invariant rules that are affected in this transaction
     * 
     * @return \Ampersand\Rule\Violation[]
     */
    public function getInvariantViolations(): array {
        $this->logger->info("Checking invariant rules");
        
        $affectedInvRules = $this->getAffectedRules(Rule::getAllInvRules());

        return RuleEngine::checkRules($affectedInvRules, true);
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
        return $this->isCommitted === null;
    }
    
    public function isClosed(){
        return $this->isCommitted !== null;
    }
    
    /**
     * Returns if session refresh is adviced in frontend
     * True when session variable is affected AND transaction is committed
     * False otherwise
     * @return boolean
     */
    public function getSessionRefreshAdvice(){
        if($this->isOpen()) throw new Exception("Cannot determine session refresh advice, because transaction is not closed (yet).", 500);
        
        return $this->sessionVarAffected && $this->isCommitted();
    }
    
/**********************************************************************************************
 * 
 * Static functions
 * 
 *********************************************************************************************/
}

?>