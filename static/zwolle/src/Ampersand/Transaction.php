<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Misc\Hook;
use Ampersand\Misc\Config;
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
class Transaction
{
    
    /**
     * Points to the current/active transaction
     *
     * @var \Ampersand\Transaction|null
     */
    private static $currentTransaction = null;

    /**
     * List of all transactions (open or closed)
     *
     * @var \Ampersand\Transaction[]
     */
    protected static $transactions = [];
    
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
    private function __construct()
    {
        $this->logger = Logger::getLogger('TRANSACTION');
        $this->id = rand();
        $this->logger->info("Opening transaction: {$this->id}");
    }

    /**
     * Get current or new transaction
     *
     * @return \Ampersand\Transaction
     */
    public static function getCurrentTransaction(): Transaction
    {
        if (is_null(self::$currentTransaction)) {
            self::$transactions[] = self::$currentTransaction = new Transaction();
        }
        return self::$currentTransaction;
    }

    /**
     * Return all transaction object
     *
     * @return \Ampersand\Transaction[]
     */
    public static function getTransactions(): array
    {
        return self::$transactions;
    }

    /**
     * Function is called when object is treated as a string
     *
     * @return string
     */
    public function __toString(): string
    {
        return 'Transaction ' . $this->id;
    }
    
    /**
     * Close transaction
     *
     * @return \Ampersand\Transaction $this
     */
    public function close(): Transaction
    {
        $this->logger->info("Request to close transaction: {$this->id}");
        
        if ($this->isClosed()) {
            throw new Exception("Cannot close transaction, because transaction is already closed", 500);
        }
        
        Hook::callHooks('preCloseTransaction', get_defined_vars());

        // Run ExecEngine
        ExecEngine::run();

        // (Re)evaluate affected conjuncts
        foreach ($this->getAffectedConjuncts() as $conj) {
            $conj->evaluate(false);
        }

        // Check invariant rules
        $violations = $this->getInvariantViolations();
        $this->invariantRulesHold = empty($violations) ? true : false;
        foreach ($violations as $violation) {
            Notifications::addInvariant($violation); // notify user of broken invariant rules
        }
        
        // Decide action (commit or rollback)
        if ($this->invariantRulesHold) {
            $this->logger->info("Commit transaction");
            $this->commit();
        } elseif (Config::get('ignoreInvariantViolations', 'transactions')) {
            $this->logger->warning("Commit transaction with invariant violations");
            $this->commit();
        } else {
            $this->logger->info("Rollback transaction, invariant rules do not hold");
            $this->rollback();
        }
        
        Hook::callHooks('postCloseTransaction', get_defined_vars());
        
        self::$currentTransaction = null; // unset currentTransaction
        return $this;
    }

    /**
     * Commit transaction
     *
     * @return void
     */
    protected function commit()
    {
        // Cache conjuncts
        foreach ($this->getAffectedConjuncts() as $conj) {
            $conj->saveCache();
        }

        // Commit transaction for each registered storage
        foreach ($this->storages as $storage) {
            $storage->commitTransaction($this);
        }

        $this->isCommitted = true;
    }

    /**
     * Rollback transaction
     *
     * @return void
     */
    protected function rollback()
    {
        // Rollback transaction for each registered storage
        foreach ($this->storages as $storage) {
            $storage->rollbackTransaction($this);
        }

        // Clear atom cache for affected concepts
        foreach ($this->affectedConcepts as $cpt) {
            $cpt->clearAtomCache();
        }

        $this->isCommitted = false;
    }
    
    /**
     * Add storage implementation to this transaction
     *
     * @param \Ampersand\Plugs\StorageInterface $storage
     * @return void
     */
    private function addAffectedStorage(StorageInterface $storage)
    {
        if (!in_array($storage, $this->storages)) {
            $this->logger->debug("Add storage: " . $storage->getLabel());
            $this->storages[] = $storage;
        }
    }
    
    public function getAffectedConcepts()
    {
        return $this->affectedConcepts;
    }
    
    public function getAffectedRelations()
    {
        return $this->affectedRelations;
    }
    
    /**
     * Mark a concept as affected within the open transaction
     * @param Concept $concept
     * @return void
     */
    public function addAffectedConcept(Concept $concept)
    {
        if (!in_array($concept, $this->affectedConcepts)) {
            $this->logger->debug("Mark concept '{$concept}' as affected concept");
            
            foreach ($concept->getPlugs() as $plug) {
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
    public function addAffectedRelations(Relation $relation)
    {
        if (!in_array($relation, $this->affectedRelations)) {
            $this->logger->debug("Mark relation '{$relation}' as affected relation");

            foreach ($relation->getPlugs() as $plug) {
                $this->addAffectedStorage($plug); // Register storage in this transaction
                $plug->startTransaction($this); // Start transaction for this storage
            }

            $this->affectedRelations[] = $relation;
        }
    }

    /**
     * Return list of affected conjuncts in this transaction
     *
     * @return \Ampersand\Rule\Conjunct[]
     */
    public function getAffectedConjuncts()
    {
        $affectedConjuncts = [];
        
        // Get conjuncts for affected concepts and relations
        foreach ($this->affectedConcepts as $concept) {
            $affectedConjuncts = array_merge($affectedConjuncts, $concept->getRelatedConjuncts());
        }
        foreach ($this->affectedRelations as $relation) {
            $affectedConjuncts = array_merge($affectedConjuncts, $relation->getRelatedConjuncts());
        }
        
        // Remove duplicates and return conjuncts
        return array_unique($affectedConjuncts);
    }

    /**
     * Get list of rules that are affected in this transaction
     * If set of rules is provided, function will return affected subset
     *
     * @param \Ampersand\Rule\Rule[]|null $rules
     * @return \Ampersand\Rule\Rule[]
     */
    public function getAffectedRules(array $rules = null): array
    {
        $affectedRuleNames = [];
        foreach ($this->getAffectedConjuncts() as $conjunct) {
            $affectedRuleNames = array_merge($affectedRuleNames, $conjunct->getRuleNames());
        }
        $affectedRuleNames = array_unique($affectedRuleNames);

        $affectedRules = array_map(function (string $ruleName): Rule {
            return Rule::getRule($ruleName);
        }, $affectedRuleNames);
        
        // Return unfiltered affected rules
        if (is_null($rules)) {
            return $affectedRules;
        
        // Filtered affected rules
        } else {
            return array_filter($affectedRules, function (Rule $rule) use ($rules) {
                return in_array($rule, $rules);
            });
        }
    }

    /**
     * Get violations of invariant rules that are affected in this transaction
     *
     * @return \Ampersand\Rule\Violation[]
     */
    public function getInvariantViolations(): array
    {
        $this->logger->info("Checking invariant rules");
        
        $affectedInvRules = $this->getAffectedRules(Rule::getAllInvRules());

        return RuleEngine::checkRules($affectedInvRules, false); // force evaluation, because conjunct violations are not (yet) saved in database
    }
    
    public function invariantRulesHold()
    {
        return $this->invariantRulesHold;
    }
    
    public function isCommitted()
    {
        return $this->isCommitted === true;
    }
    
    public function isRolledBack()
    {
        return $this->isCommitted === false;
    }
    
    public function isOpen()
    {
        return $this->isCommitted === null;
    }
    
    public function isClosed()
    {
        return $this->isCommitted !== null;
    }
}
