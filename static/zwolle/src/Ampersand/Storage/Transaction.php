<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Rule\RuleEngine;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Transaction {
    
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
    private $affectedConcepts = array();
    
    /**
     * Contains all affected relations during a transaction
     * Relations are specified with their 'fullRelationSignature' (i.e. 'rel_<relationName>_<srcConcept>_<tgtConcept>')
     * @var array
     */
    private $affectedRelations = array(); // 
    
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
    
    
    public function __construct(){
        $this->logger = Logger::getLogger('TRANSACTION');
        $this->id = rand();
    }
    
    public function close(){
        $this->logger->info("Checking all affected conjuncts");
        
        // Check invariant rules (we only have to check the affected invariant rules)
        $affectedConjuncts = RuleEngine::getAffectedConjuncts($this->getAffectedConcepts, $this->getAffectedRelations, 'inv'); // Get affected invariant conjuncts
        $this->invariantRulesHold = RuleEngine::checkInvariantRules($affectedConjuncts, true);
        
        // Check all process rules that are relevant for the activate roles
        RuleEngine::checkProcessRules();
        
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
}