<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Database\DatabaseTableCol;
use Ampersand\Database\RelationTable;
use Ampersand\Core\Concept;
use Ampersand\Rule\Conjunct;
use Ampersand\Log\Logger;
use Ampersand\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Relation {
    
    /**
     * Contains all relation definitions
     * @var Relation[]
     */
    private static $allRelations;
    
    /**
     * 
     * @var Database
     */
    private $db;
    
    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * 
     * @var string
     */
    public $signature;
    
    /**
     * 
     * @var string
     */
    public $name;
    
    /**
     * 
     * @var Concept
     */
    public $srcConcept;
    
    /**
     * 
     * @var Concept
     */
    public $tgtConcept;
    
    /**
     * @var boolean
     */
    public $isUni;
    
    /**
     * 
     * @var boolean
     */
    public $isTot;
    
    /**
     * 
     * @var boolean
     */
    public $isInj;
    
    /**
     * 
     * @var boolean
     */
    public $isSur;
    
    /**
     * 
     * @var boolean
     */
    public $isProp;
    
    /**
     * 
     * @var Conjunct[]
     */
    public $affectedConjuncts = array();
    
    /**
     * 
     * @var Conjunct[]
     */
    private $affectedSigConjuncts = array();
    
    /**
     * 
     * @var Conjunct[]
     */
    private $affectedInvConjuncts = array();
    
    /**
     * 
     * @var RelationTable
     */
    private $mysqlTable;
    
    /**
     * Relation constructor
     * Private function to prevent outside instantiation of Relations. Use Relation::getRelation($relationSignature)
     *
     * @param array $relationDef
     */
    public function __construct($relationDef){
        $this->db = Database::singleton();
        $this->logger = Logger::getLogger('FW');
        
        $this->name = $relationDef['name'];
        $this->srcConcept = Concept::getConcept($relationDef['srcConcept']);
        $this->tgtConcept = Concept::getConcept($relationDef['tgtConcept']);
        
        $this->signature = $relationDef['signature'];
        
        $this->isUni = $relationDef['uni'];
        $this->isTot = $relationDef['tot'];
        $this->isInj = $relationDef['inj'];
        $this->isSur = $relationDef['sur'];
        $this->isProp = $relationDef['prop'];
        
        foreach((array)$relationDef['affectedConjuncts'] as $conjId){
            $conj = Conjunct::getConjunct($conjId);
            
            $this->affectedConjuncts[] = $conj;
        
            if ($conj->isSigConj()) $this->affectedSigConjuncts[] = $conj;
            if ($conj->isInvConj()) $this->affectedInvConjuncts[] = $conj;
            if (!$conj->isSigConj() && !$conj->isInvConj()) $this->logger->warning("Affected conjunct '{$conj->id}' (specified for relation '{$this->__toString()}') is not part of an invariant or signal rule");
        }
        
        // Specify mysql table information
        $this->mysqlTable = new RelationTable($relationDef['mysqlTable']['name'], $relationDef['mysqlTable']['tableOf']);
        
        $srcCol = $relationDef['mysqlTable']['srcCol'];
        $tgtCol = $relationDef['mysqlTable']['tgtCol'];
        
        $this->mysqlTable->addSrcCol(new DatabaseTableCol($srcCol['name'], $srcCol['null'], $srcCol['unique']));
        $this->mysqlTable->addTgtCol(new DatabaseTableCol($tgtCol['name'], $tgtCol['null'], $tgtCol['unique']));
    }
    
    public function __toString(){
        return "{$this->name}[{$this->srcConcept->name}*{$this->tgtConcept->name}]";
    }
    
    /**
     * Returns array with signal conjuncts that are affected by updating this Relation
     * @return Conjunct[]
     */
    public function getAffectedSigConjuncts(){
        return $this->affectedSigConjuncts;
    }
    
    /**
     * Returns array with invariant conjuncts that are affected by by updating this Relation
     * @return Conjunct[]
     */
    public function getAffectedInvConjuncts(){
        return $this->affectedInvConjuncts;
    }
    
    /**
     * 
     * @return RelationTable
     */
    public function getMysqlTable(){
        return $this->mysqlTable;
    }
    
    /**
     * How to use Relation::addLink() to add link (a1,b1) into r:
	 * r :: A * B
	 * addLink(a1[A], b1[B], false);
	 * addLink(b1[B], a1[A], true);
	 * 
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @param boolean $isFlipped
     * @param string $source specifies who calls this function (e.g. 'User' or 'ExecEngine')
     * @return void
     */
    public function addLink($leftAtom, $rightAtom, $isFlipped = false, $source = 'User'){
        $this->logger->debug("Insert link ({$leftAtom->__toString()},{$rightAtom->__toString()}) into relation '{$this->__toString()}{($isFlipped ? '~' : '')}'");
         
        // Determine src and tgt atom based on $isFlipped
        $srcAtom = $isFlipped ? $rightAtom : $leftAtom;
        $tgtAtom = $isFlipped ? $leftAtom : $rightAtom;
        
        // Checks
        if($this->srcConcept != $srcAtom->concept && !in_array($srcAtom->concept, $this->srcConcept->getSpecializations())) throw new Exception ("Cannot insert link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) into relation '{$this->__toString()}', because source concept does not match relation source or its specializations", 500);
        if($this->tgtConcept != $tgtAtom->concept && !in_array($tgtAtom->concept, $this->tgtConcept->getSpecializations())) throw new Exception ("Cannot insert link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) into relation '{$this->__toString()}', because target concept does not match relation target or its specializations", 500);
        if(is_null($srcAtom->id)) throw new Exception ("Cannot insert link in relation '{$this->__toString()}', because src atom is not specified", 500);
        if(is_null($tgtAtom->id)) throw new Exception ("Cannot insert link in relation '{$this->__toString()}', because tgt atom is not specified", 500);
        
        // Ensure that atoms exists in their concept tables
        $srcAtom->addAtom();
        $tgtAtom->addAtom();
        
        // Insert link in relation table
        $this->db->addLink($this, $srcAtom, $tgtAtom);
    }
    
    /**
     * How to use Relation::deleteLink() to delete link (a1,b1) from r:
	 * r :: A * B
	 * deleteLink(a1[A], b1[B], false);
	 * deleteLink(b1[B], a1[A], true);
	 * 
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @param boolean $isFlipped
     * @param string $source specifies who calls this function (e.g. 'User' or 'ExecEngine')
     * @return void
     */
    public function deleteLink($leftAtom, $rightAtom, $isFlipped = false, $source = 'User'){
        $this->logger->debug("Delete link ({$leftAtom->__toString()},{$rightAtom->__toString()}) from relation '{$this->__toString()}{($isFlipped ? '~' : '')}'");
         
        // Determine src and tgt atom based on $isFlipped
        $srcAtom = $isFlipped ? $rightAtom : $leftAtom;
        $tgtAtom = $isFlipped ? $leftAtom : $rightAtom;
         
        // Checks
        if($this->srcConcept != $srcAtom->concept && !in_array($srcAtom->concept, $this->srcConcept->getSpecializations())) throw new Exception ("Cannot insert link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) into relation '{$this->__toString()}', because source concept does not match relation source or its specializations", 500);
        if($this->tgtConcept != $tgtAtom->concept && !in_array($tgtAtom->concept, $this->tgtConcept->getSpecializations())) throw new Exception ("Cannot insert link ({$srcAtom->__toString()},{$tgtAtom->__toString()}) into relation '{$this->__toString()}', because target concept does not match relation target or its specializations", 500);
        
        // Delete link from relation table
        $this->db->deleteLink($this, $srcAtom, $tgtAtom);
    }

    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return Relation object
     * @param string $relationSignature
     * @throws Exception if Relation is not defined
     * @return Relation
     */
    public static function getRelation($relationSignature, $srcConceptName = null, $tgtConceptName = null){
        $relations = self::getAllRelations();
        
        // If relation can be found by its fullRelationSignature return the relation
        if(array_key_exists($relationSignature, $relations)){
            $relation = $relations[$relationSignature];
            
            // If srcConceptName and tgtConceptName are provided, check that they match the found relation
            if(!is_null($srcConceptName) && $relation->srcConcept->name != $srcConceptName) throw new Exception("Provided src concept [{$srcConceptName}] does not match the found relation '{$relation->__toString()}'", 500);  
            if(!is_null($tgtConceptName) && $relation->tgtConcept->name != $tgtConceptName) throw new Exception("Provided tgt concept [{$tgtConceptName}] does not match the found relation '{$relation->__toString()}'", 500);
            
            return $relation;
        }
        
        // Else try to find the relation by its name, srcConcept and tgtConcept
        if(!is_null($srcConceptName) && !is_null($tgtConceptName)){
            foreach ($relations as $relation){
                if($relation->name == $relationSignature 
                        && $relation->srcConcept->name == $srcConceptName
                        && $relation->tgtConcept->name == $tgtConceptName) return $relation;
            }
        }
        
        // Else
        throw new Exception("Relation '{$relationSignature}' is not defined", 500);
    }
    
    /**
     * Returns array with all Relation objects
     * @return Relation[]
     */
    public static function getAllRelations(){
        if(!isset(self::$allRelations)) self::setAllRelations();
         
        return self::$allRelations;
    }
    
    /**
     * Import all Relation definitions from json file and create and save Relation objects
     * @return void
     */
    private static function setAllRelations(){
        self::$allRelations = array();
    
        // import json file
        $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'relations.json');
        $allRelationDefs = (array)json_decode($file, true);
    
        foreach ($allRelationDefs as $relationDef){
            $relation = new Relation($relationDef);
            self::$allRelations[$relation->signature] = $relation; 
        }
    }
}

?>