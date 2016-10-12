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
        $this->logger = Logger::getLogger('FW');
        
        $this->name = $relationDef['name'];
        $this->srcConcept = Concept::getConcept($relationDef['srcConceptId']);
        $this->tgtConcept = Concept::getConcept($relationDef['tgtConceptId']);
        
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
            // if (!$conj->isSigConj() && !$conj->isInvConj()) $this->logger->warning("Affected conjunct '{$conj->id}' (specified for relation '{$this->__toString()}') is not part of an invariant or signal rule");
        }
        
        // Specify mysql table information
        $this->mysqlTable = new RelationTable($relationDef['mysqlTable']['name'], $relationDef['mysqlTable']['tableOf']);
        
        $srcCol = $relationDef['mysqlTable']['srcCol'];
        $tgtCol = $relationDef['mysqlTable']['tgtCol'];
        
        $this->mysqlTable->addSrcCol(new DatabaseTableCol($srcCol['name'], $srcCol['null'], $srcCol['unique']));
        $this->mysqlTable->addTgtCol(new DatabaseTableCol($tgtCol['name'], $tgtCol['null'], $tgtCol['unique']));
    }
    
    public function __toString(){
        return $this->getSignature();
    }
    
    /**
     * Return signature of relation (format: relName[srcConceptName*tgtConceptName])
     * @return string
     */
    public function getSignature(){
        return "{$this->name}[{$this->srcConcept}*{$this->tgtConcept}]";
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
     * Check if link (tuple of src and tgt atom) exists in this relation
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @param boolean $isFlipped specifies if $leftAtom and $rightAtom must be flipped to match the relation
     * @return boolean
     */
    public function linkExists(Atom $leftAtom, Atom $rightAtom, $isFlipped = false){
        // Determine src and tgt atom based on $isFlipped
        $srcAtom = $isFlipped ? $rightAtom : $leftAtom;
        $tgtAtom = $isFlipped ? $leftAtom : $rightAtom;
        
        return (new Link($this, $srcAtom, $tgtAtom))->exists();
    }
    
    /**
     * Add link to this relation
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @param boolean $isFlipped specifies if $leftAtom and $rightAtom must be flipped to match the relation
     * @return void
     */
    public function addLink(Atom $leftAtom, Atom $rightAtom, $isFlipped = false){
        // Determine src and tgt atom based on $isFlipped
        $srcAtom = $isFlipped ? $rightAtom : $leftAtom;
        $tgtAtom = $isFlipped ? $leftAtom : $rightAtom;
        
        (new Link($this, $srcAtom, $tgtAtom))->add();
    }
    
    /**
     * Delete link from this relation
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @param boolean $isFlipped specifies if $leftAtom and $rightAtom must be flipped to match the relation
     * @return void
     */
    public function deleteLink(Atom $leftAtom, Atom $rightAtom, $isFlipped = false){
        // Determine src and tgt atom based on $isFlipped
        $srcAtom = $isFlipped ? $rightAtom : $leftAtom;
        $tgtAtom = $isFlipped ? $leftAtom : $rightAtom;
        
        (new Link($this, $srcAtom, $tgtAtom))->delete();
    }
    
    /**
     * Returns all links (pair of Atoms) in this relation
     * @return Link[]
     */
    public function getAllLinks(){    
        $db = Database::singleton();
        return $db->getAllLinks($this);
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return Relation object
     * @param string $relationSignature
     * @param Concept $srcConcept
     * @param Concept $tgtConcept
     * @throws Exception if Relation is not defined
     * @return Relation
     */
    public static function getRelation($relationSignature, Concept $srcConcept = null, Concept $tgtConcept = null){
        $relations = self::getAllRelations();
        
        if(is_string($srcConcept)) $srcConcept = Concept::getConceptByLabel($srcConcept);
        if(is_string($tgtConcept)) $tgtConcept = Concept::getConceptByLabel($tgtConcept);
        
        // If relation can be found by its fullRelationSignature return the relation
        if(array_key_exists($relationSignature, $relations)){
            $relation = $relations[$relationSignature];
            
            // If srcConceptName and tgtConceptName are provided, check that they match the found relation
            if(!is_null($srcConcept) && !in_array($srcConcept, $relation->srcConcept->getSpecializationsIncl())) throw new Exception("Provided src concept '{$srcConcept}' does not match the relation '{$relation->__toString()}'", 500);  
            if(!is_null($tgtConcept) && !in_array($tgtConcept, $relation->tgtConcept->getSpecializationsIncl())) throw new Exception("Provided tgt concept '{$tgtConcept}' does not match the relation '{$relation->__toString()}'", 500);
            
            return $relation;
        }
        
        // Else try to find the relation by its name, srcConcept and tgtConcept
        if(!is_null($srcConcept) && !is_null($tgtConcept)){
            foreach ($relations as $relation){
                if($relation->name == $relationSignature 
                        && in_array($srcConcept, $relation->srcConcept->getSpecializationsIncl())
                        && in_array($tgtConcept, $relation->tgtConcept->getSpecializationsIncl())
                  ) return $relation;
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