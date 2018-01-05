<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use Ampersand\Database\DatabaseTableCol;
use Ampersand\Database\RelationTable;
use Ampersand\Core\Concept;
use Ampersand\Rule\Conjunct;
use Ampersand\Log\Logger;
use Ampersand\Misc\Config;
use Ampersand\Transaction;
use Ampersand\Plugs\RelationPlugInterface;

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
     * Dependency injection of plug implementation
     * There must at least be one plug for every relation
     * 
     * @var \Ampersand\Plug\RelationPlugInterface[]
     */
    protected $plugs = [];
    
    /**
     *
     * @var \Ampersand\Plug\RelationPlugInterface
     */
    protected $primaryPlug;
    
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
     * List of conjuncts that are affected by adding or removing a link in this relation
     * 
     * @var \Ampersand\Rule\Conjunct[]
     */
    public $relatedConjuncts = [];
    
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
    public function __construct($relationDef, RelationPlugInterface $defaultPlug = null){
        $this->logger = Logger::getLogger('CORE');

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
            $this->relatedConjuncts[] = $conj;
        }
        
        if(!is_null($defaultPlug)) $this->addPlug($defaultPlug);

        // Specify mysql table information
        $this->mysqlTable = new RelationTable($relationDef['mysqlTable']['name'], $relationDef['mysqlTable']['tableOf']);
        
        $srcCol = $relationDef['mysqlTable']['srcCol'];
        $tgtCol = $relationDef['mysqlTable']['tgtCol'];
        
        $this->mysqlTable->addSrcCol(new DatabaseTableCol($srcCol['name'], $srcCol['null'], $srcCol['unique']));
        $this->mysqlTable->addTgtCol(new DatabaseTableCol($tgtCol['name'], $tgtCol['null'], $tgtCol['unique']));
    }
    
    /**
     * Function is called when object is treated as a string
     * @return string
     */
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
    public function getRelatedConjuncts(){
        return $this->relatedConjuncts;
    }
    
    /**
     * 
     * @return RelationTable
     */
    public function getMysqlTable(){
        return $this->mysqlTable;
    }

    /**
     * Get registered plugs for this relation
     *
     * @return \Ampersand\Plugs\RelationPlugInterface[]
     */
    public function getPlugs(){
        if(empty($this->plugs)) throw new Exception("No plug(s) provided for relation {$this->getSignature()}", 500);
        return $this->plugs;
    }

    /**
     * Add plug for this relation
     *
     * @param \Ampersand\Plugs\RelationPlugInterface $plug
     * @return void
     */
    protected function addPlug(RelationPlugInterface $plug){
        if(!in_array($plug, $this->plugs)) $this->plugs[] = $plug;
        if(count($this->plugs) === 1) $this->primaryPlug = $plug;
    }
    
    /**
     * Check if link (tuple of src and tgt atom) exists in this relation
     * @param Link $link
     * @return boolean
     */
    public function linkExists(Link $link){
        $this->logger->debug("Checking if link {$link} exists in plug");
        
        return $this->primaryPlug->linkExists($link);
    }
    
    /**
    * Get all links for this relation
    * @param Atom $srcAtom if specified get all links with $srcAtom as source
    * @param Atom $tgtAtom if specified get all links with $tgtAtom as tgt
    * @return Link[]
    */
    public function getAllLinks(Atom $srcAtom = null, Atom $tgtAtom = null){
        return $this->primaryPlug->getAllLinks($this, $srcAtom, $tgtAtom);
    }
    
    /**
     * Add link to this relation
     * @param Link $link
     * @return void
     */
    public function addLink(Link $link){
        $this->logger->debug("Add link {$link} to plug");
        Transaction::getCurrentTransaction()->addAffectedRelations($this); // Add relation to affected relations. Needed for conjunct evaluation and transaction management
        
        // Ensure that atoms exists in their concept tables
        $link->src()->add(); // TODO: remove when we know for sure that this is guaranteed by calling functions
        $link->tgt()->add(); // TODO: remove when we know for sure that this is guaranteed by calling functions
        
        foreach($this->getPlugs() as $plug) $plug->addLink($link);
    }
    
    /**
     * Delete link from this relation
     * @param Link $link
     * @return void
     */
    public function deleteLink(Link $link){
        $this->logger->debug("Delete link {$link} from plug");
        Transaction::getCurrentTransaction()->addAffectedRelations($this); // Add relation to affected relations. Needed for conjunct evaluation and transaction management
        
        foreach($this->getPlugs() as $plug) $plug->deleteLink($link);
    }
    
    /**
     * @param Atom $atom atom for which to delete all links
     * @param string $srcOrTgt specifies to delete all link with $atom as src, tgt or both (null/not provided)
     * @return void
     */
    public function deleteAllLinks(Atom $atom, $srcOrTgt = null){
        Transaction::getCurrentTransaction()->addAffectedRelations($this); // Add relation to affected relations. Needed for conjunct evaluation and transaction management
        switch ($srcOrTgt) {
            case 'src':
                $this->logger->debug("Deleting all links in relation {$this} with {$atom} set as src");
                foreach($this->getPlugs() as $plug) $plug->deleteAllLinks($this, $atom, 'src');
                break;
            case 'tgt':
                $this->logger->debug("Deleting all links in relation {$this} with {$atom} set as tgt");
                foreach($this->getPlugs() as $plug) $plug->deleteAllLinks($this, $atom, 'tgt');
                break;
            case null:
                $this->logger->debug("Deleting all links in relation {$this} with {$atom} set as src or tgt");
                foreach($this->getPlugs() as $plug) $plug->deleteAllLinks($this, $atom, null);
                break;
            default:
                throw new Exception("Unknown/unsupported param option '{$srcOrTgt}'. Supported options are 'src', 'tgt' or null", 500);
                break;
        }
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    public static function deleteAllLinksWithAtom(Atom $atom){
        foreach (self::getAllRelations() as $relation){
            if($relation->srcConcept->inSameClassificationTree($atom->concept)) $relation->deleteAllLinks($atom, 'src');
            if($relation->tgtConcept->inSameClassificationTree($atom->concept)) $relation->deleteAllLinks($atom, 'tgt');
        }
    }
    
    public static function deleteAllSpecializationLinks(Atom $atom){
        foreach (self::getAllRelations() as $relation){
            if($relation->srcConcept->hasSpecialization($atom->concept)) $relation->deleteAllLinks($atom, 'src');
            if($relation->tgtConcept->hasSpecialization($atom->concept)) $relation->deleteAllLinks($atom, 'tgt');
        }
    }
    
    /**
     * Return Relation object
     * @param string $relationSignature
     * @param string|Concept $srcConcept
     * @param string|Concept $tgtConcept
     * @throws Exception if Relation is not defined
     * @return Relation
     */
    public static function getRelation($relationSignature, Concept $srcConcept = null, Concept $tgtConcept = null){
        $relations = self::getAllRelations();
        
        if(isset($srcConcept) && !($srcConcept instanceof Concept)) $srcConcept = Concept::getConceptByLabel($srcConcept);
        if(isset($srcConcept) && !($tgtConcept instanceof Concept)) $tgtConcept = Concept::getConceptByLabel($tgtConcept);
        
        // If relation can be found by its fullRelationSignature return the relation
        if(array_key_exists($relationSignature, $relations)){
            $relation = $relations[$relationSignature];
            
            // If srcConceptName and tgtConceptName are provided, check that they match the found relation
            if(!is_null($srcConcept) && !in_array($srcConcept, $relation->srcConcept->getSpecializationsIncl())) throw new Exception("Provided src concept '{$srcConcept}' does not match the relation '{$relation}'", 500);  
            if(!is_null($tgtConcept) && !in_array($tgtConcept, $relation->tgtConcept->getSpecializationsIncl())) throw new Exception("Provided tgt concept '{$tgtConcept}' does not match the relation '{$relation}'", 500);
            
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
        throw new Exception("Relation '{$relationSignature}[{$srcConcept}*{$tgtConcept}]' is not defined", 500);
    }
    
    /**
     * Returns array with all Relation objects
     * @return Relation[]
     */
    public static function getAllRelations(){
        if(!isset(self::$allRelations)) throw new Exception("Relation definitions not loaded yet", 500);
         
        return self::$allRelations;
    }

    /**
     * Register plug for specified relations
     *
     * @param RelationPlugInterface $plug
     * @param array $relationSignatures
     * @return void
     */
    public static function registerPlug(RelationPlugInterface $plug, array $relationSignatures = null){
        // Add plugs for all relations
        if (is_null($relationSignatures)) {
            foreach (self::getAllRelations() as $rel) {
                $rel->addPlug($plug);
            }
        }

        // Only for specific relations
        else {
            foreach ($relationSignatures as $rel) {
                (self::getRelation($rel))->addPlug($plug);
            }
        }
    }
    
    /**
     * Import all Relation definitions from json file and instantiate Relation objects
     * 
     * @param string $fileName containing the Ampersand relation definitions
     * @param \Ampersand\Plugs\RelationPlugInterface $defaultPlug
     * @return void
     */
    public static function setAllRelations(string $fileName, RelationPlugInterface $defaultPlug = null){
        self::$allRelations = [];
    
        // Import json file
        $allRelationDefs = (array)json_decode(file_get_contents($fileName), true);
    
        foreach ($allRelationDefs as $relationDef){
            $relation = new Relation($relationDef, $defaultPlug);
            self::$allRelations[$relation->signature] = $relation; 
        }
    }
}
