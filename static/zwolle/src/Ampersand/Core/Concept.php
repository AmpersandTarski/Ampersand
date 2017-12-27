<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Database\DatabaseTable;
use Ampersand\Database\DatabaseTableCol;
use Ampersand\Interfacing\Resource;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Interfacing\View;
use Ampersand\Log\Logger;
use Ampersand\Rule\Conjunct;
use Ampersand\Core\Atom;
use Ampersand\Config;
use Ampersand\Interfacing\Transaction;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Concept {
    /**
     * Contains all concept definitions
     * @var Concept[]
     */
    private static $allConcepts;
    
    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * Dependency injection of ConceptPlug implementation
     * There must at least be one plug for every concept
     * 
     * @var \Ampersand\Plugs\ConceptPlugInterface[]
     */
    protected $plugs;
    
    /**
     *
     * @var \Ampersand\Plugs\ConceptPlugInterface
     */
    protected $primaryPlug;
    
    /**
     * Definition from which Concept object is created
     * @var array
     */
    private $def;
    
    /**
     * Name (and unique escaped identifier) of concept
     * TODO: rename var to $id
     * @var string $name Escaped name of concept as defined in Ampersand script
     */
    public $name;
    
    /**
     * Unescaped name of concept
     * @var string $label Unescaped name of concept as defined in Ampersand script
     */
    public $label;
    
    /**
     * Specifies technical representation of atoms of this concept (e.g. OBJECT, ALPHANUMERIC, INTERGER, BOOLEAN, etc)
     * @var string
     */
    public $type;
    
    /**
     * List of conjuncts that are affected by creating or deleting an atom of this concept
     * 
     * @var \Ampersand\Rule\Conjunct[]
     */
    public $relatedConjuncts = [];
    
    /**
     * Array of concepts (name) that are specializations of this concept
     * @var string[]
     */
    private $specializations = array();
    
    /**
     * Array of concepts (name) that are direct specializations of this concept
     * @var string[]
     */
    private $directSpecs = array();
    
    /**
     * Array of concepts (name) that are generalizations of this concept
     * @var string[]
     */
    private $generalizations = array();
    
    /**
     * Array of concepts (name) that are direct generalizations of this concept
     * @var string[]
     */
    private $directGens = array();
    
    /**
     * Concept identifier of largest generalization for this concept
     * @var string
     */
    private $largestConceptId;
    
    /**
     * Array of interface identifiers that have this concept as src concept
     * @var string[]
     */
    public $interfaceIds = array();
    
    /**
     * Default view object for atoms of this concept
     * @var View|NULL
     */
    private $defaultView = null;

    /**
     * Contains information about mysql table and columns in which this concept is administrated
     * @var DatabaseTable
     */
    private $mysqlConceptTable;
    
    /**
     * @var string[] $atomCache array with atomids that exist in the concept
     * used to prevent unnecessary checks if atom exists in plug
     */
    private $atomCache = [];
    
    /**
     * Concept constructor
     * Private function to prevent outside instantiation of concepts. Use Concept::getConcept($conceptName)
     * @param array $conceptDef
     * @param ConceptPlugInterface[] $plugs
     */
    private function __construct(array $conceptDef, array $plugs){
        $this->logger = Logger::getLogger('CORE');
        
        $this->def = $conceptDef;
        
        if(empty($plugs)) throw new Exception("No plug(s) provided for concept {$conceptDef['label']}", 500);
        $this->plugs = $plugs;
        $this->primaryPlug = current($this->plugs); // For now, we just pick the first plug as primary plug
        
        $this->name = $conceptDef['id'];
        $this->label = $conceptDef['label'];
        $this->type = $conceptDef['type'];
        
        foreach((array)$conceptDef['affectedConjuncts'] as $conjId){
            $conj = Conjunct::getConjunct($conjId);
            $this->relatedConjuncts[] = $conj;
        }
        
        $this->specializations = (array)$conceptDef['specializations'];
        $this->generalizations = (array)$conceptDef['generalizations'];
        $this->directSpec = (array)$conceptDef['directSpecs'];
        $this->directGens = (array)$conceptDef['directGens'];
        $this->interfaceIds = (array)$conceptDef['interfaces'];
        $this->largestConceptId = $conceptDef['largestConcept'];
        
        if(!is_null($conceptDef['defaultViewId'])) $this->defaultView = View::getView($conceptDef['defaultViewId']);
        
        $this->mysqlConceptTable = new DatabaseTable($conceptDef['conceptTable']['name']);
        foreach ($conceptDef['conceptTable']['cols'] as $colName){
            $this->mysqlConceptTable->addCol(new DatabaseTableCol($colName));
        }
        
        // All atoms query is a hack which allows to manually add a more efficient query to get all atoms in Concepts.json
        // E.g. to include already some (default) view variables
        // TODO: replace hack by propert implementation
        if(isset($this->def['allAtomsQuery'])) $this->mysqlConceptTable->allAtomsQuery = $this->def['allAtomsQuery'];
        
    }
    
    /**
     * Function is called when object is treated as a string
     * @return string
     */
    public function __toString(){
        return $this->label;
    }
    
    /**
     * Specifies if concept representation is integer
     * @return boolean
     */
    public function isInteger(){
        return $this->type == "INTEGER";
    }
    
    /**
     * Specifies if concept is object
     * @return boolean
     */
    public function isObject(){
        return $this->type == "OBJECT";
    }
    
    /**
     * Check if concept is file object
     * @return boolean
     */
    public function isFileObject(){
        foreach ($this->getGeneralizationsIncl() as $concept) {
            if ($concept->label == 'FileObject') return true;
        }
        return false;
    }
    
    /**
     * Returns if concept is the ampersand SESSION concept
     * @return boolean
     */
    public function isSession(){
        foreach ($this->getGeneralizationsIncl() as $concept) {
            if ($concept->label == 'SESSION') return true;
        }
        return false;
    }
    
    /**
     * Check if this concept is a generalization of another given concept
     * @param Concept $concept
     * @param boolean $thisIncluded specifies if $this concept is included in comparison
     * @return boolean
     */
    public function hasSpecialization($concept, $thisIncluded = false){
        if($thisIncluded && $concept == $this) return true;
        
        return in_array($concept->name, $this->specializations);
    }
    
    /**
     * Check if this concept is a specialization of another given concept
     * @param Concept $concept
     * @param boolean $thisIncluded specifies if $this concept is included in comparison
     * @return boolean
     */
    public function hasGeneralization($concept, $thisIncluded = false){
        if($thisIncluded && $concept == $this) return true;
        
        return in_array($concept->name, $this->generalizations);
    }
    
    /**
     * Checks if this concept is in same classification tree as the provided concept
     * @param Concept $concept
     * @return boolean
     */
    public function inSameClassificationTree($concept){
        if($this->hasSpecialization($concept, true)) return true;
        if($this->hasGeneralization($concept, true)) return true;
         
        // else
        return false;
    }
    
    /**
     * Array of all concepts of which this concept is a generalization.
     * @return Concept[]
     */
    public function getSpecializations(){
        $specializations = array();
        foreach($this->specializations as $conceptName) $specializations[$conceptName] = self::getConcept($conceptName);
        return $specializations;
    }
    
    /**
     * Array of all concepts of which this concept is a specialization (exluding the concept itself).
     * @return Concept[]
     */
    public function getGeneralizations(){    
        $generalizations = array();
        foreach ($this->generalizations as $conceptName) $generalizations[$conceptName] = self::getConcept($conceptName);
        return $generalizations;
    }
    
    /**
     * Array of all concepts of which this concept is a generalization including the concept itself.
     * @return Concept[]
     */
    public function getSpecializationsIncl(){
        $specializations = $this->getSpecializations();
        $specializations[] = $this;
        return $specializations;
    }
    
    /**
     * Array of all concepts of which this concept is a specialization including the concept itself.
     * @return Concept[]
     */
    public function getGeneralizationsIncl(){
        $generalizations = $this->getGeneralizations();
        $generalizations[] = $this;
        return $generalizations;
    }
    
    /**
     * Returns largest generalization concept (can be itself)
     * @return Concept
     */
    public function getLargestConcept(){
        return Concept::getConcept($this->largestConceptId);
    }
    
    /**
     * Returns default view for this concept (or null if no default view defined)
     * @return View|NULL
     */
    public function getDefaultView(){
        return $this->defaultView;
    }
    
    /**
     * Returns array with signal conjuncts that are affected by creating or deleting an atom of this concept
     * @return Conjunct[]
     */
    public function getRelatedConjuncts(){
        return $this->relatedConjuncts;
    }
    
    /**
     * Returns database table info for concept
     * @throws Exception if no database table is defined
     * @return DatabaseTable
     */
    public function getConceptTableInfo(){
        return $this->mysqlConceptTable;
    }
    
    /**
     * 
     * @return InterfaceObject[]
     */
    public function getInterfaces(){
        $interfaces = array();
        foreach ($this->interfaceIds as $ifcId){
            $ifc = InterfaceObject::getInterface($ifcId);
            $interfaces[$ifc->id] = $ifc;
        }
        return $interfaces;
    }

    /**
     * Get registered plugs for this concept
     *
     * @return \Ampersand\Plugs\ConceptPlugInterface[]
     */
    public function getPlugs(){
        return $this->plugs;
    }

    /**
     * Clear atom cache
     * 
     * @return void
     */
    public function clearAtomCache(){
        $this->atomCache = [];
    }
    
    /**
     * Generate a new atom identifier for this concept
     * @return string
     */
    public function createNewAtomId(){
        static $prevTimeSeconds = null;
        static $prevTimeMicros  = null;

        // TODO: remove this hack with _AI (autoincrement feature)
        if(strpos($this->name, '_AI') !== false && $this->isInteger()){
            $firstCol = current($this->mysqlConceptTable->getCols());
            $query = "SELECT MAX(`$firstCol->name`) as `MAX` FROM `{$this->mysqlConceptTable->name}`";
             
            $result = array_column((array)$this->primaryPlug->Exe($query), 'MAX');
    
            if(empty($result)) $atomId = 1;
            else $atomId = $result[0] + 1;
    
        }else{
            list($timeMicros, $timeSeconds) = explode(' ', microTime());
            $timeMicros = substr($timeMicros, 2,6); // we drop the leading "0." and trailing "00"  from the microseconds
            
            // Guarantee that time is increased
            if ($timeSeconds < $prevTimeSeconds){
            	$timeSeconds = $prevTimeSeconds;
            	$timeMicros  = ++$prevTimeMicros;
            } elseif($timeSeconds == $prevTimeSeconds){
                if($timeMicros <= $prevTimeMicros) $timeMicros = ++$prevTimeMicros;
                else $prevTimeMicros = $timeMicros;
            } else{
            	$prevTimeSeconds = $timeSeconds;
            	$prevTimeMicros = $timeMicros;
            }
            
            $atomId = $this->name . '_' . sprintf('%d',$timeSeconds) . '_' . sprintf('%08d',$timeMicros);
        }
        return $atomId;
    }
    
    /**
     * Instantiate new Atom object in backend
     * NB! this does not result automatically in a database insert
     *
     * @return Atom
     */
    public function createNewAtom(){
        return new Atom($this->createNewAtomId(), $this);
    }
    
    /**
     * @param Atom $atom
     * @return boolean
     */
    public function atomExists(Atom $atom){
        if(in_array($atom->id, $this->atomCache, true)){ // strict mode to prevent 'Nesting level too deep' error
            return true;
        }elseif($atom->id === '_NEW'){
            return true; // Return true if id is '_NEW' (special case)
        }elseif($this->primaryPlug->atomExists($atom)){
            $this->atomCache[] = $atom->id; // Add to cache
            return true;
        }else{
            return false;
        }
    }
    
    /**
     * Return content of all atoms for this concept
     * TODO: refactor when resources (e.g. for update field in UI) can be requested with interface definition
     * @return Atom[]
     */
     public function getAllAtomObjects(){
        return $this->primaryPlug->getAllAtoms($this);
    }
    
    /**
     * Rename atom
     *
     * @param Atom $atom
     * @param string $newAtomId
     * @return void
     */
    public function renameAtom(Atom $atom, $newAtomId){
        if($atom->concept != $this) throw new Exception("Cannot rename atom '{$atom}', because it does not match concept '{$this}'", 500);

        // Rename atom in concept set
        Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation and transaction management
        foreach($this->plugs as $plug) $plug->renameAtom($atom, $newAtomId);

        // Rename atom in relation sets
        $newAtom = new Atom($newAtomId, $this);
        foreach (Relation::getAllRelations() as $relation){
            // Source
            if($this->inSameClassificationTree($relation->srcConcept)){
                // Delete and add links where atom is the source
                foreach($relation->getAllLinks($atom, null) as $link){
                    $relation->deleteLink($link); // Delete old link
                    $relation->addLink(new Link($relation, $newAtom, $link->tgt())); // Add new link
                }
            }
            
            // Target
            if($this->inSameClassificationTree($relation->tgtConcept)){
                // Delete and add links where atom is the source
                foreach($relation->getAllLinks($atom, null) as $link){
                    $relation->deleteLink($link); // Delete old link
                    $relation->addLink(new Link($relation, $newAtom, $link->tgt())); // Add new link
                }
            }
        }
        
        // Update cache
        if(($key = array_search($atom->id, $this->atomCache)) !== false) unset($this->atomCache[$key]); // Delete from cache
        $this->atomCache[] = $newAtomId; // Add to cache

    }
    
    /**
     * Creating and adding a new atom to the plug 
     * ór adding an existing atom to another concept set (making it a specialization)
     * @param Atom $atom
     * @return void
     */
    public function addAtom(Atom $atom){
        // Adding atom[A] to [A] ($this)
        if($atom->concept == $this){
            if($atom->exists()){
                $this->logger->debug("Atom {$atom} already exists in concept");
            }else{
                $this->logger->debug("Add atom {$atom} to plug");
                Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation and transaction management
                
                foreach($this->plugs as $plug) $plug->addAtom($atom); // Add to plug
                $this->atomCache[] = $atom->id; // Add to cache
            }
        // Adding atom[A] to another concept [B] ($this)
        }else{
            // Check if concept A and concept B are in the same classification tree
            if(!$this->inSameClassificationTree($atom->concept)) throw new Exception("Cannot add {$atom} to concept {$this}, because concepts are not in the same classification tree", 500);
            
            // Check if atom[A] exists. Otherwise it may not be added to concept B
            if(!$atom->exists()) throw new Exception("Cannot add {$atom} to concept {$this}, because atom does not exists", 500);
            
            $atom->concept = $this; // Change concept definition
            $this->addAtom($atom);
        }
    }
    
    /**
     * Remove an existing atom from a concept set (i.e. removing specialization)
     * @param Atom $atom
     * @return void
     */
    public function removeAtom(Atom $atom){
        if($atom->concept != $this) throw new Exception("Cannot remove {$atom} from concept {$this}, because concepts don't match", 500);
        
        // Check if concept is a specialization of another concept
        if(empty($this->directGens)) throw new Exception("Cannot remove {$atom} from concept {$this}, because no generalizations exists", 500);
        if(count($this->directGens) > 1) throw new Exception("Cannot remove {$atom} from concept {$this}, because multiple generalizations exists", 500);
        
        // Check if atom exists
        if($atom->exists()){
            $this->logger->debug("Remove atom {$atom} from {$this} in plug");
            Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation and transaction management
            
            foreach($this->plugs as $plug) $plug->removeAtom($atom); // Remove from concept in plug
            if(($key = array_search($atom->id, $this->atomCache)) !== false) unset($this->atomCache[$key]); // Delete from cache
            
            // Delete all links where $atom is used as src or tgt atom
            // from relations where $this concept (or any of its specializations) is used as src or tgt concept
            Relation::deleteAllSpecializationLinks($atom);
        }else{
            $this->logger->debug("Cannot remove atom {$atom} from {$this}, because atom does not exists");
        }
    }
    
    /**
     * @param Atom $atom
     * @return void
     */
    public function deleteAtom(Atom $atom){
        if($atom->exists()){
            $this->logger->debug("Delete atom {$atom} from plug");
            Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation and transaction management
            
            foreach($this->plugs as $plug) $plug->deleteAtom($atom); // Delete from plug
            if(($key = array_search($atom->id, $this->atomCache)) !== false) unset($this->atomCache[$key]); // Delete from cache
            
            // Delete all links where $atom is used as src or tgt atom
            Relation::deleteAllLinksWithAtom($atom);
        }else{
            $this->logger->debug("Cannot delete atom {$atom}, because it does not exists");
        }
    }

    /**
     * Function to merge two atoms
     * All link from/to the $rightAtom are merged into the $leftAtom
     * The $rightAtom itself is deleted afterwards
     *
     * @param Atom $leftAtom
     * @param Atom $rightAtom
     * @return void
     */
    static function mergeAtoms(Atom $leftAtom, Atom $rightAtom){        
        // Check that left and right atoms are in the same typology.
        if(!$leftAtom->concept->inSameClassificationTree($rightAtom->concept)) throw new Exception("Cannot merge '{$rightAtom}' into '{$leftAtom}', because they not in the same classification tree", 500);

        // Skip when left and right atoms are the same
        if($leftAtom->id === $rightAtom->id){
            Logger::getLogger('CORE')->warning("Cannot merge leftAtom and rightAtom, because they are both '{$leftAtom}'");
            return;
        }

        // Check if left and right atoms exist
        if(!$leftAtom->exists()) throw new Exception("Cannot merge '{$rightAtom}' into '{$leftAtom}', because '{$leftAtom}' does not exists", 500);
        if(!$rightAtom->exists()) throw new Exception("Cannot merge '{$rightAtom}' into '{$leftAtom}', because '{$rightAtom}' does not exists", 500);

        // Merge step 1: rename right atom by left atom
        $rightAtom->concept->renameAtom($rightAtom, $leftAtom->id);

        // Merge step 2: if right atom is more specific, make left atom also more specific
        if ($leftAtom->concept->hasSpecialization($rightAtom->concept)) $rightAtom->concept->addAtom($leftAtom);
        
    }
    
    /**********************************************************************************************
     * 
     * Static functions
     * 
     *********************************************************************************************/
    
    /**
     * Return concept object given a concept identifier
     * @param string $conceptId Escaped concept name
     * @throws Exception if concept is not defined
     * @return Concept
     */
    public static function getConcept($conceptId){
        if(!array_key_exists($conceptId, $concepts = self::getAllConcepts())) throw new Exception("Concept '{$conceptId}' is not defined", 500);
         
        return $concepts[$conceptId];
    }
    
    /**
     * Return concept object given a concept label
     * @param string $conceptLabel Unescaped concept name
     * @throws Exception if concept is not defined
     * @return Concept
     */
    public static function getConceptByLabel($conceptLabel){
        foreach(self::getAllConcepts() as $concept)
            if($concept->label == $conceptLabel) return $concept;
        
        throw new Exception("Concept '{$conceptLabel}' is not defined", 500);
    }
    
    public static function getSessionConcept(){
        return self::getConcept('SESSION');
    }
    
    /**
     * Returns array with all concept objects
     * @return Concept[]
     */
    public static function getAllConcepts(){
        if(!isset(self::$allConcepts)) self::setAllConcepts();
        
        return self::$allConcepts;
    }
    
    /**
     * Import all concept definitions from json file and create and save Concept objects
     * @return void
     */
    private static function setAllConcepts(){
        self::$allConcepts = array();
         
        // import json file
        $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'concepts.json');
        $allConceptDefs = (array)json_decode($file, true);
        $plugs = [Database::singleton()];
    
        foreach ($allConceptDefs as $conceptDef) self::$allConcepts[$conceptDef['id']] = new Concept($conceptDef, $plugs);
    }
}

?>