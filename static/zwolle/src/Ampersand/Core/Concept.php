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
use Ampersand\Storage\Transaction;

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
     * Dependency injection of storage implementation
     * There must at least be one storage implementation for every concept
     * @var \Ampersand\Storage\ConceptStorageInterface[]
     */
    protected $storages;
    
    /**
     *
     * @var \Ampersand\Storage\ConceptStorageInterface
     */
    protected $primaryStorage;
    
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
	 * Array with conjunctIds (both from signal and invariant rules) that are affected by creating or deleting an atom of this concept
	 * @var string[]
	 */
	private $affectedConjunctIds = array();
	
	/**
	 * Array with signal conjuncts that are affected by creating or deleting an atom of this concept
	 * @var Conjunct[]
	 */
	private $affectedSigConjuncts = array();
	
	/**
	 * Array with invariant conjuncts that are affected by creating or deleting an atom of this concept
	 * @var Conjunct[]
	 */
	private $affectedInvConjuncts = array();
	
	/**
	 * Array of concepts (name) that are specializations of this concept
	 * @var string[]
	 */
	private $specializations = array();
	
	/**
	 * Array of concepts (name) that are generalizations of this concept
	 * @var string[]
	 */
	private $generalizations = array();
    
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
     * used to prevent unnecessary checks if atom exists in storage
     */
    private $atomCache = array();
	
	/**
	 * Concept constructor
	 * Private function to prevent outside instantiation of concepts. Use Concept::getConcept($conceptName)
	 * 
	 * @param array $conceptDef
     * @param ConceptStorageInterface[] $storages
	 */
	private function __construct(array $conceptDef, array $storages){
	    $this->logger = Logger::getLogger('CORE');
	    
        $this->def = $conceptDef;
        
        if(empty($storages)) throw new Exception("No storage(s) provided for concept {$conceptDef['label']}", 500);
        $this->storages = $storages;
        $this->primaryStorage = current($this->storages); // For now, we just pick the first storage as primary storage
        
		$this->name = $conceptDef['id'];
        $this->label = $conceptDef['label'];
		$this->type = $conceptDef['type'];
		
		$this->affectedConjunctIds = (array)$conceptDef['affectedConjuncts'];
		foreach($this->affectedConjunctIds as $conjId){
		    $conj = Conjunct::getConjunct($conjId);
		    
		    if ($conj->isSigConj()) $this->affectedSigConjuncts[] = $conj;
		    if ($conj->isInvConj()) $this->affectedInvConjuncts[] = $conj;
		    // if (!$conj->isSigConj() && !$conj->isInvConj()) $this->logger->warning("Affected conjunct '{$conj->id}' (specified for concept '[{$this->name}]') is not part of an invariant or signal rule");
		}
		
		$this->specializations = (array)$conceptDef['specializations'];
		$this->generalizations = (array)$conceptDef['generalizations'];
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
	 * Check if this concept is a generalization of another given concept
	 * @param Concept $concept
	 * @return boolean
	 */
	public function hasSpecialization($concept){
		return in_array($concept->name, $this->specializations);
	}
	
	/**
	 * Check if this concept is a specialization of another given concept
	 * @param Concept $concept
	 * @return boolean
	 */
	public function hasGeneralization($concept){
		return in_array($concept->name, $this->generalizations);
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
	 * Checks if this concept is in same classification tree as the provided concept
	 * @param Concept $concept
	 * @return boolean
	 */
	public function inSameClassificationTree($concept){
	    if($this->name == $concept->name) return true;
	    if($this->hasSpecialization($concept)) return true;
	    if($this->hasGeneralization($concept)) return true;
	     
	    // else
	    return false;
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
	public function getAffectedSigConjuncts(){
	    return $this->affectedSigConjuncts;
	}
	
	/**
	 * Returns array with invariant conjuncts that are affected by creating or deleting an atom of this concept
	 * @return Conjunct[]
	 */
	public function getAffectedInvConjuncts(){
	    return $this->affectedInvConjuncts;
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
	 * Generate a new atom identifier for this concept
	 * @return string
	 */
	public function createNewAtomId(){
        static $prevTime = null;
        
        // TODO: remove this hack with _AI (autoincrement feature)
	    if(strpos($this->name, '_AI') !== false && $this->isInteger()){
	        $firstCol = current($this->mysqlConceptTable->getCols());
	        $query = "SELECT MAX(`$firstCol->name`) as `MAX` FROM `{$this->mysqlConceptTable->name}`";
	         
	        $result = array_column((array)$this->primaryStorage->Exe($query), 'MAX');
	
	        if(empty($result)) $atomId = 1;
	        else $atomId = $result[0] + 1;
	
	    }else{
            $now = explode(' ', microTime()); // yields ["microseconds", "seconds"] both in seconds, e.g. ["0.85629400", "1322761879"]
            $time = $now[1] . substr($now[0], 2,6); // we drop the leading "0." and trailing "00"  from the microseconds
            
            // Guarantee that time is increased
            if($time <= $prevTime) $time = ++$prevTime; 
            else $prevTime = $time;
            
            $atomId = $this->name . '_' . $time;
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
        }elseif($this->primaryStorage->atomExists($atom)){
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
        return $this->primaryStorage->getAllAtoms();
    }
    
    /**
     * Creating and adding a new atom to the storage 
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
                $this->logger->debug("Add atom {$atom} to storage");
                Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation.
                
                foreach($this->storages as $storage) $storage->addAtom($atom); // Add to storage
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
        if(empty($this->getGeneralizations())) throw new Exception("Cannot remove {$atom} from concept {$this}, because no generalizations exists", 500);
        
        // Check if atom exists
        if($atom->exists()){
            $this->logger->debug("Remove atom {$atom} from {$this} in storage");
            Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation.
            
            foreach($this->storages as $storage) $storage->removeAtom($atom); // Remove from concept in storage
            if(($key = array_search($atom->id, $this->atomCache)) !== false) unset($this->atomCache[$key]); // Delete from cache
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
            $this->logger->debug("Delete atom {$atom} from storage");
            Transaction::getCurrentTransaction()->addAffectedConcept($this); // Add concept to affected concepts. Needed for conjunct evaluation.
            
            foreach($this->storages as $storage) $storage->deleteAtom($atom); // Delete from storage
            if(($key = array_search($atom->id, $this->atomCache)) !== false) unset($this->atomCache[$key]); // Delete from cache
            
            // Delete all links where $atom is used as src or tgt
            Relation::deleteAllLinksWithAtom($atom);
        }else{
            $this->logger->debug("Cannot delete atom {$atom}, because it does not exists");
        }
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
        $storages = [Database::singleton()];
	
	    foreach ($allConceptDefs as $conceptDef) self::$allConcepts[$conceptDef['id']] = new Concept($conceptDef, $storages);
	}
}

?>