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
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Interfacing\View;
use Ampersand\Log\Logger;
use Ampersand\Rule\Conjunct;
use Ampersand\Core\Atom;
use Ampersand\Config;

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
     * Dependency injection of a database connection class
     * @var Database
     */
    private $database;
    
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
	 * Specifies if this concept is an object (true) or scalar (false)
	 * @var boolean
	 */
	public $isObject;
	
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
     * @var string[] $atomCache array with atomids that exist in the concept (within database transaction)
     * used to prevent unnecessary queries to check if atom is already in database
     */
    private $atomCache = array();
	
	/**
	 * Concept constructor
	 * Private function to prevent outside instantiation of concepts. Use Concept::getConcept($conceptName)
	 * 
	 * @param array $conceptDef
	 */
	private function __construct($conceptDef){
	    $this->database = Database::singleton();
	    $this->logger = Logger::getLogger('FW');
	    
        $this->def = $conceptDef;
        
		$this->name = $conceptDef['id'];
        $this->label = $conceptDef['label'];
		$this->type = $conceptDef['type'];
		$this->isObject = ($this->type == "OBJECT") ? true : false;
		
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
		
		if(!is_null($conceptDef['defaultViewId'])) $this->defaultView = View::getView($conceptDef['defaultViewId']);
		
		$this->mysqlConceptTable = new DatabaseTable($conceptDef['conceptTable']['name']);
		foreach ($conceptDef['conceptTable']['cols'] as $colName){
		    $this->mysqlConceptTable->addCol(new DatabaseTableCol($colName));
		}
		
	}
	
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
	 * Return content of all atoms for this concept
	 * @return mixed[]
	 */
	public function getAllAtomObjects(){
        // Query all atoms in table
        if(isset($this->def['allAtomsQuery'])) $query = $this->def['allAtomsQuery'];
        else{
            $firstCol = current($this->mysqlConceptTable->getCols()); // We can query an arbitrary concept col for checking the existence of an atom
	        $query = "SELECT DISTINCT `{$firstCol->name}` as `atomId` FROM `{$this->mysqlConceptTable->name}` WHERE `{$firstCol->name}` IS NOT NULL";
        }
        
        $arr = array();
	    foreach ((array)$this->database->Exe($query) as $row){
	        $tgtAtom = new Atom($row['atomId'], $this, null, $row);
	        $arr[] = $tgtAtom->getAtom();
	    }
	    return $arr;
	}
	
	/**
	 * Return array with all atom identifiers for this concept
	 * @return string[]
	 */
	public function getAllAtomIds(){
	    $firstCol = current($this->mysqlConceptTable->getCols()); // We can query an arbitrary concept col for checking the existence of an atom
	
	    // Query all atoms in table
	    $query = "SELECT DISTINCT `{$firstCol->name}` as `atom` FROM `{$this->mysqlConceptTable->name}` WHERE `{$firstCol->name}` IS NOT NULL";
	    return array_column((array)$this->database->Exe($query), 'atom'); // no need to filter duplicates and NULLs
	
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
	    if(strpos($this->name, '_AI') !== false && $this->isInteger()){
	        $firstCol = current($this->mysqlConceptTable->getCols());
	        $query = "SELECT MAX(`$firstCol->name`) as `MAX` FROM `{$this->mysqlConceptTable->name}`";
	         
	        $result = array_column((array)$this->database->Exe($query), 'MAX');
	
	        if(empty($result)) $atomId = 1;
	        else $atomId = $result[0] + 1;
	
	    }else{
	        $time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
	        $atomId = $this->name.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds
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
     * @param Atom $atom check if atom exists in concept atom cache
     * @return boolean
     */
    public function inAtomCache($atom){
        return in_array($atom->id, $this->atomCache, true); // strict mode to prevent 'Nesting level too deep' error
    }
    
    /**
     * @param Atom $atom atom to add to concept atom cache
     * @return void
     */
    public function addToAtomCache($atom){
        $this->atomCache[] = $atom->id;
    }
    
    /**
     * @param Atom $atom atom to remove from atom cache
     * @return void
     */
    public function removeFromAtomCache($atom){
        if(($key = array_search($atom->id, $this->atomCache)) !== false) {
            unset($this->atomCache[$key]);
        }
    }
	
    /**********************************************************************************************
     * 
     * Static functions
     * 
     *********************************************************************************************/
	
	/**
	 * Return concept object given a concept identifier
	 * @param string $conceptName Escaped concept name
	 * @throws Exception if concept is not defined
	 * @return Concept
	 */
	public static function getConcept($conceptName){
	    if(!array_key_exists($conceptName, $concepts = self::getAllConcepts())) throw new Exception("Concept '{$conceptName}' is not defined", 500);
	     
	    return $concepts[$conceptName];
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
	
	    foreach ($allConceptDefs as $conceptDef) self::$allConcepts[$conceptDef['id']] = new Concept($conceptDef);
	}
}

?>