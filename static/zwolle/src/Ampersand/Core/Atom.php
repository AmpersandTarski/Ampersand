<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use DateTime;
use DateTimeZone;
use JsonSerializable;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Atom implements JsonSerializable {
    /**
     * Dependency injection of a database connection class
     * @var Database
     */
	protected $database;
	
	/**
	 *
	 * @var \Psr\Log\LoggerInterface
	 */
	protected $logger;
	
	/**
	 * Ampersand identifier of the atom
	 * @var string
	 */
	public $id;
	
	/**
	 * Escaped identifier for use in database queries
	 * @var string 
	 */
	public $idEsc;
	
	/**
	 * Specifies the concept of which this atom is an instance
	 * @var Concept
	 */
	public $concept;
    
    /**
     * @var array|null $queryData the row data (from database query) from which this resource is created
     */
    protected $queryData = null;
    
	/**
	 * Atom constructor
	 * @param string $atomId
	 * @param Concept $concept
	 * @return void
	 */
	public function __construct($atomId, Concept $concept){
		$this->database = Database::singleton();
		$this->logger = Logger::getLogger('ATOM');
		
        $this->concept = $concept;
		
        $this->id = $atomId;
        $this->idEsc = $this->database->getDBRepresentation($this); // Escape and transform id for (mysql) database queries
	}
	
	public function __toString(){
        // if atom id is longer than 40 chars, display first and last 20 chars
        $id = strlen($this->id) > 40 ? substr($this->id, 0, 20) . '...' . substr($this->id, -20) : $this->id;
	    return "{$id}[{$this->concept}]";
	}
    
    /**
     * Return label of atom to be displayed in user interfaces
     * for Atoms this is the same as the Atom identifier
     * @return string
     */
    public function getLabel(){
        return $this->id;
    }
    
    /**
     * Return json representation of Atom (identifier) according to Ampersand technical types (TTypes)
     * @throws Exception when technical type is not (yet) supported
     * @return mixed
     */
    public function jsonSerialize(){
        switch($this->concept->type){
            case "ALPHANUMERIC" :
            case "BIGALPHANUMERIC" :
            case "HUGEALPHANUMERIC" :
            case "PASSWORD" :
            case "TYPEOFONE" :
                return (string) $this->id;
            case "BOOLEAN" :
                return (bool) $this->id;
            case "DATE" :
                $datetime = new DateTime($this->id);
                return $datetime->format('Y-m-d'); // format in ISO-8601 standard
            case "DATETIME" :
                $datetime = new DateTime($this->id, new DateTimeZone('UTC')); // datetimes are stored in UTC in database
                $datetime->setTimezone(new DateTimeZone(date_default_timezone_get())); // convert back to systemtime
                return $datetime->format(DateTime::ATOM); // format in ISO-8601 standard, i.e. 2005-08-15T15:52:01+00:00 (DateTime::ATOM)
            case "FLOAT" :
                return (float) $this->id;
            case "INTEGER" :
                return (int) $this->id;
            case "OBJECT" :
                return rawurlencode($this->id);
            default :
                throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept}]'", 501);
        }
    }
    
	/**
	 * Checks if atom exists in database
	 * @return boolean
	 */
	public function exists(){
        if($this->concept->inAtomCache($this)){
            // $this->logger->debug("#217 One query saved due to caching existing atoms that exist in database");
            return true;
        }elseif($this->id === '_NEW'){
            return true; // Return true if id is '_NEW' (special case)
        }elseif($this->database->atomExists($this)){
            $this->concept->addToAtomCache($this);
    		return true;
        }else{
            return false;
        }
	}
	
	/**
	 * Add atom to concept in database
	 * @return void
	 */
	public function addAtom(){
        if($this->exists()){
            $this->logger->debug("Atom '{$this}' already exists in database");
        }else{
            $this->database->addAtomToConcept($this);
            $this->concept->addToAtomCache($this);
        }
	    return $this;
	}
    
    /**
     * 
     * @return void
     */
    public function deleteAtom(){
        if($this->exists()){
            $this->database->deleteAtom($this);
            $this->concept->removeFromAtomCache($this);
        }else{
            $this->logger->debug("Cannot delete atom '{$this}', because it does not exists");
        }
    }
    
    /**
     * @param string|Atom $tgtAtom
     * @param string|Relation $relation when provided as string, use relation signature
     * @param boolean $isFlipped specifies if $this and $tgtAtom must be flipped to match the relation
     * @return Link
     */
    public function link($tgtAtom, $relation, $isFlipped = false){
        if(is_string($relation)) $relation = Relation::getRelation($relation);
        if(is_string($tgtAtom)) $tgtAtom = $isFlipped ? new Atom($tgtAtom, $relation->srcConcept) : new Atom($tgtAtom, $relation->tgtConcept);        
        
        if($isFlipped) return new Link($relation, $tgtAtom, $this);
        else return new Link($relation, $this, $tgtAtom);
    }
    
    /**
     * Save query row data (can be used for subinterfaces)
     * @param arry $queryData 
     * @return void
     */
    public function setQueryData($data){
        $this->queryData = $data;
    }
    
    /**
     * 
     * @param string $colName
     * @throws Exception when column is not defined in query data
     * @return string
     */
    public function getQueryData($colName = null){
        if(is_null($colName)){
            return (array) $this->queryData;
        }else{
            // column name is prefixed with 'ifc_' to prevent duplicates with 'src' and 'tgt' cols, which are standard added to query data
            if(!array_key_exists($colName, (array) $this->queryData)) throw new Exception("Column '{$colName}' not defined in query data of atom '{$this->__toString()}'", 1001);
            return $this->queryData[$colName];
        }
    }
}

?>