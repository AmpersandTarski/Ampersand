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
	private $database;
	
	/**
	 *
	 * @var \Psr\Log\LoggerInterface
	 */
	private $logger;
	
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
        $this->idEsc = Database::getDBRepresentation($this); // Escape and transform id for (mysql) database queries
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
	public function atomExists(){
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
        if($this->atomExists()){
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
        if($this->atomExists()){
            $this->database->deleteAtom($this);
            $this->concept->removeFromAtomCache($this);
        }else{
            $this->logger->debug("Cannot delete atom '{$this}', because it does not exists");
        }
    }
}

?>