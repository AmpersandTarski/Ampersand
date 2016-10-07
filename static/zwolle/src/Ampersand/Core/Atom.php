<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;

use Exception;
use DateTime;
use DateTimeZone;
use Ampersand\Database\Database;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Concept;
use Ampersand\Log\Logger;
use Ampersand\Config;
use Ampersand\Session;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Atom {
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
	 * Label of atom to be displayed in user interfaces
	 * @var string
	 */
	private $label;
	
	/**
	 * Array of attributes of this atom to be used by the user interface frontend templates
	 * @var array
	 */
	private $view;
	
	/**
	 * Specifies the concept of which this atom is an instance
	 * @var Concept
	 */
	public $concept;
	
	/**
	 * Specifies interface id to be used to get/set stored content if no $parentIfc is applicable (i.e. atom is src of toplevel interface)
	 * @var string
	 */
	public $topLevelIfcId = null;
	
	/**
	 * Variable to temporarily store changed atom content
	 * @var mixed
	 */
	private $storedContent = null;
    
	/**
	 * Atom constructor
	 * @param string $atomId
	 * @param Concept $concept
	 * @param InterfaceObject $ifc
     * @param array $qData the row data (from database query) from which this atom is created
	 * @return void
	 */
	public function __construct($atomId, Concept $concept, InterfaceObject $ifc = null){
		$this->database = Database::singleton();
		$this->logger = Logger::getLogger('ATOM');
		
        $this->concept = $concept;
		
		$this->setId($atomId);

	}
	
	public function __toString(){
        // if atom id is longer than 40 chars, display first and last 20 chars
        $id = strlen($this->id) > 40 ? substr($this->id, 0, 20) . '...' . substr($this->id, -20) : $this->id;
	    return "{$id}[{$this->concept}]";
	}
	
	/**
	 * Set identifier of atom
	 * @param string $id
	 */
	public function setId($id){
	    $this->id = $id;
		$this->idEsc = $this->database->escape($this->getMysqlRepresentation()); // Escape id for database queries
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
	
	/**
	 * Returns basic information about this atom
	 * @param array $options
	 * @return array
	 */
	public function getAtom($options = array()){
		$result = array('_id_' => $this->getJsonRepresentation(), '_label_' => $this->getLabel(), '_view_' => $this->getView());
		
		if($options['navIfc']){
		    $ifcs = array();
			foreach($this->concept->getInterfaces() as $ifc){
				$ifcs[] = array('id' => $ifc->id, 'label' => $ifc->label, 'url' => $this->url . '/' . $ifc->id);
			}
			
			$result['_ifcs_'] = $ifcs;
		}
		
		return $result;
	}
    
    /**
     * Returns label (from view or atom id) for this atom
     * @return string
     */
    public function getLabel(){
        if(!isset($this->label)){
            $viewStr = implode($this->getView());
            $this->label = empty(trim($viewStr)) ? $this->id : $viewStr; // empty view => label = id
        }
        return $this->label;
    }
	
	/**
	 * Returns view array of key-value pairs for this atom
	 * @return array
	 */
	private function getView(){        
        // If view is not already set
        if(!isset($this->view)){
            $this->logger->debug("Get view for atom '{$this->__toString()}'");
            $this->view = array();
            
            // If parentIfc is set, use view as defined by interface (can be null)
            if(isset($this->parentIfc)) $viewDef = $this->parentIfc->view;
            // Else use default view of concept (can be null)
            else $viewDef = $this->concept->getDefaultView();
            
            // If there is a view definition
            if(!is_null($viewDef)){
                foreach ($viewDef->segments as $viewSegment){
                    $key = is_null($viewSegment->label) ? $viewSegment->seqNr : $viewSegment->label;
                    
                    switch ($viewSegment->segType){
                        case "Text" :
                            $this->view[$key] = $viewSegment->text;
                            break;
                        case "Exp" :
                            try {
                                // Try to get view segment from atom query data
                                $this->view[$key] = $this->getQueryData('view_' . $key); // column is prefixed with view_
                                // $this->logger->debug("VIEW <{$viewDef->label}:{$key}> #217 Query saved due to reusing data from source atom");                                
                            
                            }catch (Exception $e) {
                                // Column not defined, perform query
                                if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                                    $query = "/* VIEW <{$viewDef->label}:{$key}> */ SELECT DISTINCT `tgt` FROM ({$viewSegment->expSQL}) AS `results` WHERE `src` = '{$this->idEsc}' AND `tgt` IS NOT NULL";
                                    $tgtAtoms = array_column((array)$this->database->Exe($query), 'tgt');
                                    $this->view[$key] = count($tgtAtoms) ? $tgtAtoms[0] : null;
                                }else{
                                    throw $e;
                                }
                            }
                            break;
                        default :
                            throw new Exception("Unsupported segmentType '{$viewSegment->segType}' in VIEW <{$viewDef->label}:{$key}>", 501); // 501: Not implemented
                            break;
                    }
                }
            }
        }
        return $this->view;
	}
	
	/**
	 * Return json representation of Atom (identifier) according to Ampersand technical types (TTypes)
	 * @throws Exception when technical type is not (yet) supported
	 * @return mixed
	 */
	public function getJsonRepresentation(){
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
	 * Return mysql representation of Atom (identifier) according to Ampersand technical types (TTypes)
	 * @throws Exception when technical type is not (yet) supported
	 * @return mixed
	 */
	public function getMysqlRepresentation(){
	    if(is_null($this->id)) return null;
	    
	    switch($this->concept->type){
	        case "ALPHANUMERIC" :
	        case "BIGALPHANUMERIC" :
	        case "HUGEALPHANUMERIC" :
	        case "PASSWORD" :
	        case "TYPEOFONE" :
	            return (string) $this->id;
	        case "BOOLEAN" :
	            return (int) $this->id; // booleans are stored as tinyint(1) in the database. false = 0, true = 1
	        case "DATE" :
	            $datetime = new DateTime($this->id);
	            return $datetime->format('Y-m-d'); // format to store in database
	        case "DATETIME" :
	            $datetime = new DateTime($this->id); // $this->id can include timezone, e.g. 2005-08-15T15:52:01+00:00 (DATE_ATOM format)
	            $datetime->setTimezone(new DateTimeZone('UTC')); // convert to UTC to store in database
	            return $datetime->format('Y-m-d H:i:s'); // format to store in database (UTC)
	        case "FLOAT" :
	            return (float) $this->id;
	        case "INTEGER" :
	            return (int) $this->id;
	        case "OBJECT" :
	            return $this->id;
	        default :
	            throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept}]'", 501);
	    }
	}
	
/**************************************************************************************************
 *
 * Functions to get content of atom using interfaces
 *
 *************************************************************************************************/
	
	/**
	 * Store content of atom at a certain point (e.g. before database commit/rollback)
	 * @param array $options
     * @return void
	 */
	public function setStoredContent($options = []){
	    $this->logger->debug("Caching new concent for atom '{$this->__toString()}'");
        if(is_null($this->parentIfc)) throw new Exception("Cannot get content: no interface specified.", 500);
        
	    // $this->storedContent = $this->getContent($options);
	}
	
	/**
	 * Return the stored content
	 * @return mixed
	 */
	public function getStoredContent(){
	    $this->logger->debug("Getting cached concent for atom '{$this->__toString()}'");
	    return $this->storedContent;
	}

}

?>