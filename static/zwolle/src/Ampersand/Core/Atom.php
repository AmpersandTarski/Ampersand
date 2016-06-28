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
	 * Url to this atom (i.e. <serverUrl>/<apiPath>/resource/<conceptName>/<atomId>)
	 * @var string
	 */
	public $url;
	
	/**
	 * Specifies path (interface + atom) from which this atom is instantiated
	 * @var string
	 */
	public $path;
	
	/**
	 * Specifies the interface from which this atom is instantiated
	 * @var InterfaceObject
	 */
	private $parentIfc;
	
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
     * @var array|null $qData The row data (from database query) from which this atom is created
     */
    private $qData = null;
    
	/**
	 * Atom constructor
	 * @param string $atomId
	 * @param string $conceptName
	 * @param InterfaceObject $ifc
     * @param array|null $qData the row data (from database query) from which this atom is created
	 * @return void
	 */
	public function __construct($atomId, $conceptName, $ifc = null, $qData = null){
		$this->database = Database::singleton();
		$this->logger = Logger::getLogger('FW');
		
		$this->parentIfc = $ifc;
		$this->concept = Concept::getConcept($conceptName);
        $this->qData = $qData;
		
		$this->setId($atomId);
		
		// JSON-LD attributes
		$this->url = Config::get('serverURL') . Config::get('apiPath') . '/resource/' . $this->concept->name . '/' . $this->getJsonRepresentation();

	}
	
	public function __toString(){
	    return "{$this->id}[{$this->concept->name}]";
	}
	
	/**
	 * Set identifier of atom
	 * @param string $id
	 */
	public function setId($id){
	    // Decode url encoding for objects
	    $this->id = $this->concept->isObject ? rawurldecode($id) : $id;
	    
	    // Escape id for database queries
		$this->idEsc = $this->database->escape($this->getMysqlRepresentation());
		
        $this->path = is_null($this->parentIfc) ? 'resources/' . $this->concept->name : $this->parentIfc->path;
        $this->path .= '/' . $this->getJsonRepresentation();
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
            $this->logger->debug("Atom '{$this->__toString()}' already exists in database");
        }else{
            $this->database->addAtomToConcept($this);
            $this->concept->addToAtomCache($this);
        }
	    return $this;
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
    
    public function getQueryData($colName = null){
        if(is_null($colName)){
            if(is_null($this->qData)) return array();
            else return $this->qData;
        }else{
            // column name is prefixed with 'ifc_' to prevent duplicates with 'src' and 'tgt' cols, which are standard added to query data
            if(!array_key_exists($colName, (array)$this->qData)) throw new Exception("Column '{$colName}' not defined in query data of atom '{$this->__toString()}'", 1001);
            return $this->qData[$colName];
        }
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
	            throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept->name}]'", 501);
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
	            throw new Exception("Unknown/unsupported representation type '{$this->concept->type}' for concept '[{$this->concept->name}]'", 501);
	    }
	}
	
/**************************************************************************************************
 *
 * Fuctions related to chaining interfaces and atoms
 *
 *************************************************************************************************/

	/**
	 * Chains this atom to an interface as srcAtom 
	 * @param string $ifcId
	 * @throws Exception
	 * @return InterfaceObject
	 */
	public function ifc($ifcId){
	    if(is_null($this->parentIfc)) $ifc = InterfaceObject::getInterface($ifcId);
        elseif($this->parentIfc->isRef()) $ifc = InterfaceObject::getInterface($ifcId);
	    else $ifc = $this->parentIfc->getSubinterface($ifcId);
	    
	    $clone = clone $ifc;
	    $clone->setSrcAtom($this);
	     
	    return $clone;
	}
	
	/**
	 * Walks a given interface path starting with this atom as src. Returns the final InterfaceObject or target atom (depending on the last path parameter)
	 * @param string $path (e.g. /ifc/atom/ifc/atom/ifc)
	 * @throws Exception when path is not accessible within current session
	 * @return Atom|InterfaceObject
	 */
	public function walkIfcPath($path){
	    $session = Session::singleton();
	
	    if(!$this->atomExists()) throw new Exception ("Resource '{$this->__toString()}' not found", 404);
	     
	    $atom = $this; // starting point
	     
	    $path = trim($path, '/'); // remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
	    if($path == '') return $this; // if no path is specified, return $this (atom)
	     
	    $pathArr = explode('/', $path);
	    while (count($pathArr)){
	        // Ifc
	        $interfaceId = array_shift($pathArr); // returns the shifted value, or NULL if array is empty or is not an array.
	        $ifc = $atom->ifc($interfaceId);
	
	        // Checks
	        if($ifc->isTopLevelIfc && !$session->isAccessibleIfc($ifc->id)) throw new Exception("Interface is not accessible for session roles", 401); // 401: Unauthorized
	        if((!$ifc->crudR) && (count($pathArr) > 1)) throw new Exception ("Read not allowed for interface path '{$ifc->path}'", 405); // crudR required to walk the path further when this is not the last ifc part in the path (count > 1).
	
	        // Atom
	        $atomId = array_shift($pathArr); // returns the shifted value, or NULL if array is empty or is not an array.
	        $atom = is_null($atomId) ? null : $ifc->atom($atomId);
	    }
	     
	    return is_null($atom) ? $ifc : $atom;
	}
	
/**************************************************************************************************
 *
 * Functions to get content of atom using interfaces
 *
 *************************************************************************************************/
	
	/**
	 * Store content of atom at a certain point (e.g. before database commit/rollback)
	 * @return void
	 */
	public function setStoredContent(){
	    $this->logger->debug("Caching new concent for atom '{$this->__toString()}'");
	    // If parentIfc is null (toplevel) switch to topLevelIfc to be able to return/store the new content
	    $this->storedContent = is_null($this->parentIfc) ? $this->ifc($this->topLevelIfcId)->getContent() : $this->getContent();
	}
	
	/**
	 * Return the stored content
	 * @return mixed
	 */
	public function getStoredContent(){
	    $this->logger->debug("Getting cached concent for atom '{$this->__toString()}'");
	    return $this->storedContent;
	}
	
	/**
	 * Returns the content of this atom given the parentIfc object
	 * @param array $options
	 * @param array $recursionArr
     * @param int $depth specifies the number subinterface levels to get the content for
	 * @throws Exception
	 * @return mixed
	 */
	public function getContent($options = array(), $recursionArr = array(), $depth = null){
	    // CRUD check
	    if(!$this->parentIfc->crudR) throw new Exception("Read not allowed for '{$this->path}'", 405);
	    
	    $session = Session::singleton();
	    
	    // Default options
	    $options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
	    $options['metaData'] = isset($options['metaData']) ? filter_var($options['metaData'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['navIfc'] = isset($options['navIfc']) ? filter_var($options['navIfc'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
        if(isset($options['depth']) && is_null($depth)) $depth = $options['depth']; // initialize depth, if specified in options array
	    
	    $content = array( '_id_' => $this->getJsonRepresentation()
	                    , '_label_' => $this->getLabel()
	                    , '_view_' => $this->getView()
	                    );
	     
	    // Meta data
	    if($options['metaData']){
	        $content['_path_'] = $this->path;
	    }
	    
	    // Define interface(s) to navigate to for this tgtAtom
	    if($options['navIfc']){
	        $ifcs = array();
	        if($this->parentIfc->isLinkTo && $session->isAccessibleIfc($this->parentIfc->refInterfaceId))
	            $ifcs[] = array('id' => $this->parentIfc->refInterfaceId, 'label' => $this->parentIfc->refInterfaceId, 'url' => $this->url . '/' . $this->parentIfc->refInterfaceId);
	        else $ifcs = array_map(function($o) {
	            return array('id' => $o->id, 'label' => $o->label, 'url' => $this->url . '/' . $o->id);
	        }, $session->getInterfacesToReadConcept($this->concept->name));
	        $content['_ifcs_'] = $ifcs;
	    }
	    
        // Get content of subinterfaces if depth is not provided or max depth not yet reached
        if(is_null($depth) || $depth > 0) {
    	    // Decrease depth by 1
            if(!is_null($depth)) $depth--;
            
            // Subinterfaces
    	    foreach($this->parentIfc->subInterfaces as $subinterface){
    	        // Skip subinterface if not given read rights
    	        if(!$subinterface->crudR) continue;
    	         
    	        $subcontent = $this->ifc($subinterface->id)->getContent($options, $recursionArr, $depth);
    	        
    	        $content[$subinterface->id] = $subcontent;
    	    
    	        // _sortValues_ (if subInterface is uni)
    	        if($subinterface->isUni && $options['metaData']){
    	            if(is_bool($subcontent)) $sortValue = $subcontent; // property
    	            elseif($subinterface->tgtConcept->isObject) $sortValue = current((array)$subcontent)['_label_']; // use label to sort objects
    	            else $sortValue = $subcontent; // scalar
    	    
    	            $content['_sortValues_'][$subinterface->id] = $sortValue;
    	        }
    	    }
        }
	    
	    return $content;
			
	}
	
/**************************************************************************************************
 * 
 * CREATE, UPDATE, PATCH and DELETE functions 
 *  
 *************************************************************************************************/
	
    /**
     * Function not implemented. Use InterfaceObject->create() method instead.
     * @throws Exception
     */
	public function create(){
	    throw new Exception ("Cannot create atom at path '{$this->path}'. Add interface identifier behind path", 405);
	}
	
	/**
	 * Update atom properties in database. Function not (yet) implemented.
	 * @param mixed $data contains the data of this atom to put
	 * @param array $options
	 * @return array
	 */
	public function update($data, $options){
		throw new Exception ("Not yet implemented", 501);
	}

	/**
	 * Performs given patches on this atom and returns updated content
	 * @param array $patches
	 * @param array $options
	 * @return mixed updated content of atom
	 */
	public function patch($patches, $options = array()){
	    // CRUD check for patch is performed by Atom->doPatches() method
	        
		// Handle options
		if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
		$successMessage = isset($options['successMessage']) ? $options['successMessage'] : $this->concept->name . ' updated';
		
		// Perform patches
		$this->doPatches($patches);
		
		// Close transaction
		$this->database->closeTransaction($successMessage, null, $this);
		
		return $this->getStoredContent();
	}
	
	/**
	 * Function to delete an atom from Concept collection
	 * @param array $options
	 * @throws Exception when delete is not allowed/possible
	 * @return void
	 */
	public function delete($options = array()){
	    // CRUD check
	    if(!$this->parentIfc->crudD) throw new Exception("Delete not allowed for '{$this->path}'", 405);
	    if(!$this->parentIfc->tgtConcept->isObject) throw new Exception ("Cannot delete non-object '{$this->__toString()}' in '{$this->path}'. Use PATCH remove operation instead", 405);
	     
	    // Handle options
	    if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
	    // Perform delete
	    $this->database->deleteAtom($this);
	
	    // Close transaction
	    $this->database->closeTransaction($this->concept->name . ' deleted');
	
	    return;
	}
	
/**************************************************************************************************
 *
 * Functions to perform patches on atom, i.e. add, replace, remove tuples in relations
 *
 *************************************************************************************************/
	
	/**
	 * Performs given patches on atom, i.e. add, replace, remove tuples in relations
	 * @param array $patches
	 * @throws Exception
	 * @return void
	 */
	public function doPatches($patches = array()){	    
		$errorCount = 0;
		foreach ((array)$patches as $key => $patch){
			try{
				// Check patch
				if(!array_key_exists('op', $patch)) throw new Exception ("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
				if(!array_key_exists('path', $patch)) throw new Exception ("No 'path' specfied for patch #{$key}", 400);
				
				$atomOrIfc = $this->walkIfcPath($patch['path']);
		
				switch($patch['op']){
					case "replace" :
						$atomOrIfc->doPatchReplace($patch);
						break;
					case "add" :
						$atomOrIfc->doPatchAdd($patch);
						break;
					case "remove" :
						$atomOrIfc->doPatchRemove($patch);
						break;
					default :
						throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
				}
			}catch (Exception $e){
				Logger::getUserLogger()->error($e->getMessage());
				$errorCount++;
			}
		}
		
		if($errorCount){
			$totalPatches = count($patches);
			$processed = $totalPatches - $errorCount;
			Logger::getUserLogger()->warning("{$processed}/{$totalPatches} patches processed. {$errorCount} errors.");
		}
	}
	
    /**
     * Function not implemented. Use InterfaceObject->doPatchReplace() method instead.
     * @var array $patch
     * @throws Exception
     */
	public function doPatchReplace($patch){
	    throw new Exception ("Cannot patch replace from '{$this->path}'. Path ends with resource", 405);
	}
	
    /**
     * Function not implemented. Use InterfaceObject->doPatchAdd() method instead.
     * @var array $patch
     * @throws Exception
     */
	public function doPatchAdd($patch){
	    throw new Exception ("Cannot patch add from '{$this->path}'. Path ends with resource", 405);
	    
	}
	
    /**
     * Remove (src,tgt) tuple from relation provided in $this->parentIfc
     * @var array $patch
     * @throws Exception
     * @return void
     */
	public function doPatchRemove($patch){
	    $ifc = $this->parentIfc;
	   
	    // CRUD check
	    if(!$ifc->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	    
		// Interface is property
		if($ifc->isProp()){
			// Properties must be treated as a 'replace', so not handled here
			throw new Exception("Cannot patch remove for property '{$ifc->path}'. Use patch replace instead", 500);
		
		// Interface is a relation to an object
		}elseif($ifc->tgtConcept->isObject){
			
			$ifc->relation->deleteLink($this->parentIfc->srcAtom, $this, $ifc->relationIsFlipped);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$ifc->tgtConcept->isObject){
			if($ifc->isUni) throw new Exception("Cannot patch remove for univalent interface {$ifc->path}. Use patch replace instead", 500);
			
			$ifc->relation->deleteLink($this->parentIfc->srcAtom, $this, $ifc->relationIsFlipped);
			
		}else{
			throw new Exception ("Unknown patch remove. Please contact the application administrator", 500);
		}
		
	}
	
}

?>