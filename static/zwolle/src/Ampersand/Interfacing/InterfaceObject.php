<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;
use Ampersand\Core\Relation;
use Ampersand\Core\Concept;
use Ampersand\Interfacing\View;
use Ampersand\Core\Atom;
use Ampersand\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class InterfaceObject {
    /**
     * Contains all interface definitions
     * @var InterfaceObject[]
     */
	private static $allInterfaces; // contains all interface objects
	
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
	 * Interface id (i.e. safe name) to use in framework
	 * @var string
	 */
	public $id;
	
	/**
	 * 
	 * @var string
	 */
	public $path;
	
	/**
	 * Interface name to show in UI
	 * @var string
	 */
	public $label;
	
	/**
	 * Specifies is this interface object is a toplevel interface (true) or subinterface (false)
	 * @var boolean
	 */
	public $isTopLevelIfc = false;
	
	/**
	 * Roles that have access to this interface
	 * Only applies to top level interface objects
	 * @var string[]
	 */
	public $ifcRoleNames = array();
	
	/**
	 * Array with concepts for which all atoms may be get with the api
	 * This applies to all concepts that are used as target of a (sub)interface expression that may be updated (crudU)
	 * @var Concept[]
	 */
	public $editableConcepts = array();
	
	/**
	 * 
	 * @var boolean
	 */
	public $crudC;
	
	/**
	 * 
	 * @var boolean
	 */
	public $crudR;
	
	/**
	 * 
	 * @var boolean
	 */
	public $crudU;
	
	/**
	 * 
	 * @var boolean
	 */
	public $crudD;
	
	/**
	 * 
	 * @var Relation|null
	 */
	public $relation;
	
	/**
	 * 
	 * @var boolean|null
	 */
	public $relationIsFlipped;
	
	/**
	 * 
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
	public $isIdent;
	
	/**
	 * 
	 * @var string
	 */
	public $query;
	
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
	 * 
	 * @var View
	 */
	public $view;
	
	/**
	 * 
	 * @var string
	 */
	public $refInterfaceId;
	
	/**
	 * 
	 * @var boolean
	 */
	public $isLinkTo;
	
	/**
	 * 
	 * @var InterfaceObject[]
	 */
	public $subInterfaces = array();
	
	/**
	 * 
	 * @var Atom
	 */
	public $srcAtom = null;
	
	/**
	 * 
	 * @var Atom
	 */
	public $tgtAtom = null;
	

	/**
	 * InterfaceObject constructor
	 * @param array $ifcDef
	 * @param string $pathEntry
	 */
	public function __construct($ifcDef, $pathEntry = null){
		$this->database = Database::singleton();
		$this->logger = Logger::getLogger('FW');
		
		// Set attributes from $ifcDef
		$this->id = $ifcDef['id'];
		$this->label = $ifcDef['label'];
		
		$this->view = is_null($ifcDef['viewId']) ? null : View::getView($ifcDef['viewId']);
		
		$this->path = is_null($pathEntry) ? $this->id : "{$pathEntry}/{$this->id}";
		
		// Information about the (editable) relation if applicable
		$this->relation = is_null($ifcDef['relation']) ? null : Relation::getRelation($ifcDef['relation']);
		$this->relationIsFlipped = $ifcDef['relationIsFlipped'];
		
		// Interface expression information
		$this->srcConcept = Concept::getConcept($ifcDef['expr']['srcConcept']);
		$this->tgtConcept = Concept::getConcept($ifcDef['expr']['tgtConcept']);
		$this->isUni = $ifcDef['expr']['isUni'];
		$this->isTot = $ifcDef['expr']['isTot'];
		$this->isIdent = $ifcDef['expr']['isIdent'];
		$this->query = $ifcDef['expr']['query'];
		
		// CRUD rights
		$this->crudC = $ifcDef['crud']['create'];
		$this->crudR = $ifcDef['crud']['read'];
		$this->crudU = $ifcDef['crud']['update'];
		$this->crudD = $ifcDef['crud']['delete'];
		if($this->crudU && $this->tgtConcept->isObject) $this->editableConcepts[] = $this->tgtConcept;
		
		// Interface expression must equal (editable) relation when crudU is specified
		if($this->crudU && is_null($this->relation)) $this->logger->warning("Update rights (crUd) specified while interface expression is not an editable relation for (sub)interface: {$this->path}");
		    
		// Check for unsupported patchReplace functionality due to missing 'old value'. Related with issue #318
		if(!is_null($this->relation) && $this->crudU && !$this->tgtConcept->isObject && $this->isUni){
		    // Only applies to editable relations
		    // Only applies to crudU, because issue is with patchReplace, not with add/remove
		    // Only applies to scalar, because objects don't use patchReplace, but Remove and Add
		    // Only if interface expression (not! the relation) is univalent, because else a add/remove option is used in the UI
		    
		    if((!$this->relationIsFlipped && $this->relation->getMysqlTable()->tableOf == 'tgt')
		            || ($this->relationIsFlipped && $this->relation->getMysqlTable()->tableOf == 'src'))
		        $this->logger->warning("Unsupported edit functionality due to combination of factors for (sub)interface: '{$this->path}' - {$this->relation->__toString()}" . ($this->relationIsFlipped ? '~' : '') . " administrated in table of '{$this->relation->getMysqlTable()->tableOf}'");
		}
		    
		
		// Subinterfacing
		if(!is_null($ifcDef['subinterfaces'])){
		    // Subinterfacing is not supported/possible for tgt concepts with a scalar representation type (i.e. non-objects)
		    if(!$this->tgtConcept->isObject) throw new Exception ("Subinterfacing is not supported for concepts with a scalar representation type (i.e. non-objects). (Sub)Interface '{$this->path}' with target {$this->tgtConcept->__toString()} (type:{$this->tgtConcept->type}) has subinterfaces specified", 501);
		    
		    // Reference to top level interface
		    $this->refInterfaceId = $ifcDef['subinterfaces']['refSubInterfaceId'];
		    $this->isLinkTo = $ifcDef['subinterfaces']['refIsLinTo'];
		    
		    // Inline subinterface definitions
		    foreach ((array)$ifcDef['subinterfaces']['ifcObjects'] as $subIfcDef){
		        $ifc = new InterfaceObject($subIfcDef, $this->path);
		        $this->subInterfaces[$ifc->id] = $ifc;
		        $this->editableConcepts = array_merge($this->editableConcepts, $ifc->editableConcepts);
		    }
		}
	}
	
	public function __toString() {
		return $this->id;
	}
	
	/**
	 * Returns if interface expression relation is a property
	 * @return boolean
	 */
	public function isProp(){
	    return is_null($this->relation) ? false : $this->relation->isProp;
	}
	
	/**
	 * 
	 * @param Atom $atom
	 */
	public function setSrcAtom($atom){
	    // Check if atom can be used as source for this interface
	    if($atom->concept != $this->srcConcept && !in_array($atom->concept, $this->srcConcept->getGeneralizations())) throw new Exception ("Atom '{$atom->__toString()}' does not match source concept [{$this->srcConcept->name}] or any of its generalizations. Interface path: '{$this->path}'", 500);
	    
	    $this->srcAtom = $atom;
	    $this->path = $this->srcAtom->path . '/' . $this->id;
	    
	}
	
	public function getSubinterface($ifcId){	    
	    if(!array_key_exists($ifcId, $subifcs = $this->getSubinterfaces())) throw new Exception("Subinterface '{$ifcId}' does not exists in interface '{$this->path}'", 500);
	
	    return $subifcs[$ifcId];
	}
	
	public function getSubinterfaceByLabel($ifcLabel){
	    foreach ($this->getSubinterfaces() as $ifc)
	        if($ifc->label == $ifcLabel) return $ifc;
	    
	    throw new Exception("Subinterface '{$ifcLabel}' does not exists in interface '{$this->path}'", 500);
	}
	
	private function getSubinterfaces(){
	    if(is_null($this->refInterfaceId)) return $this->subInterfaces;
	    else return self::getInterface($this->refInterfaceId)->getSubinterfaces();
	}
	
/**************************************************************************************************
 * 
 * Fuctions related to chaining interfaces and atoms
 * 
 *************************************************************************************************/
	
	/**
	 * * Chains an atom to this interface as tgtAtom
	 * @param string $atomId
	 * @throws Exception
	 * @return Atom
	 */
	public function atom($atomId){
	    $atom = new Atom($atomId, $this->tgtConcept->name, $this);
	    
	    // Check if tgtAtom is part of tgtAtoms of interface
	    if(in_array($atomId, $this->getTgtAtomIds())) $this->tgtAtom = $atom;
	    
	    // Check if atom does not exist and if it may be created here
	    elseif(!$atom->atomExists() && $this->crudC){
	        // If interface expression is a relation, add tuple($this->srcAtom, $atom) in this relation
	        if($this->relation) $this->relation->addLink($this->srcAtom, $atom, $this->relationIsFlipped);
	        // Else only create atom
	        else $atom->addAtom();
	        
	        $this->tgtAtom = $atom;
	    }
	    // Else throw exception
	    else throw new Exception ("Resource '{$atom->id}[{$atom->concept->name}]' not found", 404);
	    
	    return $this->tgtAtom;
	}

/**************************************************************************************************
 *
 * Functions to get content of interface
 *
 *************************************************************************************************/
	
	/**
	 * Returns list of target atom ids given the srcAtom of this interface object
	 * @throws Exception
	 * @return array
	 */
	private function getTgtAtomIds(){
	    $query = "SELECT DISTINCT `tgt` FROM ({$this->query}) AS `results` WHERE `src` = '{$this->srcAtom->idEsc}' AND `tgt` IS NOT NULL";
	    $tgtAtomIds = array_column((array)$this->database->Exe($query), 'tgt');

	    // Integrity check
	    if($this->isUni && (count($tgtAtomIds) > 1)) throw new Exception("Univalent (sub)interface returns more than 1 resource: '{$this->path}'", 500);
	    
	    return (array)$tgtAtomIds;
	}
	
	/**
	 * Returns the content of this interface given the srcAtom of this interface object
	 * @param array $options
	 * @param array $recursionArr
	 * @throws Exception
	 * @return mixed
	 */
	public function getContent($options = array(), $recursionArr = array()){
	    // CRUD check
	    if(!$this->crudR) throw new Exception("Read not allowed for '{$this->path}'", 405);
	    
	    // Default options
	    $options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
	    $options['metaData'] = isset($options['metaData']) ? filter_var($options['metaData'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['navIfc'] = isset($options['navIfc']) ? filter_var($options['navIfc'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
	    
	    // Initialize result array
	    if($this->tgtConcept->isObject && !$this->isProp()) $result = array(); // return array if tgtConcept is an object (except properties), even if result is empty
	    elseif(!$this->isUni) $result = array(); // return array for non-univalent interfaces
	    else $result = null; // else (i.e. properties and univalent scalars)
	    
	    // Loop over target atoms
	    foreach ($this->getTgtAtomIds() as $tgtAtomId){
	        	
	        $tgtAtom = new Atom($tgtAtomId, $this->tgtConcept->name, $this);
	        	
	        // Object
	        if($this->tgtConcept->isObject){
	            // Property leaf: a property at a leaf of a (sub)interface is presented as true/false
	            if($this->isProp() && !$this->isIdent && empty($this->getSubinterfaces())){
	                $result = !is_null($tgtAtom->id); // convert NULL into false and everything else in true
	    
	            // Regular object, with or without subinterfaces
	            }else{
	                $content = $tgtAtom->getContent($options, $recursionArr);
	                	
	                // Add target atom to result array
	                switch($options['arrayType']){
	                    case 'num' :
	                        // if($this->isUni) $result = $content; else $result[] = $content;
	                        $result[] = $content;
	                        break;
	                    case 'assoc' :
	                        // if($this->isUni) $result = $content; else $result[$tgtAtom->id] = $content;
	                        $result[$tgtAtom->id] = $content;
	                        break;
	                    default :
	                        throw new Exception ("Unknown arrayType specified: '{$options['arrayType']}'", 500);
	                }
	                	
	            }
	    
	        // Scalar
	        }else{
	            // Leaf
	            if(empty($this->getSubinterfaces())){
	                $content = $tgtAtom->getJsonRepresentation();
	                	
	                if($this->isUni) $result = $content;
	                else $result[] = $content;
	                	
	            // Tree
	            }else{
	                throw new Exception("Scalar cannot have a subinterface (box) defined: '{$this->path}'", 500);
	            }
	        }
	    }
	    
	    // Return result
	    return $result;
	}
	
/**************************************************************************************************
 *
 * CREATE, UPDATE, PATCH and DELETE functions
 *
 *************************************************************************************************/
	
	/**
	 * Function to create a new Atom at the given interface.
	 * @param array $data
	 * @param array $options
	 * @throws Exception
	 * @return mixed
	 */
	public function create($data, $options = array()){	
	    // CRUD check
	    if(!$this->crudC) throw new Exception ("Create not allowed for '{$this->path}'", 405);
	    if(!$this->tgtConcept->isObject) throw new Exception ("Cannot create non-object [{$this->tgtConcept->name}] in '{$this->path}'. Use PATCH add operation instead", 405);
	    
	    // Handle options
	    if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
	    // Perform create
	    $newAtom = $this->tgtConcept->createNewAtom();
	    $newAtom->addAtom();
	
	    // Special case for CREATE in I[Concept] interfaces
	    if($this->srcAtom->id === '_NEW_'){
	        $this->srcAtom->setId($newAtom->id);
	        $this->path = str_replace('_NEW_', $newAtom->id, $this->path);
	    }
	
	    // If interface expression is a relation, also add tuple(this, newAtom) in this relation
	    if($this->relation) $this->relation->addLink($this->srcAtom, $newAtom, $this->relationIsFlipped);
	    
	    // Walk to new atom
	    $newAtom = $this->atom($newAtom->id);
	    
	    // Set requested state (using patches)
	    $patches = is_array($data) ? $data['patches'] : array();
	    $newAtom->doPatches($patches);
	
	    // Special case for file upload. TODO: make extension with hooks
	    if($this->tgtConcept->name == "FileObject"){
	         
	        if (is_uploaded_file($_FILES['file']['tmp_name'])){
	            $tmp_name = $_FILES['file']['tmp_name'];
	            $new_name = time() . '_' . $_FILES['file']['name'];
	            $absolutePath = Config::get('absolutePath') . Config::get('uploadPath') . $new_name;
	            $relativePath = Config::get('uploadPath') . $new_name;
	            $result = move_uploaded_file($tmp_name, $absolutePath);
	             
	            if($result) Logger::getUserLogger()->notice("File '{$new_name}' uploaded");
	            else throw new Exception ("Error in file upload", 500);
	
	            // Populate filePath and originalFileName relations in database
	            $relFilePath = Relation::getRelation('filePath', $newAtom->concept->name, 'FilePath');
	            $relOriginalFileName = Relation::getRelation('originalFileName', $newAtom->concept->name, 'FileName');
	            
	            $relFilePath->addLink($newAtom, new Atom($relativePath, 'FilePath'));
	            $relOriginalFileName->addLink($newAtom, new Atom($_FILES['file']['name'], 'FileName'));
	
	        }else{
	            throw new Exception ("No file uploaded", 500);
	        }
	    }
	
	    // Close transaction
	    $atomStoreNewContent = $this->crudR ? $newAtom : null; // Get and store new content if interface is readable (crudR)
	    $this->database->closeTransaction($newAtom->concept->name . ' created', null, $atomStoreNewContent);
	
	    // Return atom content (can be null)
	    return $newAtom->getStoredContent();
	
	}
	
	/**
	 * Function not implemented. Use Atom->update() method instead.
	 * @throws Exception
	 */
	public function update(){
	    throw new Exception ("Cannot update from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->patch() method instead.
	 * @throws Exception
	 */
	public function patch(){
	    throw new Exception ("Cannot patch from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->delete() method instead.
	 * @throws Exception
	 */
	public function delete(){
	    throw new Exception ("Cannot delete from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
/**************************************************************************************************
 *
 * Functions to perform patches (on relations): add, replace, remove
 *
 *************************************************************************************************/
	
	/**
	 * Replace (src,tgt) tuple by (src,tgt') in relation provided in this interface
	 * @throws Exception
	 * @return void
	 */
	public function doPatchReplace($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	
	    // PatchReplace only works for UNI expressions. Otherwise, use patch remove and patch add
	    if(!$this->isUni) throw new Exception("Cannot patch replace for non-univalent interface '{$this->path}'. Use patch remove + add instead", 500);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch replace. No 'value' specfied for patch with path '{$this->path}'", 400);
	    $value = $patch['value'];
	
	    // Interface is property
	    if($this->isProp() && !$this->isIdent){
	        // Throw error when patch value is something else then true, false or null
	        if(!(is_bool($value) || is_null($value))) throw new Exception("Interface '{$this->path}' is property, boolean expected, non-boolean provided");
	        	
	        // When true
	        if($value) $this->relation->addLink($this->srcAtom, $this->srcAtom, $this->relationIsFlipped);
	        // When false or null
	        else $this->relation->deleteLink($this->srcAtom, $this->srcAtom, $this->relationIsFlipped);
	        	
	    // Interface is a relation to an object
	    }elseif($this->tgtConcept->isObject){
	        throw new Exception("Cannot patch replace for object reference in interface '{$this->this}'. Use patch remove + add instead", 500);
	
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConcept->isObject){
	        
	        // Replace by nothing => deleteLink
	        if(is_null($value)) $this->relation->deleteLink($this->srcAtom, new Atom(null, $this->tgtConcept->name), $this->relationIsFlipped);
	        // Replace by other atom => addLink
	        else $this->relation->addLink($this->srcAtom, new Atom($value, $this->tgtConcept->name), $this->relationIsFlipped);
	        
	    }else{
	        throw new Exception ("Unknown patch replace. Please contact the application administrator", 500);
	    }
	}
	
	/**
	 * Add (src,tgt) tuple in relation provided in this interface
	 * @throws Exception
	 * @return void
	 */
	public function doPatchAdd($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch add. No 'value' specfied in '{$this->path}'", 400);
	    
	    $tgtAtom = new Atom($patch['value'], $this->tgtConcept->name);
	    
	    // Interface is property
	    if($this->isProp() && !$this->isIdent){
	        // Properties must be treated as a 'replace', so not handled here
	        throw new Exception("Cannot patch add for property '{$this->path}'. Use patch replace instead", 500);
	
	    // Interface is a relation to an object
	    }elseif($this->tgtConcept->isObject){
	        // Check: If tgtAtom (value) does not exists and there is not crud create right, throw exception
	        if(!$this->crudC && !$tgtAtom->atomExists()) throw new Exception ("Resource '{$tgtAtom->id}[{$tgtAtom->concept->name}]' does not exist and may not be created in {$this->path}", 403);
	        	
	        $this->relation->addLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
	
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConcept->isObject){    	
	        // Check: If interface is univalent, throw exception
	        if($this->isUni) throw new Exception("Cannot patch add for univalent interface {$this->path}. Use patch replace instead", 500);
	        	
	        $this->relation->addLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
	        
	    }else{
	        throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
	    }
	}
	
	/**
	 * Function not implemented. Use Atom->doPatchRemove() method instead.
	 * @throws Exception
	 */
	public function doPatchRemove(){
	    throw new Exception ("Cannot patch remove from '{$this->path}'. Missing resource identifier", 405);
	}
	
/**************************************************************************************************
 *
 * Static InterfaceObject functions
 *
 *************************************************************************************************/
	
	/**
	 * Returns toplevel interface object
	 * @param string $ifcId
	 * @throws Exception when interface does not exists
	 * @return InterfaceObject
	 */
	public static function getInterface($ifcId){
		if(!array_key_exists($ifcId, $interfaces = self::getAllInterfaces())) throw new Exception("Interface '{$ifcId}' is not defined", 500);
		
		return $interfaces[$ifcId];
	}
	
	
	public static function getInterfaceByLabel($ifcLabel){
	    foreach(self::getAllInterfaces() as $interface)
	        if($interface->label == $ifcLabel) return $interface;
	    
	    throw new Exception("Interface with label '{$ifcLabel}' is not defined", 500);
	}
	
	/**
	 * Returns all toplevel interface objects
	 * @return InterfaceObject[]
	 */
	public static function getAllInterfaces(){
	    if(!isset(self::$allInterfaces)) self::setAllInterfaces();
	    
	    return self::$allInterfaces;   
	}
	
	/**
	 * Returns all toplevel interface objects that are not assigned to a role
	 * @return InterfaceObject[]
	 */
	public static function getPublicInterfaces(){
	    $interfaces = array();
	    foreach(InterfaceObject::getAllInterfaces() as $ifc){
	        if (empty($ifc->ifcRoleNames)) $interfaces[$ifc->id] = $ifc;
	    }
	    return $interfaces;
	}
	
	/**
	 * Import all interface object definitions from json file and create and save InterfaceObject objects
	 * @return void
	 */
	private static function setAllInterfaces(){
	    self::$allInterfaces = array();
	    
	    // import json file
	    $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'interfaces.json');
	    $allInterfaceDefs = (array)json_decode($file, true);
	    
	    
	    foreach ($allInterfaceDefs as $ifcDef){
	        $ifc = new InterfaceObject($ifcDef['ifcObject']);
	        
	        // Set additional information about this toplevel interface object
	        $ifc->isTopLevelIfc = true; // Specify as top level ifc
	        $ifc->ifcRoleNames = $ifcDef['interfaceRoles'];
	        
	        self::$allInterfaces[$ifc->id] = $ifc;
	    }
	}
}

?>