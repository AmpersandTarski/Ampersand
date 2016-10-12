<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Ampersand\Session;
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
    
    const
        /** Default options */
        DEFAULT_OPTIONS     = 0b00000001,
        
        INCLUDE_REF_IFCS    = 0b00000001,
        
        INCLUDE_LINKTO_IFCS = 0b00000011; // linkto ifcs are ref(erence) interfaces
    
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
	 * Specifies if this interface object is a toplevel interface (true) or subinterface (false)
	 * @var boolean
	 */
	private $isRoot;
	
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
	private $view;
	
	/**
	 * 
	 * @var string
	 */
	private $refInterfaceId;
	
	/**
	 * 
	 * @var boolean
	 */
	public $isLinkTo;
	
	/**
	 * 
	 * @var InterfaceObject[]
	 */
	private $subInterfaces = array();

	/**
	 * InterfaceObject constructor
	 * @param array $ifcDef Interface object definition as provided by Ampersand generator
	 * @param string $pathEntry
     * @param boolean $rootIfc Specifies if this interface object is a toplevel interface (true) or subinterface (false)
	 */
	private function __construct($ifcDef, $pathEntry = null, $rootIfc = false){
		$this->database = Database::singleton();
		$this->logger = Logger::getLogger('FW');
		
        $this->isRoot = $rootIfc;
        
		// Set attributes from $ifcDef
		$this->id = $ifcDef['id'];
		$this->label = $ifcDef['label'];
		$this->view = is_null($ifcDef['viewId']) ? null : View::getView($ifcDef['viewId']);
		
		$this->path = is_null($pathEntry) ? $this->id : "{$pathEntry}/{$this->id}";
		
		// Information about the (editable) relation if applicable
		$this->relation = is_null($ifcDef['relation']) ? null : Relation::getRelation($ifcDef['relation']);
		$this->relationIsFlipped = $ifcDef['relationIsFlipped'];
		
		// Interface expression information
		$this->srcConcept = Concept::getConcept($ifcDef['expr']['srcConceptId']);
		$this->tgtConcept = Concept::getConcept($ifcDef['expr']['tgtConceptId']);
		$this->isUni = $ifcDef['expr']['isUni'];
		$this->isTot = $ifcDef['expr']['isTot'];
		$this->isIdent = $ifcDef['expr']['isIdent'];
		$this->query = $ifcDef['expr']['query'];
		
		// CRUD rights
		$this->crudC = $ifcDef['crud']['create'];
		$this->crudR = $ifcDef['crud']['read'];
		$this->crudU = $ifcDef['crud']['update'];
		$this->crudD = $ifcDef['crud']['delete'];
		if($this->crudU && $this->tgtConcept->isObject()) $this->editableConcepts[] = $this->tgtConcept;
		
		// Interface expression must equal (editable) relation when crudU is specified
		if($this->crudU && is_null($this->relation)) $this->logger->warning("Update rights (crUd) specified while interface expression is not an editable relation for (sub)interface: {$this->path}");
		    
		// Check for unsupported patchReplace functionality due to missing 'old value'. Related with issue #318
		if(!is_null($this->relation) && $this->crudU && !$this->tgtConcept->isObject() && $this->isUni){
		    // Only applies to editable relations
		    // Only applies to crudU, because issue is with patchReplace, not with add/remove
		    // Only applies to scalar, because objects don't use patchReplace, but Remove and Add
		    // Only if interface expression (not! the relation) is univalent, because else a add/remove option is used in the UI
		    
		    if((!$this->relationIsFlipped && $this->relation->getMysqlTable()->tableOf == 'tgt')
		            || ($this->relationIsFlipped && $this->relation->getMysqlTable()->tableOf == 'src'))
		        $this->logger->warning("Unsupported edit functionality due to combination of factors for (sub)interface: '{$this->path}' - {$this->relation}" . ($this->relationIsFlipped ? '~' : '') . " administrated in table of '{$this->relation->getMysqlTable()->tableOf}'");
		}
		    
		
		// Subinterfacing
		if(!is_null($ifcDef['subinterfaces'])){
		    // Subinterfacing is not supported/possible for tgt concepts with a scalar representation type (i.e. non-objects)
		    if(!$this->tgtConcept->isObject()) throw new Exception ("Subinterfacing is not supported for concepts with a scalar representation type (i.e. non-objects). (Sub)Interface '{$this->path}' with target {$this->tgtConcept} (type:{$this->tgtConcept->type}) has subinterfaces specified", 501);
		    
		    // Reference to top level interface
		    $this->refInterfaceId = $ifcDef['subinterfaces']['refSubInterfaceId'];
		    $this->isLinkTo = $ifcDef['subinterfaces']['refIsLinTo']; // not refIsLinkTo? no! typo in generics/interfaces.json
		    
		    // Inline subinterface definitions
		    foreach ((array)$ifcDef['subinterfaces']['ifcObjects'] as $subIfcDef){
		        $ifc = new InterfaceObject($subIfcDef, $this->path);
		        $this->subInterfaces[$ifc->id] = $ifc;
		        $this->editableConcepts = array_merge($this->editableConcepts, $ifc->editableConcepts);
		    }
		}
	}
	
    /**
     * Function is called when object is treated as a string
     * @return string
     */
	public function __toString() {
		return $this->id;
	}
    
    /**
     * Returns interface relation (when interface expression = relation), throws exception otherwise
     * @return Relation|Exception
     */
	public function relation(){
        if(is_null($this->relation)) throw new Exception ("Interface expression for '{$this->path}' is not a relation", 500);
        else return $this->relation;
    }
    
    /**
     * Returns if interface expression is editable (i.e. expression = relation)
     * @return boolean
     */
    public function isEditable(){
        return !is_null($this->relation);
    }

	/**
	 * Returns if interface expression relation is a property
	 * @return boolean
	 */
	public function isProp(){
	    return is_null($this->relation) ? false : ($this->relation->isProp && !$this->isIdent);
	}
    
    /**
	 * Returns if interface is a reference to another interface
	 * @return boolean
	 */
    public function isRef(){
        return !is_null($this->refInterfaceId);
    }
    
    /**
     * Returns identifier of interface object to which this interface refers to (or null if not set)
     * @return string|null
     */
    public function getRefToIfcId(){
        return $this->refInterfaceId;
    }
    
    /**
     * Returns if interface is a LINKTO reference to another interface
     * @return boolean
     */
    public function isLinkTo(){
        return $this->isLinkTo;
    }
    
    /**
     * Returns if interface object is a top level interface
     * @return boolean
     */
    public function isRoot(){
        return $this->isRoot;
    }
    
    /**
     * Returns if interface object is a leaf node
     * @return boolean
     */
    public function isLeaf(){
        return empty($this->getSubinterfaces());
    }
    
    /**
     * Returns if interface is a public interface (i.e. accessible every role, incl. no role)
	 * @return boolean
     */
    public function isPublic(){
        return empty($this->ifcRoleNames) && $this->isRoot();
    }
    
    public function isIdent(){
        return $this->isIdent && $this->srcConcept == $this->tgtConcept;
    }
    
    public function isUni(){
        return $this->isUni;
    }
    
    public function isTot(){
        return $this->isTot;
    }
    
    public function getPath(){
        return $this->path;
    }
    
    public function getView(){
        return $this->view;
    }
    
    public function crudC(){ return $this->crudC;}
    public function crudR(){ return $this->crudR;}
    public function crudU(){ return $this->crudU;}
    public function crudD(){ return $this->crudD;}
	
    /**
     * @param string $ifcId
     * @return InterfaceObject
     */
	public function getSubinterface($ifcId){
	    if(!array_key_exists($ifcId, $subifcs = $this->getSubinterfaces())) throw new Exception("Subinterface '{$ifcId}' does not exists in interface '{$this->path}'", 500);
	
	    return $subifcs[$ifcId];
	}
	
    /**
     * @param string $ifcLabel
     * @return InterfaceObject
     */
	public function getSubinterfaceByLabel($ifcLabel){
	    foreach ($this->getSubinterfaces() as $ifc)
	        if($ifc->label == $ifcLabel) return $ifc;
	    
	    throw new Exception("Subinterface '{$ifcLabel}' does not exists in interface '{$this->path}'", 500);
	}
    
    /**
     * Return array with all sub interface recursively (incl. the interface itself)
     * @return InterfaceObject[]
     */
    public function getInterfaceFlattened(){
        $arr = array();
        $arr[] = $this;
        foreach ($this->getSubinterfaces(self::DEFAULT_OPTIONS & ~self::INCLUDE_REF_IFCS) as $ifc){
            $arr = array_merge($arr, $ifc->getInterfaceFlattened());
        }
        return $arr;
    }
	
    /**
     * @param int $options
     * @return InterfaceObject[] 
     */
    public function getSubinterfaces($options = self::DEFAULT_OPTIONS){
        if($this->isRef() && ($options & self::INCLUDE_REF_IFCS) // if ifc is reference to other root ifc, option to include refs must be set (= default)
            && (!$this->isLinkTo() || ($options & self::INCLUDE_LINKTO_IFCS))) // this ref ifc must not be a LINKTO ór option is set to explicitly include linkto ifcs
        {
            $ifc = clone self::getInterface($this->refInterfaceId);
            $ifc->isRoot = false; // interfaces are not considered root interfaces when used by reference
            $ifc->path = "{$this->path}/{$ifc->path}"; // prefix path with current path
            return [ $this->refInterfaceId => self::getInterface($this->refInterfaceId) ];
        }
        else return $this->subInterfaces;
    }
    
    /**
     * @return InterfaceObject[]
     */
    public function getNavInterfacesForTgt(){
        $session = Session::singleton();
        $ifcs = array();
        
        if($this->isLinkTo() && $session->isAccessibleIfc($refIfc = self::getInterface($this->refInterfaceId))) $ifcs[] = $refIfc;
        else $ifcs = $session->getInterfacesToReadConcept($this->tgtConcept);
        
        return $ifcs;
    }
    
    /**
     * Returns query to get target atoms for this interface
     * @param Atom $srcAtom atom to take as source atom for this interface expression query
     * @return string
     */
    public function getQuery($srcAtom){
        $srcAtomId = $this->database->getDBRepresentation($srcAtom);
        if(strpos($this->query, '_SRCATOM') !== false){
            $query = str_replace('_SRCATOM', $srcAtomId, $this->query);
            // $this->logger->debug("#426 Faster query because subquery saved by _SRCATOM placeholder");
        }else{
            $query = "SELECT DISTINCT * FROM ({$this->query}) AS `results` WHERE `src` = '{$srcAtomId}' AND `tgt` IS NOT NULL";
        }
        return $query;
    }
	
/**************************************************************************************************
 *
 * Static InterfaceObject functions
 *
 *************************************************************************************************/
	
    /**
     * Returns if interface exists
     * @var string $ifcId Identifier of interface
     * @return boolean
     */
    public static function interfaceExists($ifcId){
        return array_key_exists($ifcId, self::getAllInterfaces());
    }
    
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
	 * Returns all toplevel interface objects that are public (i.e. not assigned to a role)
	 * @return InterfaceObject[]
	 */
	public static function getPublicInterfaces(){
	    return array_values(array_filter(InterfaceObject::getAllInterfaces(), function($ifc){
            return $ifc->isPublic();
        }));
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
	        $ifc = new InterfaceObject($ifcDef['ifcObject'], null, true);
	        
	        // Set additional information about this toplevel interface object
	        $ifc->ifcRoleNames = $ifcDef['interfaceRoles'];
	        
	        self::$allInterfaces[$ifc->id] = $ifc;
	    }
	}
}

?>