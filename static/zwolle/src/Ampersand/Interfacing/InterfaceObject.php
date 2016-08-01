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
	 * @param array $ifcDef Interface object definition as provided by Ampersand generator
	 * @param string $pathEntry
     * @param boolean $rootIfc Specifies if this interface object is a toplevel interface (true) or subinterface (false)
	 */
	public function __construct($ifcDef, $pathEntry = null, $rootIfc = false){
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
     * Returns interface relation (when interface expression = relation), throws exception otherwise
     * @return Relation|Exception
     */
	public function relation(){
        if(is_null($this->relation)) throw new Exception ("Interface expression for '{$this->path}' is not a relation", 500);
        else return $this->relation;
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
     * Returns if interface object is a top level interface
     * @return boolean
     */
    public function isRoot(){
        return $this->isRoot;
    }
    
    /**
     * Returns if interface is a public interface (i.e. accessible every role, incl. no role)
	 * @return boolean
     */
    public function isPublic(){
        return empty($this->ifcRoleNames) && $this->isRoot();
    }
    
	/**
	 * 
	 * @param Atom $atom
     * @return void
	 */
	public function setSrcAtom($atom){
	    // Check if atom can be used as source for this interface
	    if(!in_array($atom->concept, $this->srcConcept->getGeneralizationsIncl())) throw new Exception ("Atom '{$atom->__toString()}' does not match source concept '{$this->srcConcept}' or any of its generalizations. Interface path: '{$this->path}'", 500);
	    
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
    
    /**
     * Return array with all sub interface recursively (incl. the interface itself)
     * @return InterfaceObject[]
     */
    public function getInterfaceFlattened(){
        $arr = array();
        $arr[] = $this;
        foreach ($this->getSubinterfaces(false) as $ifc){
            $arr = array_merge($arr, $ifc->getInterfaceFlattened());
        }
        return $arr;
    }
	
    /**
     * @param boolean $inclRefs specifies whether to include subinterfaces from referenced interfaces
     * @return InterfaceObject[] 
     */
	private function getSubinterfaces($inclRefs = true){
	    if(!$this->isRef()) return $this->subInterfaces;
	    elseif($inclRefs) return self::getInterface($this->refInterfaceId)->getSubinterfaces();
        else return array();
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
	    $atom = new Atom($atomId, $this->tgtConcept, $this);
	    
	    // Check if tgtAtom is part of tgtAtoms of interface
        if(array_reduce($this->getTgtAtoms(), function($carry, $tgtAtom) use ($atom){
            return $carry || ($tgtAtom->id == $atom->id);
        }, false)) $this->tgtAtom = $atom;
	    
	    // Check if atom does not exist and if it may be created here
	    elseif(!$atom->atomExists() && $this->crudC){
	        // If interface expression is a relation, add tuple($this->srcAtom, $atom) in this relation
	        if($this->relation) $this->relation()->addLink($this->srcAtom, $atom, $this->relationIsFlipped);
	        // Else only create atom
	        else $atom->addAtom();
	        
	        $this->tgtAtom = $atom;
	    }
	    // Else throw exception
	    else throw new Exception ("Resource '{$atom->__toString()}' not found", 404);
	    
	    return $this->tgtAtom;
	}

/**************************************************************************************************
 *
 * Functions to get content of interface
 *
 *************************************************************************************************/
	
    /**
     * Returns query to get target atoms for this interface
     * @param Atom $srcAtom atom to take as source atom for this interface expression query
     * @return string
     */
    private function getQuery($srcAtom){
        if(strpos($this->query, '_SRCATOM') !== false){
            $query = str_replace('_SRCATOM', $srcAtom->idEsc, $this->query);
            // $this->logger->debug("#426 Faster query because subquery saved by _SRCATOM placeholder");
        }else{
            $query = "SELECT DISTINCT * FROM ({$this->query}) AS `results` WHERE `src` = '{$srcAtom->idEsc}' AND `tgt` IS NOT NULL";
        }
        return $query;
    }
    
	/**
	 * Returns list of target atoms given the srcAtom for this interface
	 * @throws Exception
	 * @return Atom[] [description]
	 */
	public function getTgtAtoms(){
        $tgtAtoms = array();
        try {
            // If interface isIdent (i.e. expr = I[Concept]) we can return the srcAtom
            if($this->isIdent && $this->srcConcept == $this->tgtConcept){
                $tgtAtoms[] = new Atom($this->srcAtom->id, $this->tgtConcept, $this, $this->srcAtom->getQueryData());
                
            // Else try to get tgt atom from srcAtom query data (in case of uni relation in same table)
            }else{
                $tgt = $this->srcAtom->getQueryData('ifc_' . $this->id); // column is prefixed with ifc_
                // $this->logger->debug("#217 One query saved due to reusing data from source atom");
                if(!is_null($tgt)) $tgtAtoms[] = new Atom($tgt, $this->tgtConcept, $this);
            }
        }catch (Exception $e) {
            // Column not defined, perform sub interface query
            if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                $data = (array)$this->database->Exe($this->getQuery($this->srcAtom));
                
                // Integrity check
                if($this->isUni && (count($data) > 1)) throw new Exception("Univalent (sub)interface returns more than 1 resource: '{$this->path}'", 500);
                
                foreach ($data as $row) {
                    $tgtAtoms[] = new Atom($row['tgt'], $this->tgtConcept, $this, $row);
                }
            }else{
                throw $e;
            }
        }
        
        return $tgtAtoms;
    }
	
	/**
	 * Returns the content of this interface given the srcAtom of this interface object
	 * @param array $options
	 * @param array $recursionArr
     * @param int $depth specifies the number subinterface levels to get the content for
	 * @throws Exception
	 * @return mixed
	 */
	public function getContent($options = array(), $recursionArr = array(), $depth = null){
        // Default options
	    $options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
	    $options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
        if(isset($options['depth']) && is_null($depth)) $depth = $options['depth']; // initialize depth, if specified in options array
	    
	    // Initialize result
        if($this->isProp() && empty($this->getSubinterfaces())) $result = false; // leaf properties are false by default (overwritten when tgt atom found)
	    elseif($this->tgtConcept->isObject) $result = array(); // array if tgtConcept is an object, even if result is empty
	    elseif(!$this->isUni) $result = array(); // array for non-univalent interfaces
	    else $result = null; // else (i.e. univalent scalars)
        
        // Loop over target atoms
        foreach ($this->getTgtAtoms() as $tgtAtom){
            
            // Reference to other interface
            if($this->isRef()
                && (!$this->isLinkTo || $options['inclLinktoData'])  // Include content is interface is not LINKTO or inclLinktoData is explicitly requested via the options
                && (!is_null($depth) || !in_array($tgtAtom->id, (array)$recursionArr[$this->refInterfaceId]))){ // Prevent infinite loops
                
                $ifc = $tgtAtom->ifc($this->refInterfaceId);
                
                // Skip ref interface if not given read rights to prevent Exception
    	        if(!$ifc->crudR) break; // breaks foreach loop
                
                foreach($ifc->getTgtAtoms() as $refTgtAtom){
                    
                    // Add target atom to $recursionArr to prevent infinite loops
        	        if($options['inclLinktoData']) $recursionArr[$this->refInterfaceId][] = $refTgtAtom->id;
                    
                    $content = $refTgtAtom->getContent($options, $recursionArr, $depth);
                    
                    // Add target atom to result array
                    switch($options['arrayType']){
                        case 'num' :
                            $result[] = $content;
                            break;
                        case 'assoc' :
                            $result[$refTgtAtom->getJsonRepresentation()] = $content;
                            break;
                        default :
                            throw new Exception ("Unknown arrayType specified: '{$options['arrayType']}'", 500);
                    }
                }
            }else{
	        		
    	        // Object
    	        if($this->tgtConcept->isObject){
    	            // Property leaf: a property at a leaf of a (sub)interface is presented as true/false (false by default, see init above)
    	            if($this->isProp() && empty($this->getSubinterfaces())){
    	                $result = true; // convert into true
    	    
    	            // Regular object, with or without subinterfaces
    	            }else{
    	                $content = $tgtAtom->getContent($options, $recursionArr, $depth);
    	                	
    	                // Add target atom to result array
    	                switch($options['arrayType']){
    	                    case 'num' :
    	                        $result[] = $content;
    	                        break;
    	                    case 'assoc' :
    	                        $result[$tgtAtom->getJsonRepresentation()] = $content;
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
	    }
	    
	    // Return result
	    return $result;
	}
	
/**************************************************************************************************
 *
 * READ, CREATE, UPDATE, PATCH and DELETE functions
 *
 *************************************************************************************************/
    
    /**
    * @param array $options 
    * @throws Exception when read is not allowed for this interface object
    * @return mixed
    */
    public function read($options = []){
        $this->logger->debug("read() called on {$this->path}");
        
        // CRUD check
        if(!$this->crudR) throw new Exception("Read not allowed for '{$this->path}'", 405);
        
        return $this->getContent($options);
    }
    
    /**
	 * Function to create a new Atom at the given interface.
	 * @param array $data
	 * @param array $options
	 * @throws Exception
	 * @return mixed
	 */
	public function create($data, $options = array()){
        $this->logger->debug("create() called on {$this->path}");
        
	    // CRUD check
	    if(!$this->crudC) throw new Exception ("Create not allowed for '{$this->path}'", 405);
	    if(!$this->tgtConcept->isObject) throw new Exception ("Cannot create non-object '{$this->tgtConcept}' in '{$this->path}'. Use PATCH add operation instead", 405);
        if($this->isRef()) throw new Exception ("Cannot create on reference interface in '{$this->path}'. See #498", 501);
	    
	    // Handle options
	    if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
	    // Perform create
	    $newAtom = $this->tgtConcept->createNewAtom();
	
	    // Special case for CREATE in I[Concept] interfaces
	    if($this->srcAtom->id === '_NEW'){
	        $this->srcAtom->setId($newAtom->id);
	        $this->path = str_replace('_NEW', $newAtom->getJsonRepresentation(), $this->path);
	    }
	
	    // If interface expression is a relation, also add tuple(this, newAtom) in this relation
	    if($this->relation) $this->relation()->addLink($this->srcAtom, $newAtom, $this->relationIsFlipped);
        else $newAtom->addAtom();
        
	    // Walk to new atom
	    $newAtom = $this->atom($newAtom->id);
	    
	    // Set requested state (using patches)
	    $patches = is_array($data) ? $data['patches'] : array();
	    $newAtom->doPatches($patches);
	
	    // Special case for file upload. TODO: make extension with hooks
	    if($this->tgtConcept->name == "FileObject"){
	        $conceptFilePath = Concept::getConceptByLabel('FilePath');
            $conceptFileName = Concept::getConceptByLabel('FileName');
            
	        if (is_uploaded_file($_FILES['file']['tmp_name'])){
	            $tmp_name = $_FILES['file']['tmp_name'];
	            $new_name = time() . '_' . $_FILES['file']['name'];
	            $absolutePath = Config::get('absolutePath') . Config::get('uploadPath') . $new_name;
	            $relativePath = Config::get('uploadPath') . $new_name;
	            $result = move_uploaded_file($tmp_name, $absolutePath);
	             
	            if($result) Logger::getUserLogger()->notice("File '{$new_name}' uploaded");
	            else throw new Exception ("Error in file upload", 500);
	
	            // Populate filePath and originalFileName relations in database
	            $relFilePath = Relation::getRelation('filePath', $newAtom->concept, $conceptFilePath);
	            $relOriginalFileName = Relation::getRelation('originalFileName', $newAtom->concept, $conceptFileName);
	            
	            $relFilePath->addLink($newAtom, new Atom($relativePath, $conceptFilePath));
	            $relOriginalFileName->addLink($newAtom, new Atom($_FILES['file']['name'], $conceptFileName));
	
	        }else{
	            throw new Exception ("No file uploaded", 500);
	        }
	    }
	
	    // Close transaction
	    $this->database->closeTransaction($newAtom->concept . ' created', null, $newAtom); // temp store content of $newAtom (also when not crudR)
	
	    // Return atom content (can be null)
	    return $newAtom->getStoredContent();
	}
	
	/**
	 * Function not implemented. Use Atom->update() method instead.
	 * @throws Exception
	 */
	public function update(){
        $this->logger->debug("update() called on {$this->path}");
	    throw new Exception ("Cannot update from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->patch() method instead.
	 * @throws Exception
	 */
	public function patch(){
        $this->logger->debug("patch() called on {$this->path}");
	    throw new Exception ("Cannot patch from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->delete() method instead.
	 * @throws Exception
	 */
	public function delete(){
        $this->logger->debug("delete() called on {$this->path}");
	    throw new Exception ("Cannot delete from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
/**************************************************************************************************
 *
 * Functions to perform patches (on relations): add, replace, remove
 *
 *************************************************************************************************/
	
	/**
	 * Replace (src,tgt) tuple by (src,tgt') in relation provided in this interface
     * @var array $patch
	 * @throws Exception
	 * @return void
	 */
	public function doPatchReplace($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
        if($this->isRef()) throw new Exception ("Cannot update on reference interface in '{$this->path}'. See #498", 501);
	
	    // PatchReplace only works for UNI expressions. Otherwise, use patch remove and patch add
	    if(!$this->isUni) throw new Exception("Cannot patch replace for non-univalent interface '{$this->path}'. Use patch remove + add instead", 500);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch replace. No 'value' specfied for patch with path '{$this->path}'", 400);
	    $value = $patch['value'];
	
	    // Interface is property
	    if($this->isProp()){
	        // Throw error when patch value is something else then true, false or null
	        if(!(is_bool($value) || is_null($value))) throw new Exception("Interface '{$this->path}' is property, boolean expected, non-boolean provided");
	        	
	        // When true
	        if($value) $this->relation()->addLink($this->srcAtom, $this->srcAtom, $this->relationIsFlipped);
	        // When false or null
	        else $this->relation()->deleteLink($this->srcAtom, $this->srcAtom, $this->relationIsFlipped);
	        	
	    // Interface is a relation to an object
	    }elseif($this->tgtConcept->isObject){
	        throw new Exception("Cannot patch replace for object reference in interface '{$this->this}'. Use patch remove + add instead", 500);
	
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConcept->isObject){
	        // Replace by nothing => deleteLink
	        if(is_null($value)) $this->relation()->deleteLink($this->srcAtom, new Atom(null, $this->tgtConcept), $this->relationIsFlipped);
	        // Replace by other atom => addLink
	        else $this->relation()->addLink($this->srcAtom, new Atom($value, $this->tgtConcept), $this->relationIsFlipped);
	        
	    }else{
	        throw new Exception ("Unknown patch replace. Please contact the application administrator", 500);
	    }
	}
	
	/**
	 * Add (src,tgt) tuple in relation provided in this interface
     * @var array $patch
	 * @throws Exception
	 * @return void
	 */
	public function doPatchAdd($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
        if($this->isRef()) throw new Exception ("Cannot update on reference interface in '{$this->path}'. See #498", 501);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch add. No 'value' specfied in '{$this->path}'", 400);
	    
	    $tgtAtom = new Atom($patch['value'], $this->tgtConcept);
	    
	    // Interface is property
	    if($this->isProp()){
	        // Properties must be treated as a 'replace', so not handled here
	        throw new Exception("Cannot patch add for property '{$this->path}'. Use patch replace instead", 500);
	
	    // Interface is a relation to an object
	    }elseif($this->tgtConcept->isObject){
	        // Check if atom exists and may be created (crudC)
	        if(!$tgtAtom->atomExists()){
                if($this->crudC) $tgtAtom->addAtom();
                else throw new Exception ("Resource '{$tgtAtom->__toString()}' does not exist and may not be created in {$this->path}", 403);
            }
	        
            // Add link when possible (relation is specified)	
	        if(is_null($this->relation)) $this->logger->debug("addLink skipped because '{$this->path}' is not an editable expression");
            else $this->relation()->addLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
            
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConcept->isObject){    	
	        // Check: If interface is univalent, throw exception
	        if($this->isUni) throw new Exception("Cannot patch add for univalent interface {$this->path}. Use patch replace instead", 500);
	        	
	        $this->relation()->addLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
	        
	    }else{
	        throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
	    }
	}
	
    /**
     * Remove (src,tgt) tuple from relation provided in $this->parentIfc
     * @var array $patch
     * @throws Exception
     * @return void
     */
	public function doPatchRemove($patch){	   
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
        if($this->isRef()) throw new Exception ("Cannot update on reference interface in '{$this->path}'. See #498", 501);
	    
        // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch remove. No 'value' specfied in '{$this->path}'", 400);
        
        $tgtAtom = new Atom($patch['value'], $this->tgtConcept);
        
		// Interface is property
		if($this->isProp()){
			// Properties must be treated as a 'replace', so not handled here
			throw new Exception("Cannot patch remove for property '{$this->path}'. Use patch replace instead", 500);
		
		// Interface is a relation to an object
        }elseif($this->tgtConcept->isObject){
			
			$this->relation()->deleteLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
		
		// Interface is a relation to a scalar (i.e. not an object)
        }elseif(!$this->tgtConcept->isObject){
			if($this->isUni) throw new Exception("Cannot patch remove for univalent interface {$this->path}. Use patch replace instead", 500);
			
			$this->relation()->deleteLink($this->srcAtom, $tgtAtom, $this->relationIsFlipped);
			
		}else{
			throw new Exception ("Unknown patch remove. Please contact the application administrator", 500);
		}
		
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