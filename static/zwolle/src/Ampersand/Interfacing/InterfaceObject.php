<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Exception;
use Ampersand\Core\Relation;
use Ampersand\Core\Concept;
use Ampersand\Interfacing\View;
use Ampersand\Core\Atom;
use Ampersand\Misc\Config;
use Ampersand\Plugs\IfcPlugInterface;
use Ampersand\Interfacing\Options;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class InterfaceObject {
    
    /**
     * Contains all interface definitions
     * @var \Ampersand\Interfacing\InterfaceObject[]
     */
    private static $allInterfaces; // contains all interface objects
    
    /**
     * Dependency injection of an IfcPlug implementation
     * @var \Ampersand\Plugs\IfcPlugInterface
     */
    private $plug;
    
    /**
     * Interface id (i.e. safe name) to use in framework
     * @var string
     */
    public $id;
    
    /**
     * 
     * @var string
     */
    private $path;
    
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
     * 
     * @var boolean
     */
    private $crudC;
    
    /**
     * 
     * @var boolean
     */
    private $crudR;
    
    /**
     * 
     * @var boolean
     */
    private $crudU;
    
    /**
     * 
     * @var boolean
     */
    private $crudD;
    
    /**
     * 
     * @var \Ampersand\Core\Relation|null
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
    private $isUni;
    
    /**
     * 
     * @var boolean
     */
    private $isTot;
    
    /**
     * 
     * @var boolean
     */
    private $isIdent;
    
    /**
     * 
     * @var string
     */
    private $query;
    
    /**
     * 
     * @var \Ampersand\Core\Concept
     */
    public $srcConcept;
    
    /**
     * 
     * @var \Ampersand\Core\Concept
     */
    public $tgtConcept;
    
    /**
     * 
     * @var \Ampersand\Interfacing\View
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
    private $isLinkTo;
    
    /**
     * 
     * @var \Ampersand\Interfacing\InterfaceObject[]
     */
    private $subInterfaces = array();

    /**
     * InterfaceObject constructor
     * @param array $ifcDef Interface object definition as provided by Ampersand generator
     * @param \Ampersand\Plugs\IfcPlugInterface $plug
     * @param string $pathEntry
     * @param bool $rootIfc Specifies if this interface object is a toplevel interface (true) or subinterface (false)
     */
    private function __construct(array $ifcDef, IfcPlugInterface $plug, string $pathEntry = null, bool $rootIfc = false){
        $this->plug = $plug;
        $this->isRoot = $rootIfc;
        
        // Set attributes from $ifcDef
        $this->id = $ifcDef['id'];
        $this->label = $ifcDef['label'];
        $this->view = is_null($ifcDef['viewId']) ? null : View::getView($ifcDef['viewId']);
        
        $this->path = is_null($pathEntry) ? $this->label : "{$pathEntry}/{$this->label}"; // Use label, because path is only used for human readable purposes (e.g. Exception messages)
        
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
        
        // Subinterfacing
        if(!is_null($ifcDef['subinterfaces'])){
            // Subinterfacing is not supported/possible for tgt concepts with a scalar representation type (i.e. non-objects)
            if(!$this->tgtConcept->isObject()) throw new Exception ("Subinterfacing is not supported for concepts with a scalar representation type (i.e. non-objects). (Sub)Interface '{$this->path}' with target {$this->tgtConcept} (type:{$this->tgtConcept->type}) has subinterfaces specified", 501);
            
            /* Reference to top level interface
             * e.g.:
             * INTERFACE "A" : expr1 INTERFACE "B"
             * INTERFACE "B" : expr2 BOX ["label" : expr3]
             * 
             * is interpreted as:
             * INTERFACE "A" : expr1;epxr2 BOX ["label" : expr3]
             */
            $this->refInterfaceId = $ifcDef['subinterfaces']['refSubInterfaceId'];
            $this->isLinkTo = $ifcDef['subinterfaces']['refIsLinTo']; // not refIsLinkTo? no! typo in generics/interfaces.json
            
            // Inline subinterface definitions
            foreach ((array)$ifcDef['subinterfaces']['ifcObjects'] as $subIfcDef){
                $ifc = new InterfaceObject($subIfcDef, $this->plug, $this->path);
                $this->subInterfaces[$ifc->id] = $ifc;
            }
        }
        
        // CRUD rights
        $this->crudC = $this->isRef() ? null : $ifcDef['crud']['create'];
        $this->crudR = $this->isRef() ? null : $ifcDef['crud']['read'];
        $this->crudU = $this->isRef() ? null : $ifcDef['crud']['update'];
        $this->crudD = $this->isRef() ? null : $ifcDef['crud']['delete'];
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
     * Array with all editable concepts for this interface and all sub interfaces
     * @var Concept[]
     */
    public function getEditableConcepts(){
        $arr = [];
        
        // Determine editable concept for this interface
        if($this->crudU() && $this->tgtConcept->isObject()) $arr[] = $this->tgtConcept;
        
        // Add editable concepts for subinterfaces
        foreach($this->getSubinterfaces(Options::DEFAULT_OPTIONS | Options::INCLUDE_REF_IFCS) as $ifc){
            $arr = array_merge($arr, $ifc->getEditableConcepts());
        }
        
        return $arr;
    }

    /**
     * Returns if interface expression relation is a property
     * @return boolean
     */
    public function isProp(){
        return is_null($this->relation) ? false : ($this->relation->isProp && !$this->isIdent());
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
     * Returns referenced interface object
     * @throws Exception when $this is not a reference interface
     * @return InterfaceObject
     */
    public function getRefToIfc(){
        if($this->isRef()) return self::getInterface($this->refInterfaceId);
        else throw new Exception ("Interface is not a reference interface: " . $this->getPath(), 500); 
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
    
    public function crudC(){
        // If crudC not specified during object construction (e.g. in case of ref interface)
        if(is_null($this->crudC)){
            if($this->isRef()) $this->crudC = $this->getRefToIfc()->crudC();
            else throw new Exception ("Create rights not specified for interface " . $this->getPath(), 500);
        }
        
        return $this->crudC;
    }
    
    public function crudR(){
        // If crudR not specified during object construction (e.g. in case of ref interface)
        if(is_null($this->crudR)){
            if($this->isRef()) $this->crudR = $this->getRefToIfc()->crudR();
            else throw new Exception ("Read rights not specified for interface " . $this->getPath(), 500);
        }
        
        return $this->crudR;
    }
    
    public function crudU(){
        // If crudU not specified during object construction (e.g. in case of ref interface)
        if(is_null($this->crudU)){
            if($this->isRef()) $this->crudU = $this->getRefToIfc()->crudU();
            else throw new Exception ("Read rights not specified for interface " . $this->getPath(), 500);
        }
        
        return $this->crudU;
    }
    public function crudD(){
        // If crudD not specified during object construction (e.g. in case of ref interface)
        if(is_null($this->crudD)){
            if($this->isRef()) $this->crudD = $this->getRefToIfc()->crudD();
            else throw new Exception ("Read rights not specified for interface " . $this->getPath(), 500);
        }
        
        return $this->crudD;
    }

    /**
     * Returns generated query for this interface expression
     * @return string
     */
    public function getQuery(){
        return str_replace('_SESSION', session_id(), $this->query); // Replace _SESSION var with current session id.
    }
    
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
        $arr = [$this];
        foreach ($this->getSubinterfaces(Options::DEFAULT_OPTIONS & ~Options::INCLUDE_REF_IFCS) as $ifc){
            $arr = array_merge($arr, $ifc->getInterfaceFlattened());
        }
        return $arr;
    }
    
    /**
     * @param int $options
     * @return InterfaceObject[] 
     */
    public function getSubinterfaces(int $options = Options::DEFAULT_OPTIONS){
        if($this->isRef() && ($options & Options::INCLUDE_REF_IFCS) // if ifc is reference to other root ifc, option to include refs must be set (= default)
            && (!$this->isLinkTo() || ($options & Options::INCLUDE_LINKTO_IFCS))) // this ref ifc must not be a LINKTO ór option is set to explicitly include linkto ifcs
        {
            /* Return the subinterfaces of the reference interface. This skips the referenced toplevel interface. 
             * e.g.:
             * INTERFACE "A" : expr1 INTERFACE "B"
             * INTERFACE "B" : expr2 BOX ["label" : expr3]
             * 
             * is interpreted as:
             * INTERFACE "A" : expr1;epxr2 BOX ["label" : expr3]
             */
            return self::getInterface($this->refInterfaceId)->getSubinterfaces($options);
        }
        else return $this->subInterfaces;
    }
    
    /**
     * @return InterfaceObject[]
     */
    public function getNavInterfacesForTgt(){
        /** @var \Pimple\Container $container */
        global $container;
        $ifcs = [];
        if($this->isLinkTo() && $container['ampersand_app']->isAccessibleIfc($refIfc = self::getInterface($this->refInterfaceId))) $ifcs[] = $refIfc;
        else $ifcs = $container['ampersand_app']->getInterfacesToReadConcepts([$this->tgtConcept]);
        
        return $ifcs;
    }
    
    /**
     * Returns interface data (tgt atoms) for given src atom
     * @param Atom $srcAtom atom to take as source atom for this interface expression query
     * @return array
     */
    public function getIfcData($srcAtom){
        $data = (array) $this->plug->executeIfcExpression($this, $srcAtom);
        
        // Integrity check
        if($this->isUni() && count($data) > 1) throw new Exception("Univalent (sub)interface returns more than 1 resource: " . $this->getPath(), 500);
        
        return $data;
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
        if(!isset(self::$allInterfaces)) throw new Exception("Interface definitions not loaded yet", 500);
        
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
     * Import all interface object definitions from json file and instantiate InterfaceObject objects
     * 
     * @param string $fileName containing the Ampersand interface definitions
     * @param \Ampersand\Plugs\IfcPlugInterface $defaultPlug
     * @return void
     */
    public static function setAllInterfaces(string $fileName, IfcPlugInterface $defaultPlug){
        self::$allInterfaces = [];
        
        $allInterfaceDefs = (array)json_decode(file_get_contents($fileName), true);
        
        foreach ($allInterfaceDefs as $ifcDef){
            $ifc = new InterfaceObject($ifcDef['ifcObject'], $defaultPlug, null, true);
            
            // Set additional information about this toplevel interface object
            $ifc->ifcRoleNames = $ifcDef['interfaceRoles'];
            
            self::$allInterfaces[$ifc->id] = $ifc;
        }
    }
}
