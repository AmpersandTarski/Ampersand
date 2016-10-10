<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;
use stdClass;
use Exception;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 * 
 */
class Resource extends Atom {
    
    const
        /** Default options */
        DEFAULT_OPTIONS     = 0b00000000,
        
        INCLUDE_META_DATA   = 0b00000001,
        
        INCLUDE_NAV_IFCS    = 0b00000010,
        
        INCLUDE_SORT_DATA   = 0b00000100;
        
    /**
     * @var ResourceList $parentList specifies the resource list in which this resource is a tgt atom
     */
    private $parentList = null;
    
    /**
     * @var array|null $qData the row data (from database query) from which this resource is created
     */
    private $qData = null;
    
    /**
     * Label of resource to be displayed in user interfaces
     * @var string
     */
    private $label = null;
    
    /**
     * @var array $data contains the interface data filled by the get() method
     */
    private $data = [];
    
    /**
     * @param string $resourceId Ampersand atom identifier
     * @param string $resourceType Ampersand concept name
     * @param ResourceList $parentList
     */
    public function __construct($resourceId, $resourceType, ResourceList $parentList = null){
        // Set parentList
        $this->parentList = $parentList;
        
        // Get Ampersand concept for this resourceType
        $cpt = Concept::getConceptByLabel($resourceType);
        if(!$cpt->isObject()) throw new Exception ("Cannot instantiate resource given non-object concept {$cpt->name}.");
        
        // Call Atom constructor
        parent::__construct(rawurldecode($resourceId), $cpt); // url decode resource identifier
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
    
    public function jsonSerializable(){
        $content = [];
        
        // Add Ampersand atom attributes
        $content['_id_'] = $this->id;
        $content['_label_'] = $this->getLabel();
        $this->data['_view_'] = $this->getView();
        
        // Merge with interface data (only when get() method is called before)
        return array_merge($content, $this->data);
        
    }
    
    /**
     * @return string
     */
    public function getPath(){
        if(isset($this->parentList)) return $parentList->getPath() . '/' . $this->id;
        else return "/resources/{$this->concept->name}/" . $this->id;
    }
    
    public function getURL(){
        return Config::get('serverURL') . Config::get('apiPath') . $this->getPath();
    }
    
    public function getURI(){
        return Config::get('serverURL') . Config::get('apiPath') . "/resources/{$this->concept->name}/" . $this->id;
    }
    
    /**
     * @param string $ifcId
     * @param string $tgtId
     * @return Resource resource representation of given interface and target atom
     */
    public function one($ifcId, $tgtId){
        $rl = $this->all($ifcId);
        return $rl->one($tgtId);
    }
    
    /**
     * @param string $ifcId
     * @return ResourceList resource list with target atoms of given interface
     */
    public function all($ifcId){
        if(isset($this->parentList)){
            $parentIfc = $this->parentList->getIfc();
            if(!$parentIfc->crudR()) throw new Exception ("Read not allowed for " . $parentIfc->getPath(), 405);
            
            $ifc = $parentIfc->getSubinterface($ifcId);
        }
        else $ifc = InterfaceObject::getInterface($ifcId);
        
        return new ResourceList($this, $ifc);
    }
    
    /**
     * @param string|array $path
     * @param string $returnType
     * @return Resource|ResourceList
     */
    public function walkPath($path, &$returnType = null){
        if(!$this->atomExists()) throw new Exception ("Resource '{$this}' not found", 404);
        
        // Prepare path list
        if(is_array($path)) $path = implode ('/', $path);
        $path = trim($path, '/'); // remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
        if($path == '') return $this; // if no path is specified, return $this (atom)
        $pathList = explode('/', $path);
        
        $r = $this;
        while (count($pathList)){
            switch(get_class($r)){
                case 'Resource' :
                    $r = $r->all(array_shift($pathList));
                    break;
                case 'ResourceList' :
                    $r = $r->one(array_shift($pathList));
                    break;
            }
        }
        
        if(isset($returnType) && $returnType != get_class($r)) throw new Exception ("Provided path results in '" . get_class($r) . "' while '{$returnType}' requested", 500);
        else $returnType = get_class($r);
        
        return $r;
    }

/**************************************************************************************************
 * Methods to call on Resource
 *************************************************************************************************/
 
    /**
     * @param int $rcOptions
     * @param int $ifcOptions
     * @param int $depth
     * @param array $recursionArr
     * @return Resource $this
     */
    public function get($rcOptions = self::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        if(isset($this->parentList)){
            $parentIfc = $this->parentList->getIfc();
            if(!$parentIfc->crudR()) throw new Exception ("Read not allowed for ". $parentIfc->getPath(), 405);
        }
        
        // Meta data
        if($rcOptions & self::INCLUDE_META_DATA) $this->data['_path_'] = rawurlencode($this->getPath());
        
        // Interface(s) to navigate to for this resource
        if(($rcOptions & self::INCLUDE_NAV_IFCS) && isset($parentIfc)){
            $this->data['_ifcs_'] = array_map(function($o) {
                   return array('id' => $o->id, 'label' => $o->label);
            }, $parentIfc->getNavInterfacesForTgt());
        }
        
        // Get content of subinterfaces if depth is not provided or max depth not yet reached
        if(isset($parentIfc) && (is_null($depth) || $depth > 0)) {
            if(!is_null($depth)) $depth--; // decrease depth by 1
            
            // Prevent infinite loops for reference interfaces when no depth is provided
            // We only need to check LINKTO ref interfaces, because cycles may not exists in regular references (enforced by Ampersand generator)
            // If $depth is provided, no check is required, because recursion is finite
            if($parentIfc->isLinkTo() && is_null($depth)){
                if(in_array($this->id, $recursionArr[$parentIfc->getRefToIfcId()])) throw new Exception ("Infinite loop detected for {$this} in " . $parentIfc->getPath(), 500);
                else $recursionArr[$parentIfc->getRefToIfcId()][] = $this->id;
            }
            
            // 
            foreach($parentIfc->getSubinterfaces($ifcOptions) as $subifc){
                if(!$subifc->crudR()) continue; // skip subinterface if not given read rights (otherwise exception will be thrown when getting content)
                    
                // Add content of subifc
                $this->data[$subifc->id] = $subcontent = $this->all($subifc->id)->get($rcOptions, $ifcOptions, $depth, $recursionArr);
                
                // Add sort data if subIfc is univalent
                if($subifc->isUni() && ($rcOptions & self::INCLUDE_SORT_DATA)){
                    $this->data['_sortValues'] = [];
                    
                    // If subifc is PROP (i.e. content is boolean)
                    if($subinteface->isProp()) $this->data['_sortValues_'][$subifc->id] = $subcontent;
                    // Elseif subifc points to object
                    elseif($subifc->tgtConcept->isObject()) $this->data['_sortValues_'][$subifc->id] = current($subcontent)->getLabel(); // use label to sort objects. We can use current() because subifc is univalent
                    // Else scalar
                    else $this->data['_sortValues_'][$subifc->id] = $subcontent;
                }
            }
        }
        
        return $this;
    }
    
    /**
     * Update a resource (updates only first level of subinterfaces, for now)
     * @param stdClass $resourceToPut
     * @return Resource $this
     */
    public function put(stdClass $resourceToPut = null){
        if(!isset($this->parentList)) throw new Exception("Cannot perform put without interface specification", 400);
        if(!isset($resourceToPut)) return $this; // nothing to do
        
        foreach ($resourceToPut as $ifcId => $value){
            if(substr($ifcId, 0, 1) == '_' && substr($ifcId, -1) == '_') continue; // skip special internal attributes
            try{
                $rl = $this->all($ifcId);
            }catch (Exception $e) {
                $this->logger->warning("Skipping unknown subinterface: '{$ifcId}'");
                continue;
            }
            
            $rl->put($value);
        }
        
        return $this;
    }
    
    /**
     * Path this resource with provided patches
     * @param array $patches
     * @return Resource $this
     */
    public function patch(array $patches){
        foreach ($patches as $key => $patch){
            if(!array_key_exists('op', $patch)) throw new Exception ("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
            if(!array_key_exists('path', $patch)) throw new Exception ("No 'path' specfied for patch #{$key}", 400);
        
            // Walk path to lowest level
            $resourceOrList = $this->walkPath($patch['path'], $returnType = null);
            
            // Process patch
            switch($patch['op']){
                case "replace" :
                    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch replace. No 'value' specfied for patch #{$key}", 400);
                    if($returnType != 'ResourceList') throw new Exception ("Cannot patch replace on resource, path must end with an interface");
                    $resourceOrList->replace($patch['value']);
                    break;
                case "add" :
                    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch add. No 'value' specfied for patch #{$key}", 400);
                    if($returnType != 'ResourceList') throw new Exception ("Cannot patch add on resource, path must end with an interface");
                    $resourceOrList->add($patch['value']);
                    break;
                case "remove" :
                    if($returnType != 'Resource') throw new Exception("Cannot patch remove on resource list, path must end with a resource", 400);
                    $resourceOrList->remove();
                    break;
                default :
                    throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
            }
        }
        return $this;
    }
    
    /**
     * Delete this resource and remove as target atom from current interface
     * @return Resource $this
     */
    public function delete(){
        if(!isset($this->parentList)) throw new Exception("Cannot perform delete without interface specification", 400);
        if(!$this->parentList->getIfc()->crudD()) throw new Exception ("Delete not allowed for ". $this->parentList->getIfc()->getPath(), 405);
        
        // Perform delete
        $this->deleteAtom();
        
        return $this;
    }
    
/**************************************************************************************************
 * Redirect for methods to call on ResourceList
 *************************************************************************************************/
    
    /**
     * @param string $ifcId
     * @return array representation of resource content of given interface
     */
    public function getList($ifcId, $rcOptions = Resource::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        return $this->all($ifcId)->get($rcOptions, $ifcOptions, $depth, $recursionArr);
    }
    
    /**
     * Create a new resource as target atom to given interface
     * @param string $ifcId
     * @param stdClass $resourceToPost
     * @return Resource newly created resource
     */
    public function post($ifcId, stdClass $resourceToPost){
        return $this->all($ifcId)->post($resourceToPost);
    }
    
    /**
     * Set provided value for univalent sub interface
     * @param string $ifcId
     * @param string $value (value null is supported)
     * @return boolean
     */
    public function set($ifcId, $value){
        return $this->all($ifcId)->set($value);
    }
    
    /**
     * Set sub interface to null
     * @param string $ifcId
     * @return boolean
     */
    public function unset($ifcId){
        return $this->all($ifcId)->set(null);
    }
    
    /**
     * Add provided value to sub interface
     * @param string $ifcId
     * @param string $value
     * @return boolean
     */
    public function add($ifcId, $value){
        return $this->all($ifcId)->add($value);
    }
    
    /**
     * Remove provided value from sub interface
     * OR remove this resource as from parent list (when no params provided)
     * @param string $ifcId
     * @param string $value
     * @return boolean
     */
    public function remove($ifcId = null, $value = null){
        if(is_null($ifcId)){
            if(!isset($this->parentList)) throw new Exception ("Cannot remove this resource because no parent resource list is provided", 400);
            else return $this->parentList->remove($this); // Remove this resource from the parent list
        }else{
            return $this->all($ifcId)->remove($value); // Remove tgt atom from provided ifc
        }
    }
    
/**************************************************************************************************
 * Helper functions
 *************************************************************************************************/
    
    /**
     * Save query row data (can be used for subinterfaces)
     * @param arry $qData 
     * @return void
     */
    public function setQData($qData){
        $this->qData = $qData;
    }
    
    /**
     * 
     * @param string $colName
     * @throws Exception when column is not defined in query data
     * @return string
     */
    public function getQueryData($colName = null){
        if(is_null($colName)){
            return (array) $this->qData;
        }else{
            // column name is prefixed with 'ifc_' to prevent duplicates with 'src' and 'tgt' cols, which are standard added to query data
            if(!array_key_exists($colName, (array) $this->qData)) throw new Exception("Column '{$colName}' not defined in query data of atom '{$this->__toString()}'", 1001);
            return $this->qData[$colName];
        }
    }
}

?>