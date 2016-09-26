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
     * @var InterfaceObject $parentIfc specifies the interface from which this atom is instantiated
     */
    private $parentIfc = null;
    
    /**
     * @var Resource $parentResource
     */
    private $parentResource = null;
    
    /**
     * @var string url to this resource (i.e. <serverUrl>/<apiPath>/resource/<resourceType>/<resourceId>/<ifc>/<resource>/<etc>)
     */
    private $url = null;
    
    private $uri = null;
    
    private $path = null;
    
    /**
     * @var array|null $qData the row data (from database query) from which this resource is created
     */
    private $qData = null;
    
    /**
     * @param string $resourceId Ampersand atom identifier
     * @param string $resourceType Ampersand concept name
     * @param InterfaceObject $parentIfc
     * @param Resource $parentResource
     */
    public function __construct($resourceId, $resourceType, InterfaceObject $parentIfc = null, Resource $parentResource = null){
        if(isset($parentResource) && !isset($parentIfc)) throw new Exception ("Parent interface must be specified when parent resource is provided", 500);
        
        $this->parentIfc = $parentIfc;
        $this->parentResource = $parentResource;
        
        // Get Ampersand concept for this resourceType
        $cpt = Concept::getConceptByLabel($resourceType);
        if(!$cpt->isObject()) throw new Exception ("Cannot instantiate resource given non-object concept {$cpt->name}.");
        
        parent::__construct(rawurldecode($resourceId), $cpt); // url decode resource identifier
        
        // Set path
        if(isset($parentResource)) $this->path .= '/' . $parentResource->path; // start path with path of parent resource (if specified)
        if(isset($parentIfc)) $this->path .= '/' . $parentIfc->label; // add label of parent interface (if specified) TODO: what if only parentIfc is specified??
        if(!isset($this->path)) $this->path = "/resources/{$this->concept->name}"; // case when path is still empty (i.e. no parent resource nor ifc)
        $this->path .= '/' . $this->getJsonRepresentation(); // add resource identifier to end of path
        
        // Set URL and URI
        $this->url = Config::get('serverURL') . Config::get('apiPath') . $this->path;
        $this->uri = Config::get('serverURL') . Config::get('apiPath') . "/resources/{$this->concept->name}/" . $this->getJsonRepresentation();
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
        if(isset($this->parentIfc)){
            if(!$this->parentIfc->crudR()) throw new Exception ("Read not allowed for " . $this->parentIfc->path(), 403);
            
            $ifc = $this->parentIfc->getSubinterface($ifcId);
        }
        else $ifc = InterfaceObject::getInterface($ifcId);
        
        return new ResourceList($this, $ifc);
        
    }
    
    /**
     * @param int $rcOptions
     * @param int $ifcOptions
     * @param int $depth
     * @param array $recursionArr
     * @return stdClass representation of resource content of current interface
     */
    public function get($rcOptions = self::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        if(isset($this->parentIfc) && !$this->parentIfc->crudR()) throw new Exception ("Read not allowed for ". $this->parentIfc->path(), 403);
        $content = new stdClass();
        
        $content->_id_ = $this->getJsonRepresentation();
        $content->_label_ = $this->getLabel();
        $content->_view_ = $this->getView();
         
        // Meta data
        if($rcOptions & self::INCLUDE_META_DATA) $content->_path_ = $this->path;
        
        // Interface(s) to navigate to for this resource
        if($rcOptions & self::INCLUDE_NAV_IFCS){
            $content->_ifcs_ = array_map(function($o) {
                   return array('id' => $o->id, 'label' => $o->label);
            }, $this->parentIfc->getNavInterfacesForTgt());
        }
        
        // Get content of subinterfaces if depth is not provided or max depth not yet reached
        if(isset($this->parentIfc) && (is_null($depth) || $depth > 0)) {
            if(!is_null($depth)) $depth--; // decrease depth by 1
            
            // Prevent infinite loops for reference interfaces when no depth is provided
            // We only need to check LINKTO ref interfaces, because cycles may not exists in regular references (enforced by Ampersand generator)
            // If $depth is provided, no check is required, because recursion is finite
            if($this->parentIfc->isLinkTo() && is_null($depth)){
                if(in_array($this->id, $recursionArr[$this->parentIfc->getRefToIfcId()])) throw new Exception ("Infinite loop detected for {$this} in " . $this->parentIfc->path(), 500);
                else $recursionArr[$this->parentIfc->getRefToIfcId()][] = $this->id;
            }
            
            // 
            foreach($this->parentIfc->getSubinterfaces($ifcOptions) as $subifc){
                if(!$subifc->crudR()) continue; // skip subinterface if not given read rights (otherwise exception will be thrown when getting content)
                    
                // Add content of subifc
                $content->{$subifc->id} = $subcontent = $this->all($subifc->id)->getList($rcOptions, $ifcOptions, $depth, $recursionArr);
                
                // Add sort data if subIfc is univalent
                if($subifc->isUni() && ($rcOptions & self::INCLUDE_SORT_DATA)){
                    // If subifc is PROP (i.e. content is boolean)
                    if($subinteface->isProp()) $content->_sortValues_[$subifc->id] = $subcontent;
                    // Elseif subifc points to object
                    elseif($subifc->tgtConcept->isObject()) $content->_sortValues_[$subifc->id] = current($subcontent)->_label_; // use label to sort objects. We can use current() because subifc is univalent
                    // Else scalar
                    else $content->_sortValues_[$subifc->id] = $subcontent;
                }
            }
        }
        
        return $content;
    }
    
    /**
     * @param string $ifcId
     * @return array representation of resource content of given interface
     */
    public function getList($ifcId, $rcOptions = Resource::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        return $this->all($ifcId)->getList($rcOptions, $ifcOptions, $depth, $recursionArr);
    }
    
    /**
     * @return ??
     */
    public function put(){
        
    }
    
    /**
     * Create a new resource and add as target atom to given interface
     * @param string $ifcId
     * @return stdClass representation newly created resource
     */
    public function post($ifcId){
        
    }
    
    /**
     * Path this resource with provided patches
     * @param array $patches
     * @return stdClass representation updated resource
     */
    public function patch($patches){
        
    }
    
    /**
     * Remove this resource as target atom from current interface
     * @return boolean
     */
    public function remove(){
        
    }
    
    /**
     * Delete this resource and remove as target atom from current interface
     * @return boolean
     */
    public function delete(){
        
    }
    
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