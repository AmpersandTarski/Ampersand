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
use Ampersand\Log\Logger;
use function Ampersand\Misc\isSequential;
use Ampersand\Misc\Config;
use Ampersand\Interfacing\Options;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 * 
 */
class Resource extends Atom {
        
    /**
     * @var ResourceList $parentList specifies the resource list in which this resource is a tgt atom
     */
    private $parentList = null;
    
    /**
     * Label of resource to be displayed in user interfaces
     * @var string
     */
    private $label = null;
    
    /**
     * Contains view data of this resource for the UI templates
     * DO NOT initialize var here, isset() is used below
     * @var array $viewData
     */
    private $viewData;
    
    /**
     * Contains the interface data filled by the get() method
     * @var array $ifcData
     */
    private $ifcData = [];
    
    /**
     * Constructor
     * 
     * @param string $resourceId Ampersand atom identifier
     * @param \Ampersand\Core\Concept $cpt
     * @param \Ampersand\Interfacing\ResourceList $parentList
     */
    public function __construct($resourceId, Concept $cpt, ResourceList $parentList = null){
        // Set parentList
        $this->parentList = $parentList;
        
        if(!$cpt->isObject()) throw new Exception ("Cannot instantiate resource given non-object concept {$cpt->name}.");
        
        // Call Atom constructor
        if(is_null($resourceId)) $resourceId = $cpt->createNewAtomId();
        parent::__construct(rawurldecode($resourceId), $cpt); // url decode resource identifier
        
        $this->logger = Logger::getLogger('INTERFACING');
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
     * Function is called when object encoded to json with json_encode()
     * @return array
     */
    public function jsonSerialize(){
        $content = [];
        
        // Add Ampersand atom attributes
        $content['_id_'] = $this->id;
        $content['_label_'] = $this->getLabel();
        
        // Add view data if array is assoc (i.e. not sequential)
        $data = $this->getView();
        if(!isSequential($data)) $content['_view_'] = $data;
        
        // Merge with interface data (only when get() method is called before)
        return array_merge($content, $this->ifcData);
        
    }
    
    /**
     * Returns view array of key-value pairs for this atom
     * @return array
     */
    private function getView(){
        // If view is not already set
        if(!isset($this->viewData)){
            $this->logger->debug("Get view for atom '{$this}'");
            
            if(isset($this->parentList)) $viewDef = $this->parentList->ifc->getView(); // if parentList is defined, use view of ifc (can be null)
            else $viewDef = $this->concept->getDefaultView(); // else use default view of concept (can be null)
            
            if(!is_null($viewDef)) $this->viewData = $viewDef->getViewData($this); // if there is a view definition
            else $this->viewData = [];
        }
        return $this->viewData;
    }
    
    /**
     * @return string
     */
    public function getPath(){
        if(isset($this->parentList)){
            
            /* Skip resource id for ident interface expressions (I[Concept])
             * I expressions are commonly used for adding structure to an interface using (sub) boxes
             * This results in verbose paths
             * e.g.: pathToApi/resource/Person/John/Person/John/Person details/John/Name
             * By skipping ident expressions the paths are more concise without loosing information
             * e.g.: pathToApi/resource/Person/John/Person/PersonDetails/Name
             */
            if ($this->parentList->getIfc()->isIdent()) {
                return $this->parentList->getPath();
            } else {
                return $this->parentList->getPath() . '/' . $this->id;
            }
        } else {
            if ($this->concept->isSession()) {
                return "resource/session"; // Don't put session id here, this is implicit
            } else {
                return "resource/{$this->concept->name}/" . $this->id;
            }
        }
    }
    
    public function getURL(){
        return Config::get('serverURL') . Config::get('apiPath') . "/" . $this->getPath();
    }
    
    public function getURI(){
        return Config::get('serverURL') . Config::get('apiPath') . "/resource/{$this->concept->name}/" . $this->id;
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
    public function walkPath($path, $returnType = null){
        // Prepare path list
        if(is_array($path)) $path = implode ('/', $path);
        $path = trim($path, '/'); // remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
        $pathList = explode('/', $path);

        // Check if entry resource ($this) exists
        if(!$this->exists()){
            $ifc = InterfaceObject::getInterface(reset($pathList));
            
            // Automatically create if allowed
            if($ifc->crudC() && $ifc->isIdent()) $this->add();
            else throw new Exception ("Resource '{$this}' not found", 404);
        }

        // Walk path by alternating between $r = Resource and $r = ResourceList
        $r = $this; // start with resource ($this)
        while (count($pathList)){
            switch(get_class($r)){
                case 'Ampersand\Interfacing\Resource' :
                    $r = $r->all(array_shift($pathList));
                    break;
                case 'Ampersand\Interfacing\ResourceList' :
                    // See explaination at getPath() function above why this if/else construct is here
                    if($r->getIfc()->isIdent()) $r = $r->one();
                    else $r = $r->one(array_shift($pathList));
                    break;
                default:
                    throw new Exception("Unknown class type: " . get_class($r), 500);
            }
        }
        
        // Check if correct object is returned (Resource vs ResourceList)
        if(isset($returnType) && $returnType != get_class($r)){
            if(get_class($r) == 'Ampersand\Interfacing\ResourceList' && $r->getIfc()->isIdent()) $r = $r->one();
            else throw new Exception ("Provided path results in '" . get_class($r) . "'. This must be '{$returnType}'", 400);
        }
        
        // Return
        return $r;
    }

/**************************************************************************************************
 * Methods to call on Resource
 *************************************************************************************************/
 
    /**
     * @param int $options
     * @param int $depth
     * @param array $recursionArr
     * @return Resource $this
     */
    public function get(int $options = Options::DEFAULT_OPTIONS, int $depth = null, array $recursionArr = []){
        $this->logger->debug("get() called for {$this}");
        if(isset($this->parentList)){
            $parentIfc = $this->parentList->getIfc();
            if(!$parentIfc->crudR()) throw new Exception ("Read not allowed for ". $parentIfc->getPath(), 405);
        }
        
        // Meta data
        if($options & Options::INCLUDE_META_DATA) $this->ifcData['_path_'] = $this->getPath();
        
        // Interface(s) to navigate to for this resource
        if(($options & Options::INCLUDE_NAV_IFCS) && isset($parentIfc)){
            $this->ifcData['_ifcs_'] = array_map(function($o) {
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
            foreach($parentIfc->getSubinterfaces($options) as $subifc){
                if(!$subifc->crudR()) continue; // skip subinterface if not given read rights (otherwise exception will be thrown when getting content)
                    
                // Add content of subifc
                $this->ifcData[$subifc->id] = $subcontent = $this->all($subifc->id)->get($options, $depth, $recursionArr);
                
                // Add sort data if subIfc is univalent
                if($subifc->isUni() && ($options & Options::INCLUDE_SORT_DATA)){
                    $this->ifcData['_sortValues_'] = [];
                    
                    // If subifc is PROP (i.e. content is boolean)
                    if(!isset($subcontent)) $this->ifcData['_sortValues_'][$subifc->id] = null;
                    elseif($subifc->isProp()) $this->ifcData['_sortValues_'][$subifc->id] = $subcontent;
                    // Elseif subifc points to object
                    elseif($subifc->tgtConcept->isObject()) $this->ifcData['_sortValues_'][$subifc->id] = $subcontent->getLabel(); // use label to sort objects. We can use current() because subifc is univalent
                    // Else scalar
                    else $this->ifcData['_sortValues_'][$subifc->id] = $subcontent;
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
        
        // Clear query data
        $this->setQueryData(null);
        
        return $this;
    }
    
    /**
     * Patch this resource with provided patches
     * @param array $patches
     * @return Resource $this
     */
    public function patch(array $patches){
        foreach ($patches as $key => $patch){
            if(!property_exists($patch, 'op')) throw new Exception ("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
            if(!property_exists($patch, 'path')) throw new Exception ("No 'path' specfied for patch #{$key}", 400);
            
            // Process patch
            switch($patch->op){
                case "replace" :
                    if(!property_exists($patch, 'value')) throw new Exception ("Cannot patch replace. No 'value' specfied for patch #{$key}", 400);
                    $this->walkPath($patch->path, 'Ampersand\Interfacing\ResourceList')->replace($patch->value);
                    break;
                case "add" :
                    if(!property_exists($patch, 'value')) throw new Exception ("Cannot patch add. No 'value' specfied for patch #{$key}", 400);
                    $this->walkPath($patch->path, 'Ampersand\Interfacing\ResourceList')->add($patch->value);
                    break;
                case "remove" :
                    $this->walkPath($patch->path, 'Ampersand\Interfacing\Resource')->remove();
                    break;
                default :
                    throw new Exception("Unknown patch operation '{$patch->op}'. Supported are: 'replace', 'add' and 'remove'", 501);
            }
        }
        
        // Clear query data
        $this->setQueryData(null);
        
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
        parent::delete();
        
        return $this;
    }
    
/**************************************************************************************************
 * Redirect for methods to call on ResourceList
 *************************************************************************************************/
    
    /**
     * @param string $ifcId
     * @param int $options
     * @param int $depth
     * @param array $recursionArr
     * @return array representation of resource content of given interface
     */
    public function getList(string $ifcId, int $options = Options::DEFAULT_OPTIONS, int $depth = null, array $recursionArr = []){
        return $this->all($ifcId)->get($options, $options, $depth, $recursionArr);
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
    public function push($ifcId, $value){
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
            else return $this->parentList->remove($this->id); // Remove this resource from the parent list
        }else{
            return $this->all($ifcId)->remove($value); // Remove tgt atom from provided ifc
        }
    }
    
/**********************************************************************************************
 * Static functions
 *********************************************************************************************/
    
    /**
     * Return all resources for a given resourceType
     * TODO: refactor when resources (e.g. for update field in UI) can be requested with interface definition
     * @param string $resourceType name/id of concept
     * @return Resource[]
     */
    public static function getAllResources($resourceType){
        $concept = Concept::getConcept($resourceType);
        
        $resources = [];
        foreach ($concept->getAllAtomObjects() as $atom) {
            $r = new Resource($atom->id, $concept);
            $r->setQueryData($atom->getQueryData());
            $resources[] = $r;
        }
        
        return $resources;
    }

    /**
     * Factory function for Resource class
     *
     * @param string $id
     * @param string $conceptName
     * @return \Ampersand\Interfacing\Resource
     */
    public static function makeResource(string $id, string $conceptName): Resource {
        return new Resource($id, Concept::getConcept($conceptName));
    }

    /**
     * Factory function for new resource object
     *
     * @param string $conceptName
     * @return \Ampersand\Interfacing\Resource
     */
    public static function makeNewResource(string $conceptName): Resource {
        try {
            $concept = Concept::getConcept($conceptName);
        } catch (Exception $e) {
            throw new Exception ("Resource type not found", 404);
        }
        
        if(!$concept->isObject()) throw new Exception ("Resource type not found", 404); // Only non-scalar concepts can be used as resource
        if($concept->isSession()) throw new Exception ("Resource type not found", 404); // Prevent users create other sessions
        
        return new Resource($concept->createNewAtomId(), $concept);
    }

    /**
     * Factory function to create a Resource object using an Atom object
     *
     * @param \Ampersand\Core\Atom $atom
     * @return \Ampersand\Interfacing\Resource
     */
    public static function makeResourceFromAtom(Atom $atom): Resource {
        return new Resource($atom->id, $atom->concept);
    }
}
