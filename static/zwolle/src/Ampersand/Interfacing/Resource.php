<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use stdClass;
use ArrayAccess;
use IteratorAggregate;
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
class Resource extends Atom implements ArrayAccess, IteratorAggregate
{
        
    /**
     * @var ResourceList $parentList specifies the resource list in which this resource is a tgt atom
     */
    protected $parentList = null;
    
    /**
     * Label of resource to be displayed in user interfaces
     * @var string
     */
    protected $label = null;
    
    /**
     * Contains view data of this resource for the UI templates
     * DO NOT initialize var here, isset() is used below
     * @var array $viewData
     */
    protected $viewData;
    
    /**
     * Contains the interface data filled by the get() method
     * @var array|null $ifcData
     */
    protected $ifcData = null;
    
    /**
     * Constructor
     *
     * @param string $resourceId Ampersand atom identifier
     * @param \Ampersand\Core\Concept $cpt
     * @param \Ampersand\Interfacing\ResourceList|null $parentList
     */
    public function __construct(string $resourceId, Concept $cpt, ResourceList $parentList = null)
    {
        // Set parentList
        $this->parentList = $parentList;
        
        // Call Atom constructor
        parent::__construct(rawurldecode($resourceId), $cpt); // url decode resource identifier
    }

    /**
     * Function is called when object is treated as a string
     * This functionality is needed when the ArrayAccess::offsetGet method below is used by internal code
     *
     * @return string
     */
    public function __toString()
    {
        return (string) parent::jsonSerialize();
    }
    
    /**
     * Returns label (from view or atom id) for this atom
     * @return string
     */
    public function getLabel(): string
    {
        if (!isset($this->label)) {
            $viewStr = implode($this->getView());
            $this->label = empty(trim($viewStr)) ? $this->id : $viewStr; // empty view => label = id
        }
        return $this->label;
    }
    
    /**
     * Function is called when object encoded to json with json_encode()
     *
     * @return array|string
     */
    public function jsonSerialize()
    {
        if ($this->concept->isObject()) {
            $content = [];
            
            // Add Ampersand atom attributes
            $content['_id_'] = $this->id;
            $content['_label_'] = $this->getLabel();
            
            // Add view data if array is assoc (i.e. not sequential)
            $data = $this->getView();
            if (!isSequential($data)) {
                $content['_view_'] = $data;
            }
            
            // Merge with inerface data (which is set when get() method is called before)
            if (!is_null($this->ifcData)) {
                return array_merge($content, $this->ifcData);
            }
            
            return $content;
        } else {
            return parent::jsonSerialize();
        }
    }
    
    /**
     * Returns view array of key-value pairs for this atom
     * @return array
     */
    private function getView()
    {
        // If view is not already set
        if (!isset($this->viewData)) {
            if (isset($this->parentList)) {
                $viewDef = $this->parentList->getIfc()->getView(); // if parentList is defined, use view of ifc (can be null)
            } else {
                $viewDef = $this->concept->getDefaultView(); // else use default view of concept (can be null)
            }
            
            if (!is_null($viewDef)) {
                $this->viewData = $viewDef->getViewData($this); // if there is a view definition
            } else {
                $this->viewData = [];
            }
        }
        return $this->viewData;
    }
    
    /**
     * @return string
     */
    public function getPath()
    {
        if (isset($this->parentList)) {
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
                return "session"; // Don't put session id here, this is implicit
            } else {
                return "resource/{$this->concept->name}/" . $this->id;
            }
        }
    }
    
    public function getURL()
    {
        return Config::get('serverURL') . Config::get('apiPath') . "/" . $this->getPath();
    }
    
    public function getURI()
    {
        return Config::get('serverURL') . Config::get('apiPath') . "/resource/{$this->concept->name}/" . $this->id;
    }

    /**
     * Return parent list (i.e. the list of which this Resource is a target resource/atom)
     *
     * @return \Ampersand\Interfacing\ResourceList|null
     */
    public function getParentList()
    {
        return $this->parentList;
    }
    
    /**
     * Return resource representation of given interface and target atom
     *
     * @param string $ifcId
     * @param string $tgtId
     * @return \Ampersand\Interfacing\Resource
     */
    public function one($ifcId, $tgtId): Resource
    {
        $rl = $this->all($ifcId);
        return $rl->one($tgtId);
    }
    
    /**
     * Return resource list with target atoms of given interface
     * For system use of interfaces you can skip the role check by setting the second parameters to true
     *
     * @param string $ifcId
     * @param bool $skipAccessCheck
     * @return \Ampersand\Interfacing\ResourceList
     */
    public function all($ifcId, bool $skipAccessCheck = false): ResourceList
    {
        if (isset($this->parentList)) {
            $ifc = $this->parentList->getIfc()->getSubinterface($ifcId);
        } else {
            $ifc = InterfaceObject::getInterface($ifcId);
        }
        
        return new ResourceList($this, $ifc, $skipAccessCheck);
    }

    /**
     * Walk path from this resource. Path must end/result in a Resource
     * Use Resource::walkPathToResourceList if path must end in a ResourceList
     *
     * @param string|array $path
     * @return \Ampersand\Interfacing\Resource
     */
    public function walkPathToResource($path) : Resource
    {
        $r = $this->walkPath($path);

        // For ident interface expressions the Resource id is left out of the path. Therefore,
        // automatically step into the (only possible) target resource
        if (get_class($r) === 'Ampersand\Interfacing\ResourceList' && $r->getIfc()->isIdent()) {
            $r = $r->one();
        }

        // Check if correct object is returned (Resource vs ResourceList)
        if (get_class($r) === 'Ampersand\Interfacing\Resource') {
            return $r;
        } else {
            throw new Exception("Provided path '{$path}' MUST end with a resource identifier", 400);
        }
    }

    /**
     * Walk path from this resource. Path must end/result in a ResourceList
     * Use Resource::walkPathToResource if path must end in a Resource
     *
     * @param string|array $path
     * @return \Ampersand\Interfacing\ResourceList
     */
    public function walkPathToResourceList($path): ResourceList
    {
        $r = $this->walkPath($path);

        // Check if correct object is returned (Resource vs ResourceList)
        if (get_class($r) === 'Ampersand\Interfacing\ResourceList') {
            return $r;
        } else {
            throw new Exception("Provided path '{$path}' MUST NOT end with a resource identifier", 400);
        }
    }
    
    /**
     * Walk path from this resource to either a Resource or a ResourceList
     *
     * @param string|array $path
     * @return \Ampersand\Interfacing\Resource|\Ampersand\Interfacing\ResourceList
     */
    public function walkPath($path)
    {
        // Prepare path list
        if (is_array($path)) {
            $path = implode('/', $path);
        }
        $path = trim($path, '/'); // remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
        
        if ($path === '') {
            $pathList = []; // support no path
        } else {
            $pathList = explode('/', $path);
        }

        // Check if entry resource ($this) exists
        if (!$this->exists()) {
            if (empty($pathList)) {
                throw new Exception("Resource '{$this}' not found", 404);
            }
            
            $ifc = InterfaceObject::getInterface(reset($pathList));
            
            // Automatically create if allowed
            if ($ifc->crudC() && $ifc->isIdent()) {
                $this->add();
            } else {
                throw new Exception("Resource '{$this}' not found", 404);
            }
        }

        // Walk path by alternating between $r = Resource and $r = ResourceList
        $r = $this; // start with resource ($this)
        while (count($pathList)) {
            switch (get_class($r)) {
                case 'Ampersand\Interfacing\Resource':
                    $r = $r->all(array_shift($pathList));
                    break;
                case 'Ampersand\Interfacing\ResourceList':
                    // See explaination at getPath() function above why this if/else construct is here
                    if ($r->getIfc()->isIdent()) {
                        $r = $r->one();
                    } else {
                        $r = $r->one(array_shift($pathList));
                    }
                    break;
                default:
                    throw new Exception("Unknown class type: " . get_class($r), 500);
            }
        }
        
        // Return
        return $r;
    }

/**************************************************************************************************
 * ArrayAccess methods
 *************************************************************************************************/
    public function offsetExists($offset)
    {
        // Get data (1 level deep) if icfData is not (yet) set
        if (is_null($this->ifcData)) {
            $this->get(Options::INCLUDE_REF_IFCS | Options::INCLUDE_LINKTO_IFCS, 1);
        }
        return isset($this->ifcData[$offset]);
    }

    public function offsetGet($offset)
    {
        // Get data (1 level deep) if icfData is not (yet) set
        if (is_null($this->ifcData)) {
            $this->get(Options::INCLUDE_REF_IFCS | Options::INCLUDE_LINKTO_IFCS, 1);
        }
        if ($this->parentList->getIfc()->getSubinterface($offset)->tgtConcept->isObject()) {
            return $this->ifcData[$offset];
        }
        if ($this->parentList->getIfc()->getSubinterface($offset)->isUni()) {
            return $this->ifcData[$offset]->id ?? null;
        } else {
            return array_map(function(Resource $resource){
                return $resource->id;
            }, $this->ifcData[$offset]);
        }
    }

    public function offsetSet($offset, $value)
    {
        throw new Exception("ArrayAccess::offsetSet() not implemented on Resource class", 500);
    }

    public function offsetUnset($offset)
    {
        throw new Exception("ArrayAccess::offsetUnset() not implemented on Resource class", 500);
    }

/**************************************************************************************************
 * ArrayAccess methods
 *************************************************************************************************/
public function getIterator()
{
    throw new Exception("It is not possible to iterate over a single Resource", 500);
}

/**************************************************************************************************
 * Methods to call on Resource
 *************************************************************************************************/
 
    /**
     * @param int $options
     * @param int|null $depth
     * @param array $recursionArr
     * @return \Ampersand\Interfacing\Resource $this
     */
    public function get(int $options = Options::DEFAULT_OPTIONS, int $depth = null, array $recursionArr = []): Resource
    {
        if (!$this->concept->isObject()) {
            throw new Exception("Cannot get resource, because its type '{$this->concept}' is a non-object concept", 400);
        }

        if (isset($this->parentList)) {
            $parentIfc = $this->parentList->getIfc();
            if (!$parentIfc->crudR()) {
                throw new Exception("Read not allowed for ". $parentIfc->getPath(), 405);
            }
        }
        
        // Meta data
        if ($options & Options::INCLUDE_META_DATA) {
            $this->ifcData['_path_'] = $this->getPath();
        }
        
        // Interface(s) to navigate to for this resource
        if (($options & Options::INCLUDE_NAV_IFCS) && isset($parentIfc)) {
            $this->ifcData['_ifcs_'] = array_map(function ($o) {
                   return ['id' => $o->id, 'label' => $o->label];
            }, $parentIfc->getNavInterfacesForTgt());
        }
        
        // Get content of subinterfaces if depth is not provided or max depth not yet reached
        if (isset($parentIfc) && (is_null($depth) || $depth > 0)) {
            if (!is_null($depth)) {
                $depth--; // decrease depth by 1
            }
            
            // Prevent infinite loops for reference interfaces when no depth is provided
            // We only need to check LINKTO ref interfaces, because cycles may not exist in regular references (enforced by Ampersand generator)
            // If $depth is provided, no check is required, because recursion is finite
            if ($parentIfc->isLinkTo() && is_null($depth)) {
                if (in_array($this->id, $recursionArr[$parentIfc->getRefToIfcId()] ?? [])) {
                    throw new Exception("Infinite loop detected for {$this} in " . $parentIfc->getPath(), 500);
                } else {
                    $recursionArr[$parentIfc->getRefToIfcId()][] = $this->id;
                }
            }
            
            // Init array for sorting in case of sorting boxes (i.e. SCOLS, SHCOLS, SPCOLS)
            $addSortValues = false;
            if (in_array($parentIfc->getBoxClass(), ['SCOLS', 'SHCOLS', 'SPCOLS']) && ($options & Options::INCLUDE_SORT_DATA)) {
                $this->ifcData['_sortValues_'] = [];
                $addSortValues = true;
            }
            
            // Get sub interface data
            foreach ($parentIfc->getSubinterfaces($options) as $subifc) {
                if (!$subifc->crudR()) {
                    continue; // skip subinterface if not given read rights (otherwise exception will be thrown when getting content)
                }
                    
                // Add content of subifc
                $this->ifcData[$subifc->id] = $subcontent = $this->all($subifc->id)->get($options, $depth, $recursionArr);
                
                // Add sort data if subIfc is univalent
                if ($subifc->isUni() && $addSortValues) {
                    // If subifc is PROP (i.e. content is boolean)
                    if (!isset($subcontent)) {
                        $this->ifcData['_sortValues_'][$subifc->id] = null;
                    } elseif ($subifc->isProp()) {
                        $this->ifcData['_sortValues_'][$subifc->id] = $subcontent;
                    } // Elseif subifc points to object
                    elseif ($subifc->tgtConcept->isObject()) {
                        $this->ifcData['_sortValues_'][$subifc->id] = $subcontent->getLabel(); // use label to sort objects. We can use current() because subifc is univalent
                    } // Else scalar
                    else {
                        $this->ifcData['_sortValues_'][$subifc->id] = $subcontent;
                    }
                }
            }
        }
        
        return $this;
    }
    
    /**
     * Update a resource (updates only first level of subinterfaces, for now)
     * @param \stdClass|null $resourceToPut
     * @return \Ampersand\Interfacing\Resource $this
     */
    public function put(stdClass $resourceToPut = null): Resource
    {
        if (!$this->concept->isObject()) {
            throw new Exception("Cannot put resource, because its type '{$this->concept}' is a non-object concept", 400);
        }
        if (!isset($this->parentList)) {
            throw new Exception("Cannot perform put without interface specification", 400);
        }
        if (!isset($resourceToPut)) {
            return $this; // nothing to do
        }
        
        foreach ($resourceToPut as $ifcId => $value) {
            if (substr($ifcId, 0, 1) == '_' && substr($ifcId, -1) == '_') {
                continue; // skip special internal attributes
            }
            try {
                $rl = $this->all($ifcId);
            } catch (Exception $e) {
                throw new Exception("Unknown attribute '{$ifcId}'", 400);
            }
            
            $rl->put($value);
        }
        
        // Clear query data
        $this->setQueryData(null);
        
        return $this;
    }
    
    /**
     * Patch this resource with provided patches
     * Use JSONPatch specification for $patches (see: http://jsonpatch.com/)
     *
     * @param array $patches
     * @return \Ampersand\Interfacing\Resource $this
     */
    public function patch(array $patches): Resource
    {
        if (!$this->concept->isObject()) {
            throw new Exception("Cannot patch resource, because its type '{$this->concept}' is a non-object concept", 400);
        }

        foreach ($patches as $key => $patch) {
            if (!property_exists($patch, 'op')) {
                throw new Exception("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
            }
            if (!property_exists($patch, 'path')) {
                throw new Exception("No 'path' specfied for patch #{$key}", 400);
            }
            
            // Process patch
            switch ($patch->op) {
                case "replace":
                    if (!property_exists($patch, 'value')) {
                        throw new Exception("Cannot patch replace. No 'value' specfied for patch #{$key}", 400);
                    }
                    $this->walkPathToResourceList($patch->path)->replace($patch->value);
                    break;
                case "add":
                    if (!property_exists($patch, 'value')) {
                        throw new Exception("Cannot patch add. No 'value' specfied for patch #{$key}", 400);
                    }
                    $this->walkPathToResourceList($patch->path)->add($patch->value);
                    break;
                case "remove":
                    // Regular json patch remove operation, uses last part of 'path' attribuut as resource to remove from list
                    if (!property_exists($patch, 'value')) {
                        $this->walkPathToResource($patch->path)->remove();
                    
                    // Not part of official json path specification. Uses 'value' attribute that must be removed from list
                    } elseif (property_exists($patch, 'value')) {
                        $this->walkPathToResourceList($patch->path)->remove($patch->value);
                    }
                    break;
                default:
                    throw new Exception("Unknown patch operation '{$patch->op}'. Supported are: 'replace', 'add' and 'remove'", 501);
            }
        }
        
        // Clear query data
        $this->setQueryData(null);
        
        return $this;
    }
    
    /**
     * Delete this resource and remove as target atom from current interface
     * @return \Ampersand\Interfacing\Resource $this
     */
    public function delete(): Resource
    {
        if (!$this->concept->isObject()) {
            throw new Exception("Cannot delete resource, because its type '{$this->concept}' is a non-object concept", 400);
        }

        if (!isset($this->parentList)) {
            throw new Exception("Cannot perform delete without interface specification", 400);
        }
        if (!$this->parentList->getIfc()->crudD()) {
            throw new Exception("Delete not allowed for ". $this->parentList->getIfc()->getPath(), 405);
        }
        
        // Perform delete
        parent::delete();
        
        return $this;
    }
    
/**************************************************************************************************
 * Redirect for methods to call on ResourceList
 *************************************************************************************************/
    
    /**
     * Get representation of resource content given a certain interface
     *
     * @param string $ifcId
     * @param int $options
     * @param int|null $depth
     * @param array $recursionArr
     * @return bool|null|\Ampersand\Interfacing\Resource|\Ampersand\Interfacing\Resource[]
     */
    public function getList(string $ifcId, int $options = Options::DEFAULT_OPTIONS, int $depth = null, array $recursionArr = [])
    {
        return $this->all($ifcId)->get($options, $depth, $recursionArr);
    }
    
    /**
     * Create and return a new resource as target atom to given interface
     *
     * @param string $ifcId
     * @param \stdClass $resourceToPost
     * @return \Ampersand\Interfacing\Resource
     */
    public function post(string $ifcId, stdClass $resourceToPost): Resource
    {
        return $this->all($ifcId)->post($resourceToPost);
    }
    
    /**
     * Set provided value for univalent sub interface
     * @param string $ifcId
     * @param string $value (value null is supported)
     * @return boolean
     */
    public function set($ifcId, $value)
    {
        return $this->all($ifcId)->set($value);
    }
    
    /**
     * Set sub interface to null
     * @param string $ifcId
     * @return boolean
     */
    public function unset($ifcId)
    {
        return $this->all($ifcId)->set(null);
    }
    
    /**
     * Add provided value to sub interface
     * @param string $ifcId
     * @param string $value
     * @return boolean
     */
    public function push($ifcId, $value)
    {
        return $this->all($ifcId)->add($value);
    }
    
    /**
     * Remove provided value from sub interface
     * OR remove this resource as from parent list (when no params provided)
     * @param string $ifcId
     * @param string $value
     * @return boolean
     */
    public function remove($ifcId = null, $value = null)
    {
        if (is_null($ifcId)) {
            if (!isset($this->parentList)) {
                throw new Exception("Cannot remove this resource because no parent resource list is provided", 400);
            } else {
                return $this->parentList->remove($this->id); // Remove this resource from the parent list
            }
        } else {
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
    public static function getAllResources($resourceType)
    {
        $concept = Concept::getConcept($resourceType);
        
        if (!$concept->isObject()) {
            throw new Exception("Cannot get resource(s) given non-object concept {$concept}.", 500);
        }
        
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
    public static function makeResource(string $id, string $conceptName): Resource
    {
        return new Resource($id, Concept::getConcept($conceptName));
    }

    /**
     * Factory function for new resource object
     *
     * @param string $conceptName
     * @return \Ampersand\Interfacing\Resource
     */
    public static function makeNewResource(string $conceptName): Resource
    {
        try {
            $concept = Concept::getConcept($conceptName);
        } catch (Exception $e) {
            throw new Exception("Resource type not found", 404);
        }
        
        if (!$concept->isObject()) {
            throw new Exception("Resource type not found", 404); // Only non-scalar concepts can be used as resource
        }
        if ($concept->isSession()) {
            throw new Exception("Resource type not found", 404); // Prevent users create other sessions
        }
        
        return new Resource($concept->createNewAtomId(), $concept);
    }

    /**
     * Factory function to create a Resource object using an Atom object
     *
     * @param \Ampersand\Core\Atom $atom
     * @return \Ampersand\Interfacing\Resource
     */
    public static function makeResourceFromAtom(Atom $atom): Resource
    {
        return new Resource($atom->id, $atom->concept);
    }
}
