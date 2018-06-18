<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use stdClass;
use Exception;
use ArrayIterator;
use IteratorAggregate;
use Ampersand\Core\Atom;
use Ampersand\Log\Logger;
use Ampersand\Interfacing\Options;
use Ampersand\Interfacing\Resource;
use Ampersand\Interfacing\InterfaceObject;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class ResourceList implements IteratorAggregate
{
    
    /**
    *
    * @var \Psr\Log\LoggerInterface
    */
    protected $logger;
    
    /**
     * The source Resource (i.e. Atom) of this resource list
     *
     * @var \Ampersand\Interfacing\Resource
     */
    protected $src = null;
    
    /**
     * The Interface that is the base of this resource list
     *
     * @var \Ampersand\Interfacing\InterfaceObject
     */
    protected $ifc = null;
    
    /**
     * List with target resources
     *
     * @var \Ampersand\Interfacing\Resource[]
     */
    protected $tgts = null;
    
    /**
     * Constructor
     *
     * @param \Ampersand\Interfacing\Resource $src
     * @param \Ampersand\Interfacing\InterfaceObject $ifc
     * @param bool $skipAccessCheck
     */
    public function __construct(Resource $src, InterfaceObject $ifc, bool $skipAccessCheck = false)
    {
        /** @var \Pimple\Container $container */
        global $container; // TODO: remove dependency on global $container var
        $this->logger = Logger::getLogger('INTERFACING');
        
        if ($ifc->isRoot() && !$container['ampersand_app']->isAccessibleIfc($ifc) && !$skipAccessCheck) {
            throw new Exception("Unauthorized to access interface {$ifc->label}", 403);
        }
        
        // Epsilon. TODO: remove after multiple concept specifications are possible for Atom objects
        if ($src->concept !== $ifc->srcConcept) {
            $this->src = new Resource($src->id, $ifc->srcConcept, $src->getParentList());
        // No epsilon
        } else {
            $this->src = $src;
        }
        
        $this->ifc = $ifc;
    }
    
    /**
     * @return \ArrayIterator
     */
    public function getIterator(): ArrayIterator
    {
        return new ArrayIterator($this->getTgtResources());
    }
    
    /**
     * Get source resource of this list
     *
     * @return \Ampersand\Interfacing\Resource
     */
    public function getSrc(): Resource
    {
        return $this->src;
    }
    
    /**
     * @return string
     */
    public function getPath(): string
    {
        return $this->src->getPath() . '/' . $this->ifc->id;
    }
    
    /**
     * @return \Ampersand\Interfacing\InterfaceObject
     */
    public function getIfc(): InterfaceObject
    {
        return $this->ifc;
    }
    
    /**
     * @param string $tgtId
     * @return \Ampersand\Interfacing\Resource
     */
    public function one($tgtId = null): Resource
    {
        if (!$this->ifc->crudR()) {
            throw new Exception("Read not allowed for " . $this->ifc->getPath(), 405);
        }

        // If no tgtId is provided, the srcId is used. Usefull for ident interface expressions (I[Concept])
        if (is_null($tgtId)) {
            $tgtId = $this->src->id;
        }
        
        foreach ($this->getTgtResources() as $resource) {
            if ($resource->id == $tgtId) {
                return $resource;
            }
        }

        // Create the target atom if allowed
        if ($this->ifc->tgtConcept->isObject() && $this->ifc->crudC()) {
            return $this->makeResource($tgtId);
        }
        
        // When not found
        throw new Exception("Resource not found", 404);
    }
    
    /**
     * Return list of target resources
     *
     * @param bool $fromCache specifies if target resources may be get from cache (true) or recalculated (false)
     * @return \Ampersand\Interfacing\Resource[]
     */
    protected function getTgtResources(bool $fromCache = true): array
    {
        if (!isset($this->tgts) || !$fromCache) {
            $this->tgts = [];
            // If interface isIdent (i.e. expr = I[Concept]), and no epsilon is required (i.e. srcConcept equals tgtConcept of parent ifc) we can return the src
            if ($this->ifc->isIdent() && (($this->ifc->srcConcept === $this->ifc->getParentInterface()->tgtConcept) ?? false)) {
                $this->tgts[] = $this->makeResource($this->src->id);
                
            // Else try to get tgt atom from src query data (in case of uni relation in same table)
            } else {
                $tgtId = $this->src->getQueryData('ifc_' . $this->ifc->id, $exists); // column is prefixed with ifc_ in query data
                if ($exists) {
                    if (!is_null($tgtId)) {
                        $this->tgts[] = $this->makeResource($tgtId);
                    }
                } else {
                    foreach ($this->ifc->getIfcData($this->src) as $row) {
                        $r = $this->makeResource($row['tgt']);
                        $r->setQueryData($row);
                        $this->tgts[] = $r;
                    }
                }
            }
        }
        
        return $this->tgts;
    }

    /**
     * Resource factory. Instantiates a new target resource
     *
     * @param string $tgtId
     * @return \Ampersand\Interfacing\Resource
     */
    protected function makeResource(string $tgtId): Resource
    {
        return new Resource($tgtId, $this->ifc->tgtConcept, $this);
    }

    /**
     * Resource factory. Instantiates a new target resource with a new (random) id
     *
     * @return \Ampersand\Interfacing\Resource
     */
    protected function makeNewResource(): Resource
    {
        $cpt = $this->ifc->tgtConcept;
        return new Resource($cpt->createNewAtomId(), $cpt, $this);
    }

/**************************************************************************************************
 * Methods to call on ResourceList
 *************************************************************************************************/
     
    /**
     * @param int $options
     * @param int|null $depth
     * @param array $recursionArr
     * @return bool|null|\Ampersand\Interfacing\Resource|\Ampersand\Interfacing\Resource[]
     */
    public function get($options = Options::DEFAULT_OPTIONS, int $depth = null, $recursionArr = [])
    {
        $this->logger->debug("get() called for {$this->src} / {$this->ifc}");
        if (!$this->ifc->crudR()) {
            throw new Exception("Read not allowed for ". $this->ifc->getPath(), 405);
        }

        // The following check is needed because the frontend UI does not support root interfaces expressions with non-object target concepts (yet)
        // Subinterfaces are not a problem
        if ($this->ifc->isRoot() && !$this->ifc->tgtConcept->isObject()) {
            throw new Exception("No support for root interface expressions with non-object target concept (see #745)", 501);
        }
        
        // Initialize result
        $result = [];
        
        // Object nodes
        if ($this->ifc->tgtConcept->isObject()) {
            foreach ($this->getTgtResources() as $resource) {
                $result[] = $resource->get($options, $depth, $recursionArr); // for json_encode $resource->jsonSerializable() is called
            }
            
            // Special case for leave PROP: return false when result is empty, otherwise true (i.e. I atom must be present)
            // Enables boolean functionality for editing ampersand property relations
            if ($this->ifc->isLeaf() && $this->ifc->isProp()) {
                if (empty($result)) {
                    return false;
                } else {
                    return true;
                }
            }
            
        // Non-object nodes (i.e. leaves, because subinterfaces are not allowed for non-objects)
        // Notice that ->get() is not called on $resource. The interface stops here.
        } else {
            foreach ($this->getTgtResources() as $resource) {
                $result[] = $resource; // for json_encode $resource->jsonSerializable() is called
            }
        }
        
        // Return result using UNI-aspect (univalent-> value/object, non-univalent -> list of values/objects)
        if ($this->ifc->isUni() && empty($result)) {
            return null;
        } elseif ($this->ifc->isUni()) {
            return current($result);
        } else {
            return $result;
        }
    }
    
    /**
     * @param \stdClass $resourceToPost
     * @return \Ampersand\Interfacing\Resource
     */
    public function post(stdClass $resourceToPost): Resource
    {
        if (!$this->ifc->crudC()) {
            throw new Exception("Create not allowed for ". $this->ifc->getPath(), 405);
        }
        
        // Use attribute '_id_' if provided
        if (isset($resourceToPost->_id_)) {
            $resource = $this->makeResource($resourceToPost->_id_);
            if ($resource->exists()) {
                throw new Exception("Cannot create resource that already exists", 400);
            }
        } elseif ($this->ifc->isIdent()) {
            $resource = $this->makeResource($this->src->id);
        } else {
            $resource = $this->makeNewResource();
        }
        
        // If interface is editable, also add tuple(src, tgt) in interface relation
        if ($this->ifc->isEditable()) {
            $this->add($resource->id, true);
        } else {
            $resource->add();
        }
        
        // Put resource attributes
        $resource->put($resourceToPost);
        
        return $resource;
    }
    
    /**
     * Update a complete resource list (updates only this subinterface, not any level(s) deeper for now)
     * @param mixed $value
     * @return bool
     */
    public function put($value): bool
    {
        
        if ($this->ifc->isUni()) { // expect value to be object or literal
            if (is_array($value)) {
                throw new Exception("Non-array expected but array provided while updating " . $this->ifc->getPath(), 400);
            }
            
            if ($this->ifc->tgtConcept->isObject()) {
                if (is_null($value) || is_string($value)) { // null object or string
                    $this->set($value);
                } elseif (isset($value->_id_)) { // object with _id_ attribute
                    $this->set($value->_id_);
                } else {
                    throw new Exception("Cannot identify provided object while updating " . $this->ifc->getPath(), 400);
                }
            } else { // expect value to be literal (i.e. non-object) or null
                $this->set($value);
            }
        } else { // expect value to be array
            if (!is_array($value)) {
                throw new Exception("Array expected but not provided while updating " . $this->ifc->getPath(), 400);
            }
            
            // First empty existing list
            $this->removeAll();
            
            // Add provided values
            foreach ($value as $item) {
                if ($this->ifc->tgtConcept->isObject()) { // expect item to be object
                    if (!is_object($item)) {
                        throw new Exception("Object expected but " . gettype($item) . " provided while updating " . $this->ifc->getPath(), 400);
                    }
                    
                    if (is_string($item)) { // string
                        $this->add($item);
                    } elseif (isset($item->_id_)) { // object with _id_ attribute
                        $this->add($item->_id_);
                    } else {
                        throw new Exception("Cannot identify provided object while updating " . $this->ifc->getPath(), 400);
                    }
                } else { // expect item to be literal (i.e. non-object) or null
                    $this->add($item);
                }
            }
        }
        
        return true;
    }
    
    /**
     * Alias of set() method. Used by Resource::patch() method
     * @param mixed|null $value
     * @return bool
     */
    public function replace($value = null): bool
    {
        if (!$this->ifc->isUni()) {
            throw new Exception("Cannot use replace for non-univalent interface " . $this->ifc->getPath() . ". Use add or remove instead", 400);
        }
        return $this->set($value);
    }
    
    /**
     * Set provided value (for univalent interfaces)
     *
     * @param mixed|null $value
     * @return bool
     */
    public function set($value = null): bool
    {
        if (!$this->ifc->isUni()) {
            throw new Exception("Cannot use set() for non-univalent interface " . $this->ifc->getPath() . ". Use add or remove instead", 400);
        }
        
        // Handle Ampersand properties [PROP]
        if ($this->ifc->isProp()) {
            if ($value === true) {
                $this->add($this->src->id);
            } elseif ($value === false) {
                $this->remove($this->src->id);
            } else {
                throw new Exception("Boolean expected, non-boolean provided.", 400);
            }
        } else {
            if (is_null($value)) {
                $this->removeAll();
            } else {
                $this->add($value);
            }
        }
        
        return true;
    }
    
    /**
     * Add value to resource list
     * @param mixed $value
     * @param bool $skipCrudUCheck
     * @return bool
     */
    public function add($value, bool $skipCrudUCheck = false): bool
    {
        if (!isset($value)) {
            throw new Exception("Cannot add item. Value not provided", 400);
        }
        if (is_object($value) || is_array($value)) {
            throw new Exception("Literal expected but " . gettype($value) . " provided while updating " . $this->ifc->getPath(), 400);
        }
        
        if (!$this->ifc->isEditable()) {
            throw new Exception("Interface is not editable " . $this->ifc->getPath(), 405);
        }
        if (!$this->ifc->crudU() && !$skipCrudUCheck) {
            throw new Exception("Update not allowed for " . $this->ifc->getPath(), 405);
        }
        
        $tgt = new Atom($value, $this->ifc->tgtConcept);
        if ($tgt->concept->isObject() && !$this->ifc->crudC() && !$tgt->exists()) {
            throw new Exception("Create not allowed for " . $this->ifc->getPath(), 405);
        }
        
        $tgt->add();
        $this->src->link($tgt, $this->ifc->relation(), $this->ifc->relationIsFlipped)->add();
        
        return true;
    }
    
    /**
     * Remove value from resource list
     *
     * @param mixed $value
     * @return bool
     */
    public function remove($value): bool
    {
        if (!isset($value)) {
            throw new Exception("Cannot remove item. Value not provided", 400);
        }
        if (is_object($value) || is_array($value)) {
            throw new Exception("Literal expected but " . gettype($value) . " provided while updating " . $this->ifc->getPath(), 400);
        }
        
        if (!$this->ifc->isEditable()) {
            throw new Exception("Interface is not editable " . $this->ifc->getPath(), 405);
        }
        if (!$this->ifc->crudU()) {
            throw new Exception("Update not allowed for " . $this->ifc->getPath(), 405);
        }
        
        $tgt = new Atom($value, $this->ifc->tgtConcept);
        $this->src->link($tgt, $this->ifc->relation(), $this->ifc->relationIsFlipped)->delete();
        
        return true;
    }
    
    /**
     * Undocumented function
     *
     * @return bool
     */
    public function removeAll(): bool
    {
        if (!$this->ifc->isEditable()) {
            throw new Exception("Interface is not editable " . $this->ifc->getPath(), 405);
        }
        if (!$this->ifc->crudU()) {
            throw new Exception("Update not allowed for " . $this->ifc->getPath(), 405);
        }
        
        foreach ($this->getTgtResources() as $tgt) {
            $this->src->link($tgt, $this->ifc->relation(), $this->ifc->relationIsFlipped)->delete();
        }

        return true;
    }
}
