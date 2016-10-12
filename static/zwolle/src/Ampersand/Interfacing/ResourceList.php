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
use Ampersand\Session;
use Ampersand\Config;
use Ampersand\Core\Atom;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class ResourceList implements IteratorAggregate {
    
    /**
    *
    * @var \Psr\Log\LoggerInterface
    */
    protected $logger;
    
    /**
     * @var Resource $src source of resource list
     */
    private $src = null;
    
    /**
     * @var InterfaceObject $parentIfc [description]
     */
    private $parentIfc = null;
    
    /**
     * @var array $tgtResources list with target resources
     */
    private $tgtResources = null;
    
    
    public function __construct(Resource $src, InterfaceObject $parentIfc){
        $session = Session::singleton();
        $this->logger = Logger::getLogger('INTERFACING');
        
        if($parentIfc->isRoot() && !$session->isAccessibleIfc($parentIfc)) throw new Exception("Unauthorized to access interface {$parentIfc->label}", 401); // 401: Unauthorized
        
        $this->src = $src;
        $this->ifc = $parentIfc;
    }
    
    /**
     * @return ArrayIterator
     */
    public function getIterator(){
        if($this->ifc->tgtConcept->isObject()) return new ArrayIterator($this->getTgtResources());
        else return new ArrayIterator($this->getTgtAtoms());
    }
    
    /**
     * @return string
     */
    public function getPath(){
        return $this->src->getPath() . '/' . $this->ifc->label;
    }
    
    /**
     * @return InterfaceObject
     */
    public function getIfc(){
        return $this->ifc;
    }
    
    /**
     * @param string $tgtId
     * @return Resource
     */
    public function one($tgtId){
        $arr = $this->getTgtResources();
        
        // Functionality to automatically add/create resource if allowed
        if(!array_key_exists($tgtId, $arr)){
            $resource = new Resource($tgtId, $this->ifc->tgtConcept->name);
            
            // If resource already exists and may be added (crudU)
            if($this->ifc->crudU() && $resource->exists()) $this->add($resource->id);
            
            // Elseif resource not yet exists and may be created (crudC) 
            elseif($this->ifc->crudC() && !$resource->exists()){
                $obj = new stdClass();
                $obj->_id_ = $resource->id;
                $this->post($obj);
            }
            
            // Else: return not found
            else throw new Exception ("Resource '{$resource}' not found", 404);
            
            // Reevaluate interface expression, tgt should now be there, otherwise throw exception
            if(!array_key_exists($tgtId, $arr = $this->getTgtResources(false))) throw new Exception ("Oeps.. something went wrong", 500);
        }
        
        return $arr[$tgtId];
    }
    
    /**
     * @param boolean $fromCache specifies if target resources may be get from cache (true) or recalculated (false)
     * @return Resource[]
     */
    private function getTgtResources($fromCache = true){
        if(!isset($this->tgtResources) || !$fromCache){
            $this->tgtResources = [];
            try {
                // If interface isIdent (i.e. expr = I[Concept]) we can return the src
                if($this->ifc->isIdent()){
                    $this->tgtResources[$this->src->id] = new Resource($this->src->id, $this->ifc->tgtConcept->name, $this);
                    
                // Else try to get tgt atom from src query data (in case of uni relation in same table)
                }else{
                    $tgt = $this->src->getQueryData('ifc_' . $this->ifc->id); // column is prefixed with ifc_ in query data
                    if(!is_null($tgt)) $this->tgtResources[$tgt] = new Resource($tgt, $this->ifc->tgtConcept->name, $this);
                }
            }catch (Exception $e) {
                // Column not defined, perform sub interface query
                if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                    $db = Database::singleton();
                    $data = (array) $db->Exe($this->ifc->getQuery($this->src));
                    
                    // Integrity check
                    if($this->ifc->isUni() && count($data) > 1) throw new Exception("Univalent (sub)interface returns more than 1 resource: " . $this->ifc->getPath(), 500);
                    
                    foreach ($data as $row) {
                        $r = new Resource($row['tgt'], $this->ifc->tgtConcept->name, $this);
                        $r->setQueryData($row);
                        $this->tgtResources[$r->id] = $r;
                    }
                }else{
                    throw $e;
                }
            }
        }
        
        return $this->tgtResources;
    }
    
    
    /**
     * Codes below look similar to getTgtResources() function above, but returns list of Atoms instead of Resources
     * @return Atom[]
     */
    private function getTgtAtoms($fromCache = true){
        if(isset($this->tgtResources) && $fromCache) return $this->tgtResources;
        elseif($this->ifc->tgtConcept->isObject()) return $this->getTgtResources($fromCache);
        
        // Otherwise (i.e. non-object atoms) get atoms from database. This is never cached. We only cache resources (i.e. object atoms)
        else{
            $tgtAtoms = [];
            try {
                // Try to get tgt atom from src query data (in case of uni relation in same table)
                $tgt = $this->src->getQueryData('ifc_' . $this->ifc->id); // column is prefixed with ifc_ in query data
                if(!is_null($tgt)) $tgtAtoms[] = new Atom($tgt, $this->ifc->tgtConcept);
                
            }catch (Exception $e) {
                // Column not defined, perform sub interface query
                if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                    $db = Database::singleton();
                    $data = (array) $db->Exe($this->ifc->getQuery($this->src));
                    
                    // Integrity check
                    if($this->ifc->isUni() && count($data) > 1) throw new Exception("Univalent (sub)interface returns more than 1 resource: " . $this->ifc->getPath(), 500);
                    
                    foreach ($data as $row) $tgtAtoms[] = new Atom($row['tgt'], $this->ifc->tgtConcept);
                    
                }else{
                    throw $e;
                }
            }
            return $tgtAtoms;
        }
        
    }

/**************************************************************************************************
 * Methods to call on ResourceList
 *************************************************************************************************/
     
    /**
     * @param int $rcOptions
     * @param int $ifcOptions
     * @param int $depth
     * @param array $recursionArr
     * @return mixed[]
     */
    public function get($rcOptions = Resource::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        $this->logger->debug("get() called for {$this->src} / {$this->ifc}");
        if(!$this->ifc->crudR()) throw new Exception ("Read not allowed for ". $this->ifc->getPath(), 405);
        
        // Initialize result
        $result = [];
        
        // Object nodes
        if($this->ifc->tgtConcept->isObject()){
            
            foreach ($this->getTgtResources() as $resource){
                $result[] = $resource->get($rcOptions, $ifcOptions, $depth, $recursionArr); // for json_encode $resource->jsonSerializable() is called
            }
            
            // Special case for leave PROP: return false when result is empty, otherwise true (i.e. I atom must be present)
            // Enables boolean functionality for editing ampersand property relations
            if($this->ifc->isLeaf() && $this->ifc->isProp()){
                if(empty($result)) return false;
                else return true;
            }
            
        // Non-object nodes (leaves, because subinterfaces are not allowed for non-objects)
        }else{
            foreach ($this->getTgtAtoms() as $atom) $result[] = $atom; // for json_encode $atom->jsonSerializable() is called
        }
        
        // Return result using UNI-aspect (univalent-> value/object, non-univalent -> list of values/objects)
        if($this->ifc->isUni() && empty($result)) return null;
        elseif($this->ifc->isUni()) return current($result);
        else return $result;
        
    }
    
    /**
     * @param stdClass $resourceToPost
     * @return Resource
     */
    public function post(stdClass $resourceToPost){
        if(!$this->ifc->crudC()) throw new Exception ("Create not allowed for ". $this->ifc->getPath(), 405);
        
        // Use attribute '_id_' if provided
        if(isset($resourceToPost->_id_)){
            $resource = new Resource($resourceToPost->_id_, $this->ifc->tgtConcept->name, $this);
            if($resource->exists()) throw new Exception ("Cannot create resource that already exists", 400);
        }else{
            $resource = new Resource(null, $this->ifc->tgtConcept->name, $this);
        }
        
        // If interface is editable, also add tuple(src, tgt) in interface relation
        if($this->ifc->isEditable() && $this->ifc->crudU()) $this->add($resource->id);
        else $resource->addAtom();
        
        // Put resource attributes
        $resource->put($resourceToPost);
        
        // Special case for file upload. TODO: make extension with hooks
        if($this->ifc->tgtConcept->isFileObject()){
            if (is_uploaded_file($_FILES['file']['tmp_name'])){
                $tmp_name = $_FILES['file']['tmp_name'];
                $new_name = time() . '_' . $_FILES['file']['name'];
                $absolutePath = Config::get('absolutePath') . Config::get('uploadPath') . $new_name;
                $relativePath = Config::get('uploadPath') . $new_name;
                $result = move_uploaded_file($tmp_name, $absolutePath);
                 
                if($result) Logger::getUserLogger()->notice("File '{$new_name}' uploaded");
                else throw new Exception ("Error in file upload", 500);
                
                // Populate filePath and originalFileName relations in database
                $resource->link($relativePath, 'filePath[FileObject*FilePath]')->add();
                $resource->link($_FILES['file']['name'], 'originalFileName[FileObject*FileName]')->add();
            }else{
                throw new Exception ("No file uploaded", 500);
            }
        }
        
        return $resource;
    }
    
    /**
     * Update a complete resource list (updates only this subinterface, not any level(s) deeper for now)
     * @param mixed $value
     * @return boolean
     */
    public function put($value){
        
        if($this->ifc->isUni()){ // expect value to be object or literal
            if(is_array($value)) throw new Exception("Non-array expected but array provided while updating " . $this->ifc->getPath(), 400);
            
            if($this->ifc->tgtConcept->isObject()){ // expect value to be object or null
                if(!is_object($value) && !is_null($value)) throw new Exception("Object (or null) expected but " . gettype($value) . " provided while updating " . $this->ifc->getPath(), 400);
                
                if(is_null($value)) $this->set($value);
                elseif(isset($value->_id_)) $this->set($value->_id_);
                else throw new Exception("No object identifier (_id_) provided while updating " . $this->ifc->getPath(), 400);
                
            }else{ // expect value to be literal (i.e. non-object) or null
                $this->set($value);
            }
            
        }else{ // expect value to be array
            if(!is_array($value)) throw new Exception("Array expected but not provided while updating " . $this->ifc->getPath(), 400);
            
            // First empty existing list
            $this->removeAll();
            
            // Add provided values
            foreach($value as $item){
                if($this->ifc->tgtConcept->isObject()){ // expect item to be object
                    if(!is_object($item)) throw new Exception("Object expected but " . gettype($item) . " provided while updating " . $this->ifc->getPath(), 400);
                    
                    if(isset($item->_id_)) $this->add($item->_id_);
                    else throw new Exception("No object identifier (_id_) provided while updating " . $this->ifc->getPath(), 400);
                }else{ // expect item to be literal (i.e. non-object) or null
                    $this->add($item);
                }
            }
        }
        
        return true;
    }
    
    /**
     * Alias of set() method. Used by Resource::patch() method
     * @param string $value
     * @return boolean
     */
    public function replace($value){
        if(!$this->ifc->isUni()) throw new Exception ("Cannot use replace for non-univalent interface " . $this->ifc->getPath() . ". Use add or remove instead", 400);
        return $this->set($value);
    }
    
    /**
     * Set provided value (for univalent interfaces)
     * @param string $value (value null is supported)
     * @return boolean
     */
    public function set($value){
        if(!$this->ifc->isUni()) throw new Exception ("Cannot use set() for non-univalent interface " . $this->ifc->getPath() . ". Use add or remove instead", 400);
        
        if(is_null($value)) $this->remove(null);
        else $this->add($value);
        
        return true;
    }
    
    /**
     * Add value to resource list
     * @param string $value
     * @return boolean
     */
    public function add($value){
        if(!isset($value)) throw new Exception ("Cannot add item. Value not provided", 400);
        if(is_object($value) || is_array($value)) throw new Exception("Literal expected but " . gettype($value) . " provided while updating " . $this->ifc->getPath(), 400);
        
        if(!$this->ifc->isEditable()) throw new Exception ("Interface is not editable " . $this->ifc->getPath(), 405);
        if(!$this->ifc->crudU()) throw new Exception ("Update not allowed for " . $this->ifc->getPath(), 405);
        
        $tgt = new Atom($value, $this->ifc->tgtConcept);
        if($tgt->concept->isObject() && !$this->ifc->crudC() && !$tgt->exists()) throw new Exception ("Create not allowed for " . $this->ifc->getPath(), 405);
        
        $this->src->link($tgt, $this->ifc->relation(), $this->ifc->relationIsFlipped)->add();
        
        return true;
    }
    
    /**
     * Remove value from resource list
     * @param string $value
     * @return boolean
     */
    public function remove($value){
        if(!isset($value) && !$this->ifc->isUni()) throw new Exception ("Cannot remove item. Value not provided", 400);
        if(is_object($value) || is_array($value)) throw new Exception("Literal expected but " . gettype($value) . " provided while updating " . $this->ifc->getPath(), 400);
        
        if(!$this->ifc->isEditable()) throw new Exception ("Interface is not editable " . $this->ifc->getPath(), 405);
        if(!$this->ifc->crudU()) throw new Exception ("Update not allowed for " . $this->ifc->getPath(), 405);
        
        $tgt = new Atom($value, $this->ifc->tgtConcept);
        $this->src->link($tgt, $this->ifc->relation(), $ifc->relationIsFlipped)->delete();
        
        return true;
    }
    
    public function removeAll(){
        if(!$this->ifc->isEditable()) throw new Exception ("Interface is not editable " . $this->ifc->getPath(), 405);
        if(!$this->ifc->crudU()) throw new Exception ("Update not allowed for " . $this->ifc->getPath(), 405);
        
        foreach ($this->getTgtAtoms() as $tgt) {
            $this->src->link($tgt, $this->ifc->relation(), $ifc->relationIsFlipped)->delete();
        }
    }
    
}

?>