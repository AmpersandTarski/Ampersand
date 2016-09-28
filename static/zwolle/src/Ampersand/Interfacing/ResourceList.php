<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;
use stdClass;
use Exception;
use Ampersand\Config;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class ResourceList {
    
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
        $this->src = $src;
        $this->parentIfc = $parentIfc;
    }
    
    /**
     * @param string $tgtId
     * @return Resource
     */
    public function one($tgtId){
        // Functionality to automatically add/create resource if allowed
        if(!in_array($tgtId, $arr = $this->getTgtResources())){
            $resource = new Resource($tgtId, $this->parentIfc->tgtConcept->name);
            
            // If resource already exists and may be added (crudU)
            if($resource->atomExists() && $this->parentIfc->crudU()) $this->add($resource);
            // Elseif resource not yet exists and may be created (crudC) 
            elseif(!$resource->atomExists() && $this->parentIfc->crudC()){
                $obj = new stdClass();
                $obj->_id_ = $resource->id;
                $this->post($obj);
            }
            // Else: return not found
            else throw new Exception ("Resource '{$resource}' not found", 404);
            
            // Reevaluate interface expression, tgt should now be there, otherwise throw exception
            if(!in_array($tgtId, $arr = $this->getTgtResources(false))) throw new Exception ("Oeps.. something went wrong", 500);
        }
        else return $arr[$tgtId];
    }
    
    /**
     * @param boolean $fromCache specifies if target resources may be get from cache (true) or recalculated (false)
     * @return Resource[]
     */
    private function getTgtResources($fromCache = true){
        if(!isset($this->tgtResources) || !$fromCache){
            $this->tgtResources = array();
            try {
                // If interface isIdent (i.e. expr = I[Concept]) we can return the src
                if($this->parentIfc->isIdent()){
                    $this->tgtResources[$this->src->id] = new Resource($this->src->id, $this->parentIfc->tgtConcept->name, $this->parentIfc, $this->src);
                    
                // Else try to get tgt atom from src query data (in case of uni relation in same table)
                }else{
                    $tgt = $this->src->getQueryData('ifc_' . $this->parentIfc->id); // column is prefixed with ifc_ in query data
                    if(!is_null($tgt)) $this->tgtResources[$tgt] = new Resource($tgt, $this->parentIfc->tgtConcept->name, $this->parentIfc, $this->src);
                }
            }catch (Exception $e) {
                // Column not defined, perform sub interface query
                if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                    $db = Database::singleton();
                    $data = (array) $db->Exe($this->parentIfc->getQuery($this->src));
                    
                    // Integrity check
                    if($this->parentIfc->isUni() && count($data) > 1) throw new Exception("Univalent (sub)interface returns more than 1 resource: " . $this->parentIfc->path(), 500);
                    
                    foreach ($data as $row) {
                        $r = new Resource($row['tgt'], $this->parentIfc->tgtConcept->name, $this->parentIfc, $this->src);
                        $r->setQData($row);
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
        elseif($this->parentIfc->tgtConcept->isObject()) return $this->getTgtResources($fromCache);
        
        // Otherwise (i.e. non-object atoms) get atoms from database. This is never cached. We only cache resources (i.e. object atoms)
        else{
            
            try {
                // Try to get tgt atom from src query data (in case of uni relation in same table)
                $tgt = $this->src->getQueryData('ifc_' . $this->parentIfc->id); // column is prefixed with ifc_ in query data
                if(!is_null($tgt)) $tgtAtoms[] = new Atom($tgt, $this->parentIfc->tgtConcept);
                
            }catch (Exception $e) {
                // Column not defined, perform sub interface query
                if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                    $db = Database::singleton();
                    $data = (array) $db->Exe($this->parentIfc->getQuery($this->src));
                    
                    // Integrity check
                    if($this->parentIfc->isUni() && count($data) > 1) throw new Exception("Univalent (sub)interface returns more than 1 resource: " . $this->parentIfc->path(), 500);
                    
                    foreach ($data as $row) $tgtAtoms[] = new Atom($row['tgt'], $this->parentIfc->tgtConcept);
                    
                }else{
                    throw $e;
                }
            }
            return $tgtAtoms;
        }
        
    }
    
    /**
     * @param int $rcOptions
     * @param int $ifcOptions
     * @param int $depth
     * @param array $recursionArr
     * @return mixed[]
     */
    public function getList($rcOptions = Resource::DEFAULT_OPTIONS, $ifcOptions = InterfaceObject::DEFAULT_OPTIONS, $depth = null, $recursionArr = []){
        if(!$this->parentIfc->crudR()) throw new Exception ("Read not allowed for ". $this->parentIfc->path(), 403);
        
        // Initialize result
        $result = [];
        
        // Object nodes
        if($this->parentIfc->tgtConcept->isObject()){
            
            foreach ($this->getTgtResources() as $resource){
                $result[] = $resource->get($rcOptions, $ifcOptions, $depth, $recursionArr);
            }
            
            // Special case for leave PROP: return false when result is empty, otherwise true (i.e. I atom must be present)
            // Enables boolean functionality for editing ampersand property relations
            if($this->parentIfc->isLeaf() && $this->parentIfc->isProp()){
                if(empty($result)) return false;
                else return true;
            }
            
        // Non-object nodes (leaves, because subinterfaces are not allowed for non-objects)
        }else{
            foreach ($this->getTgtAtoms() as $atom) $result[] = $atom->getJsonRepresentation();
        }
        
        // Return result using UNI-aspect (univalent-> value/object, non-univalent -> list of values/objects)
        if($this->parentIfc->isUni() && empty($result)) return null;
        elseif($this->parentIfc->isUni()) return current($result);
        else return $result;
        
    }
    
    /**
     * @param stdClass $resourceToPost
     * @return stdClass representation of newly created resource
     */
    public function post(stdClass $resourceToPost){
        if(!$this->parentIfc->crudC()) throw new Exception ("Create not allowed for ". $this->parentIfc->path(), 403);
        
        // Use attribute '_id_' if provided
        if(isset($resourceToPost->_id_)){
            $resource = new Resource($resourceToPost->_id_, $this->parentIfc->tgtConcept->name, $this->parentIfc, $this->src);
            if($resource->atomExists()) throw new Exception ("Cannot create resource that already exists", 403);
        }else{
            $id = $this->parentIfc->tgtConcept->createNewAtomId();
            $resource = new Resource($id, $this->parentIfc->tgtConcept->name, $this->parentIfc, $this->src);
        }
        
        // If interface is editable, also add tuple(src, tgt) in interface relation
        if($this->parentIfc->isEditable() && $this->parentIfc->crudU()) $this->add($resource);
        else $resource->addAtom();
        
        // Put resource attributes
        $resource->put($resourceToPost);
        
        // Special case for file upload. TODO: make extension with hooks
        if($this->parentIfc->tgtConcept->isFileObject()){
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
                
                $relFilePath->addLink($resource, new Atom($relativePath, $conceptFilePath));
                $relOriginalFileName->addLink($resource, new Atom($_FILES['file']['name'], $conceptFileName));
                
            }else{
                throw new Exception ("No file uploaded", 500);
            }
        }
        
        return $resource->get();
    }
    
    
    public function add($resource){
        if($this->relation) $this->relation()->addLink($this->srcAtom, $newAtom, $this->relationIsFlipped);
    }
    
    public function remove($resource){
        
    }
}

?>