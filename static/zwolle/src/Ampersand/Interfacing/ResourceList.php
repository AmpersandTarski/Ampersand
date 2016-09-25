<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;
use Ampersand\Core\Atom;
use Ampersand\Database\Database;

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
        if(!in_array($tgtId, $arr = $this->getTgtResources())) throw new Exception ("Resource not found", 404);
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
    
    public function getList($options = Resource::DEFAULT_OPTIONS, $depth = null){
        if(!$this->parentIfc->crudR()) throw new Exception ("Read not allowed for ". $this->parentIfc->path(), 403);
        
        // Initialize result
        $result = [];
        
        // Object nodes
        if($this->parentIfc->tgtConcept->isObject()){
            
            foreach ($this->getTgtResources() as $resource){
                $result[] = $resource->get($options, $depth);
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
        
        
        // TODO: check what to do with code below
        // Loop over target atoms
        foreach ($this->getTgtResources() as $tgtResource){
            
            // Reference to other interface
            if($this->parentIfc->isRef()
                && (!$this->parentIfc->isLinkTo || ($options & Resource::INCLUDE_LINKTO_DATA))  // Include content is interface is not LINKTO or inclLinktoData is explicitly requested via the options
                && (!is_null($depth) || !in_array($tgtResource->id, (array)$recursionArr[$this->parentIfc->refInterfaceId]))){ // Prevent infinite loops
                
                $ifc = $tgtResource->ifc($this->parentIfc->refInterfaceId);
                
                // Skip ref interface if not given read rights to prevent Exception
                if(!$ifc->crudR) break; // breaks foreach loop
                
                foreach($ifc->getTgtAtoms() as $refTgtAtom){
                    
                    // Add target atom to $recursionArr to prevent infinite loops
                    if($options['inclLinktoData']) $recursionArr[$this->parentIfc->refInterfaceId][] = $refTgtAtom->id;
                    
                    $result[] = $refTgtAtom->getContent($options, $recursionArr, $depth);
                }
            }
        }
    }
}

?>