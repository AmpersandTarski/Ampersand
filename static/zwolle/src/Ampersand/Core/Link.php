<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Core;
use JsonSerializable;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Link implements JsonSerializable {
    private $db;
    
    public $rel;
    
    public $src;
    
    public $tgt;
    
    public function __construct(Relation $rel, Atom $src, Atom $tgt){
        $this->rel = $rel;
        $this->src = $src;
        $this->tgt = $tgt;
        
        // Checks
        if(is_null($this->src->id) || is_null($this->tgt->id)) throw new Exception ("Cannot instantiate link {$this}, because src and/or tgt atom is not specified", 500);
        if(!in_array($this->src->concept, $this->rel->srcConcept->getSpecializationsIncl())) throw new Exception ("Cannot instantiate link {$this}, because source atom does not match relation source concept or any of its specializations", 500);
        if(!in_array($this->tgt->concept, $this->rel->tgtConcept->getSpecializationsIncl())) throw new Exception ("Cannot instantiate link {$this}, because target atom does not match relation target concept or any of its specializations", 500);
        
        $this->db = Database::singleton();
        
    }
    
    /**
     * Function is called when object is treated as a string
     * @return string
     */
    public function __toString(){
        return "({$this->src},{$this->tgt})[{$this->rel}]";
    }
    
    /**
     * Function is called when object encoded to json with json_encode()
     * @return array
     */
    public function jsonSerialize(){
        return ['rel' => $this->rel->getSignature(),  'src' => $this->src, 'tgt' => $this->tgt];
    }
    
    /**
     * Check if link exists in relation
     * @return boolean
     */
    public function exists(){
        $logger = Logger::getLogger("CORE")->debug("Checking if link {$this} exists in database");
        
        return $this->db->linkExists($this->rel, $this->src, $this->tgt);
    }
    
    /**
     * Add link to database
     * @return Link $this
     */
    public function add(){
        $logger = Logger::getLogger("CORE")->debug("Add link {$this} to database");
        
        // Ensure that atoms exists in their concept tables
        $this->src->add();
        $this->tgt->add();
        
        $this->db->addLink($this->rel, $this->src, $this->tgt);
        
        return $this;
    }
    
    /**
     * Delete link from database
     * @return Link $this
     */
    public function delete(){
        $logger = Logger::getLogger("CORE")->debug("Delete link {$this} from database");
        
        $this->db->deleteLink($this->rel, $this->src, $this->tgt);
        
        return $this;
    }
    
    /**
     * Return source atom
     * @return Atom $this->src
     */
    public function src(){
        return $this->src;
    }
    
    /**
     * Return target atom
     * @return Atom $this->tgt
     */
    public function tgt(){
        return $this->tgt;
    }
    
}

?>