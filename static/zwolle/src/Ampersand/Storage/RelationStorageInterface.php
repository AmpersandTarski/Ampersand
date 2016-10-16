<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;

use Ampersand\Core\Atom;
use Ampersand\Core\Link;
use Ampersand\Core\Relation;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface RelationStorageInterface extends StorageInterface {
    
    public function linkExists(Link $link);
    
    /**
    * Get all links given a relation
    * @param Relation $relation
    * @return Link[]
    */
    public function getAllLinks(Relation $relation);
    
    public function addLink(Link $link);
    
    public function deleteLink(Link $link);
    
    /**
     * @param Relation $relation relation from which to delete all links
     * @param Atom $atom atom for which to delete all links
     * @param string $srcOrTgt specifies to delete all link with $atom as src, tgt or both (null/not provided)
     * @return void
     */
    public function deleteAllLinks(Relation $relation, Atom $atom = null, $srcOrTgt = null);
}

?>