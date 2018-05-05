<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs;

use Ampersand\Core\Atom;
use Ampersand\Core\Link;
use Ampersand\Core\Relation;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface RelationPlugInterface extends StorageInterface
{
    
    public function linkExists(Link $link);
    
    /**
    * Get all links given a relation
    * @param Relation $relation
    * @param \Ampersand\Core\Atom|null $srcAtom if specified get all links with $srcAtom as source
    * @param \Ampersand\Core\Atom|null $tgtAtom if specified get all links with $tgtAtom as tgt
    * @return Link[]
    */
    public function getAllLinks(Relation $relation, Atom $srcAtom = null, Atom $tgtAtom = null);
    
    public function addLink(Link $link);
    
    public function deleteLink(Link $link);
    
    /**
     * @param \Ampersand\Core\Relation $relation relation from which to delete all links
     * @param \Ampersand\Core\Atom|null $atom atom for which to delete all links
     * @param string|null $srcOrTgt specifies to delete all link with $atom as src, tgt or both (null/not provided)
     * @return void
     */
    public function deleteAllLinks(Relation $relation, Atom $atom = null, string $srcOrTgt = null);
}
