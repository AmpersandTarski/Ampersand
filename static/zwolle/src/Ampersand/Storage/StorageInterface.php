<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface StorageInterface {
    
    public function atomExists(Atom $atom);
    
    public function linkExists(Relation $relation, Atom $srcAtom, Atom $tgtAtom);
    
    public function addAtom(Atom $atom);
    
    public function atomSetConcept(Atom $atom, Concept $conceptB);
    
    public function atomClearConcept(Atom $atom);
    
    public function addLink(Relation $relation, Atom $srcAtom, Atom $tgtAtom);
    
    public function deleteLink(Relation $relation, Atom $srcAtom, Atom $tgtAtom);
    
    public function deleteAtom(Atom $atom);
    
    public function startTransaction();
    
    public function commitTransaction();
    
    public function rollbackTransaction();
}