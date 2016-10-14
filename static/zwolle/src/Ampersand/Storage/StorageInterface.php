<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;
use Ampersand\Core\Link;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface StorageInterface {
    
    public function atomExists(Atom $atom);
    
    public function linkExists(Link $link);
    
    public function addAtom(Atom $atom);
    
    public function atomSetConcept(Atom $atom, Concept $conceptB);
    
    public function atomClearConcept(Atom $atom);
    
    public function addLink(Link $link);
    
    public function deleteLink(Link $link);
    
    public function deleteAtom(Atom $atom);
    
    public function startTransaction();
    
    public function commitTransaction();
    
    public function rollbackTransaction();
}