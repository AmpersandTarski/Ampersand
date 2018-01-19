<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs;

use Ampersand\Core\Atom;
use Ampersand\Core\Concept;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface ConceptPlugInterface extends StorageInterface {
    
    public function atomExists(Atom $atom);
    
    public function getAllAtoms(Concept $concept);
    
    public function addAtom(Atom $atom);
    
    public function removeAtom(Atom $atom);
    
    public function deleteAtom(Atom $atom);
}
