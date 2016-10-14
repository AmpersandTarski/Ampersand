<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;

use Ampersand\Core\Atom;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface ConceptStorageInterface extends StorageInterface {
    
    public function atomExists(Atom $atom);
    
    public function addAtom(Atom $atom);
    
    public function removeAtom(Atom $atom);
    
    public function deleteAtom(Atom $atom);
}

?>