<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;

/**
 * Interface for storage implementations
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface StorageInterface {
    
    public function getLabel();
    
    public function commitTransaction();
    
    public function rollbackTransaction();
}

?>