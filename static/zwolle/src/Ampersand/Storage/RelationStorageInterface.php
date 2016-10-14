<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Storage;

use Ampersand\Core\Link;

/**
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface RelationStorageInterface extends StorageInterface {
    
    public function linkExists(Link $link);
    
    public function addLink(Link $link);
    
    public function deleteLink(Link $link);
}

?>