<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Ampersand\IO\AbstractWriter;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class JSONWriter extends AbstractWriter {
    
    public function serialize($data){
        // Output
        fwrite($this->resource, json_encode($data, JSON_PRETTY_PRINT));
    }
}