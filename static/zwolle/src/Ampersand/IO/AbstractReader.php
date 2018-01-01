<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

abstract class AbstractReader {

    /**
     * The stream used to input
     *
     * @var stream
     */
    protected $stream = null;

    /**
     * Constructor
     * 
     * @param array $options Configuration options
     */
    public function __construct($options = []){
        
    }

    public function getContent(){
        return stream_get_contents($this->stream, -1, 0);
    }

    public function loadFile($filePath){
        // Check if file exists
        if (!file_exists($filePath) || !is_readable($filePath)) {
            throw new Exception("Could not open {$filePath}. File does not exist");
        }

        // Open file
        $this->stream = fopen($filePath, 'r');
        if ($this->stream === FALSE) {
            throw new Exception("Could not open {$filePath}");
        }
    }
}
