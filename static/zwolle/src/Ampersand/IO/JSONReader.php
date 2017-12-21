<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

class JSONReader {

    /**
     * Undocumented variable
     *
     * @var array $options
     */
    protected $options = [];

    /**
     * Content of loaded json file.
     * 
     * @var object $content
     */
    protected $content = null;

    /**
     * @param array $options Configuration options
     */
    public function __construct($options = []){
        \array_merge($this->options, $options);
    }
    
    public function loadFileContent($fileContent){
        $this->content = json_decode($fileContent);
    }

    public function loadFile($filePath){
        $content = file_get_contents($filePath);
        $this->loadFileContent($content);
    }
}

?>