<?php


namespace Ampersand\IO;

abstract class AbstractWriter {
    /**
     * The resource used to output/serialize
     *
     * @var resource
     */
    protected $resource = null;
    
    /**
     * 
     * @param resource $resource (e.g. stream, file, etc)
     * @param array $options Configuration options
     */
    public function __construct($resource, $options = []){
        $this->resource = $resource;
    }

    public function serialize($data){
        fwrite($this->resource, $data);
    }

    public function close(){
        fclose($this->resource);
    }
}

?>