<?php

namespace Ampersand\IO;

abstract class AbstractWriter {
    
    /**
     * The stream used to output
     *
     * @var stream
     */
    protected $stream = null;
    
    /**
     *
     * @param stream $stream (e.g. stream)
     * @param array $options Configuration options
     */
    public function __construct($stream = null, $options = []){
        if (is_null($stream)) {
            $this->stream = fopen('php://temp', 'w+');
        } else {
            $this->stream = $stream;
        }
    }

    public function write($data){
        fwrite($this->stream, $data);
    }

    public function getContent(){
        return stream_get_contents($this->stream, -1, 0);
    }

    public function print(){
        print $this->getContent();
    }

    public function close(){
        fclose($this->stream);
    }
}
