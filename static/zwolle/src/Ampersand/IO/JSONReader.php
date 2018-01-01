<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Ampersand\IO\AbstractReader;

class JSONReader extends AbstractReader {

    public function getContent(){
        return json_decode(stream_get_contents($this->stream, -1, 0));
    }

}
