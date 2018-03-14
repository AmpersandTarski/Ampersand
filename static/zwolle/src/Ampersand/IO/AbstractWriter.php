<?php

namespace Ampersand\IO;

use Psr\Http\Message\StreamInterface;
use Slim\Http\Stream;

abstract class AbstractWriter
{
    
    /**
     * The stream used to output
     *
     * @var \Psr\Http\Message\StreamInterface
     */
    protected $stream = null;
    
    /**
     *
     * @param \Psr\Http\Message\StreamInterface|null $stream
     * @param array $options Configuration options
     */
    public function __construct(StreamInterface $stream = null, array $options = [])
    {
        if (is_null($stream)) {
            $this->stream = new Stream(fopen('php://temp', 'w+'));
        } else {
            $this->stream = $stream;
        }
    }

    public function write(string $data)
    {
        $this->stream->write($data);
    }

    public function getContent()
    {
        return $this->stream->getContents();
    }

    public function close()
    {
        $this->stream->close();
    }
}
