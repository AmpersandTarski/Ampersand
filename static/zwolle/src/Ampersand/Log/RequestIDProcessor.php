<?php

namespace Ampersand\Log;

/**
 * Adds same generated value into every record within a request
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 */
class RequestIDProcessor
{
    /**
     * Request identifier
     *
     * @var string
     */
    protected $requestID;
    
    public function __construct()
    {
        $this->requestID = bin2hex(random_bytes(5));
    }

    /**
     * @param  array $record
     * @return array
     */
    public function __invoke(array $record)
    {
        $record['extra']['request_id'] = $this->requestID;

        return $record;
    }
}
