<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Misc;

use Psr\Log\LoggerInterface;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Generics
{
    const HASH_ALGORITHM = 'md5';

    /**
     * Logger
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;

    /**
     * Directory where Ampersand model is generated in
     *
     * @var string
     */
    protected $folder;

    /**
     * Filepath for saving checksums of generated Ampersand model
     *
     * @var string
     */
    protected $checksumPath;

    /**
     * List of files that contain the generated Ampersand model
     *
     * @var array
     */
    protected $filesToCompare = [];

    /**
     * Constructor
     *
     * @param string $folder directory where Ampersand model is generated in
     */
    public function __construct(string $folder, LoggerInterface $logger)
    {
        $this->folder = realpath($folder);
        $this->logger = $logger;

        $this->checksumPath = "{$this->folder}/checksums.txt";
        $this->filesToCompare = glob("{$this->folder}/*.json");

        if (!file_exists($this->checksumPath)) {
            $this->writeChecksumFile();
        }
    }

    /**
     * Write new checksum file of generated model
     *
     * @return void
     */
    public function writeChecksumFile()
    {
        $this->logger->debug("Writing checksum file for generated Ampersand model files");

        $checksums = [];
        foreach ($this->filesToCompare as $path) {
            $filename = pathinfo($path, PATHINFO_BASENAME);
            $checksums[$filename] = hash_file(self::HASH_ALGORITHM, $path);
        }
    
        file_put_contents($this->checksumPath, serialize($checksums));
    }

    /**
     * Verify checksums of generated model. Return true when valid, false otherwise.
     *
     * @return bool
     */
    public function verifyChecksum(): bool
    {
        $this->logger->debug("Verifying checksum for Ampersand model files");

        $valid = true; // assume all checksums match

        // Get stored checksums
        $checkSums = unserialize(file_get_contents($this->checksumPath));

        // Compare checksum with actual file
        foreach ($this->filesToCompare as $path) {
            $filename = pathinfo($path, PATHINFO_BASENAME);
            if ($checkSums[$filename] !== hash_file(self::HASH_ALGORITHM, $path)) {
                $this->logger->warning("Invalid checksum of file '{$filename}'");
                $valid = false;
            }
        }

        return $valid;
    }

    public function getFolder(): string
    {
        return $this->folder;
    }
}
