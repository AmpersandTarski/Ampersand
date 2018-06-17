<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Misc;

use Psr\Log\LoggerInterface;
use Exception;

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
    protected $checksumFile;

    /**
     * List of files that contain the generated Ampersand model
     *
     * @var array
     */
    protected $modelFiles = [];

    /**
     * Constructor
     *
     * @param string $folder directory where Ampersand model is generated in
     */
    public function __construct(string $folder, LoggerInterface $logger)
    {
        $this->folder = realpath($folder);
        $this->logger = $logger;

        $this->checksumFile = "{$this->folder}/checksums.txt";
        $this->modelFiles = glob("{$this->folder}/*.json");

        if (!file_exists($this->checksumFile)) {
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
        foreach ($this->modelFiles as $path) {
            $filename = pathinfo($path, PATHINFO_BASENAME);
            $checksums[$filename] = hash_file(self::HASH_ALGORITHM, $path);
        }
    
        file_put_contents($this->checksumFile, serialize($checksums));
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
        $checkSums = unserialize(file_get_contents($this->checksumFile));

        // Compare checksum with actual file
        foreach ($this->modelFiles as $path) {
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

    protected function loadFile(string $filename)
    {
        if (!array_key_exists($filename, $this->modelFiles)) {
            throw new Exception("Filename '{$filename}' is not a valid file to load here", 500);
        }

        return file_get_contents($this->modelFiles[$filename]);
    }

    protected function getFile(string $filename)
    {
        static $loadedFiles = [];

        if (!array_key_exists($filename, $loadedFiles)) {
            $loadedFiles[$filename] = $this->loadFile($filename);
        }

        return $loadedFiles[$filename];
    }

    public function getSetting(string $setting)
    {
        $fileContent = $this->getFile('settings.json');
        $settings = json_decode($fileContent, false);
        
        if (!property_exists($settings, $setting)) {
            throw new Exception("Undefined setting '{$setting}' in settings.json", 500);
        }

        return $settings->$setting;
    }
}
