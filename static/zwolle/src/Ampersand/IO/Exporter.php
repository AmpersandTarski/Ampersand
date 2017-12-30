<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Exception;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\Log\Logger;
use Ampersand\IO\AbstractWriter;

class Exporter {

    /**
     * Logger
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;

    /**
     * Undocumented variable
     *
     * @var \Ampersand\IO\AbstractWriter
     */
    protected $writer;
    
    /**
     * Constructor
     *
     * @param \Ampersand\IO\AbstractWriter $writer
     * @param array $options
     */
    public function __construct(AbstractWriter $writer, array $options = []){
        $this->logger = Logger::getLogger('IO');
        $this->writer = $writer;
    }

    public function exportAllPopulation(){
        $allAtoms = array();
        foreach (Concept::getAllConcepts() as $concept){
            $allAtoms[$concept->name] = array_map(function($atom){
                return $atom->id;
            }, $concept->getAllAtomObjects());
        }
        
        $allLinks = array();
        foreach (Relation::getAllRelations() as $rel){
            $allLinks[$rel->signature] = $rel->getAllLinks();
        }

        $this->writer->write(['atoms' => $allAtoms, 'links' => $allLinks]);

        $this->writer->print();

        $this->writer->close();
    }
}

?>