<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Exception;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;
use Ampersand\IO\AbstractWriter;
use Psr\Log\LoggerInterface;

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
     * @param \Psr\Log\LoggerInterface $logger
     * @param array $options
     */
    public function __construct(AbstractWriter $writer, LoggerInterface $logger, array $options = []){
        $this->logger = $logger;
        $this->writer = $writer;
    }

    public function exportAllPopulation(){
        $conceptPop = [];
        foreach (Concept::getAllConcepts() as $concept){
            $conceptPop[] = [
                'concept' => $concept->name,
                'atoms' => array_map(function($atom){ return $atom->id; }, $concept->getAllAtomObjects())
            ];
        }
        
        $relationPop = [];
        foreach (Relation::getAllRelations() as $rel){
            $relationPop[] = [
                'relation' => $rel->signature,
                'links' => $rel->getAllLinks()
            ];
        }

        $this->writer->write(['atoms' => $conceptPop, 'links' => $relationPop]);

        return $this;
    }
}
