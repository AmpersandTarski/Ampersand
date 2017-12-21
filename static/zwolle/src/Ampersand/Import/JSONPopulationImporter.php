<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Import;

use Exception;
use Ampersand\Core\Concept;
use Ampersand\Core\Atom;
use Ampersand\Core\Relation;
use Ampersand\Core\Link;

class JSONPopulationImporter extends \Ampersand\IO\JSONReader {

    /**
     * Logger
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;
    
    /**
     * Constructor
     *
     * @param array $options
     */
    public function __construct($options = []){
        parent::__construct($options);

        $this->logger = \Ampersand\Log\Logger::getLogger('IMPORTER');
    }
    
    /**
     * Import population (i.e. all provided atoms and links)
     *
     * @return void
     */
    public function importPopulation(){
        $this->logger->info("Start import of population");

        // Before importing, check if all provided concepts and relations are defined
        $this->logger->debug("Checking if all concepts for which population is provided are defined");
        foreach($$this->content->atoms as $pop) if(!empty($pop->atoms)) Concept::getConcept($pop->concept);
        $this->logger->debug("Checking if all relations for which population is provided are defined");
        foreach($$this->content->links as $pop) if(!empty($pop->links)) Concept::getConcept($pop->relation);

        $this->importAtoms();
        $this->importLinks();

        $this->logger->info("End import of population");
    }

    /**
     * Import concept population (i.e. all provided atoms)
     *
     * @return void
     */
    public function importAtoms(){
        $this->logger->info("Importing concept populations");

        foreach((array)$this->content->atoms as $population){
            if(empty($population->atoms)) continue; // Skip when nothing to import

            $concept = Concept::getConcept($population->concept);
            $total = count($population->atoms);
            $this->logger->debug("Importing {$total} atoms for concept {$concept}");
            
            foreach($population->atoms as $atomId){
                $atom = new Atom($atomId, $concept);
                $atom->add();
            }
        }
    }

    /**
     * Import relation population (i.e. all provided links)
     *
     * @return void
     */
    public function importLinks(){
        $this->logger->info("Importing relation populations");
        
        foreach ((array)$this->content->links as $population){
            if(empty($population->links)) continue; // Skip when nothing to import

            $relation = Relation::getRelation($population->relation);
            $total = count($population->links);
            $this->logger->debug("Importing {$total} links for relation {$relation}");
            
            foreach($population->links as $pair){
                $link = new Link($relation, new Atom($pair->src, $relation->srcConcept), new Atom($pair->tgt, $relation->tgtConcept));
                $link->add();
            }
        }
    }
    
}


?>