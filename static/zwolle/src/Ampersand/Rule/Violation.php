<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Core\Atom;
use Ampersand\Session;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Violation {

    /**
     * Rule to which this violation belongs to
     * @var Rule\Rule
     */
    public $rule;

    /**
     *
     * @var Atom
     */
    public $src;

    /**
     *
     * @var Atom
     */
    public $tgt;

    /**
     *
     * @var string
     */
    private $message;

    /**
     * Array with urls that could be used to solve the violation
     * @var string[]
     */
    public $urls;

    /**
     * Constructor of violation
     * @param Rule\Rule $rule
     * @param string $srcAtom
     * @param string $tgtAtom
     */
    public function __construct($rule, $srcAtomId, $tgtAtomId){
        $this->rule = $rule;
        $this->src = new Atom($srcAtomId, $rule->srcConcept->name);
        $this->tgt = new Atom($tgtAtomId, $rule->tgtConcept->name);
    }

    public function __toString(){

    }

    /**
     *
     * @throws Exception when segment type is unknown
     * @throws Exception when segment expression return more that 1 tgt atom
     * @return string
     */
    public function getViolationMessage(){
        $database = Database::singleton();

        $strArr = array();
        foreach ($this->rule->violationSegments as $segment){
            // text segment
            if ($segment['segmentType'] == 'Text'){
                $strArr[] = $segment['Text'];
                 
            // expressie segment
            }elseif($segment['segmentType'] == 'Exp'){
                // select starting atom depending on whether the segment uses the src of tgt atom.
                $atom = $segment['srcOrTgt'] == 'Src' ? $this->src : $this->tgt;

                // quering the expression
                $query = "SELECT DISTINCT `tgt` FROM ($segment[expSQL]) AS `results` WHERE `src` = '{$atom->idEsc}'"; // SRC of TGT kunnen door een expressie gevolgd worden
                $rows = $database->Exe($query);

                // returning the result
                if(count($rows) > 1) throw new Exception("Expression of pairview results in more than one tgt atom", 501); // 501: Not implemented
                $strArr[] = $rows[0]['tgt'];

            // unknown segment
            }else{
                $errorMessage = "Unknown segmentType '{$segment['segmentType']}' in violationSegments of rule '{$this->rule->id}'";
                throw new Exception($errorMessage, 501); // 501: Not implemented
            }
        }

        // If empty array of strings (i.e. no violation segments defined), use default violation representation: '<srcAtom>,<tgtAtom>'
        $this->message = empty($strArr) ? "{$this->src->label},{$this->tgt->label}" : implode($strArr);

        return $this->message;
    }

    /**
     * Build links to interfaces to solve the violation
     * @return array
     */
    public function getLinks(){
        $session = Session::singleton();

        $links = array();
        foreach ($session->getInterfacesToReadConcept($this->src->concept->name) as $interface){
            $links[] = "#/{$interface->id}/{$this->src->id}";
        }
        foreach ($session->getInterfacesToReadConcept($this->tgt->concept->name) as $interface){
            $links[] = "#/{$interface->id}/{$this->tgt->id}";
        }
        return array_unique($links);
    }
}

?>