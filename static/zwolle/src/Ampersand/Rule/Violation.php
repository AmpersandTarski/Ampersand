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
     * @param string $srcAtomId
     * @param string $tgtAtomId
     */
    public function __construct($rule, $srcAtomId, $tgtAtomId){
        $this->rule = $rule;
        $this->src = new Atom($srcAtomId, $rule->srcConcept);
        $this->tgt = new Atom($tgtAtomId, $rule->tgtConcept);
    }
    
    /**
     * Function is called when object is treated as a string
     * @return string role label
     */
    public function __toString(){
        return "({$this->src},{$this->tgt})";
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
                $atomId = $database->getDBRepresentation($atom);
                $expSQL = str_replace('_SESSION', session_id(), $segment['expSQL']);
                $query = "SELECT DISTINCT `tgt` FROM ($expSQL) AS `results` WHERE `src` = '{$atomId}'"; // SRC of TGT kunnen door een expressie gevolgd worden
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

        // If empty array of strings (i.e. no violation segments defined), use default violation representation: '<src>,<tgt>'
        $this->message = empty($strArr) ? "{$this->src},{$this->tgt}" : implode($strArr);

        return $this->message;
    }

    /**
     * Get interfaces to solve the violation
     * @param string $srcOrTgt specifies to get interfaces for source concept (src), target concept (tgt) or both (null)
     * @return array
     */
    public function getInterfaces($srcOrTgt = null){
        $session = Session::singleton();
        
        switch ($srcOrTgt) {
            case 'src':
                return $session->getInterfacesToReadConcepts([$this->src->concept]);
                break;
            case 'tgt':
                return $session->getInterfacesToReadConcepts([$this->tgt->concept]);
                break;
            default:
                return $session->getInterfacesToReadConcepts([$this->src->concept, $this->tgt->concept]);
                break;
        }
    }
}

?>