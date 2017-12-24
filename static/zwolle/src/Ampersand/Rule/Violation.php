<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Core\Atom;
use Ampersand\AmpersandApp;
use Ampersand\Rule\Rule;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Violation {

    /**
     * Rule to which this violation belongs to
     * @var \Ampersand\Rule\Rule
     */
    public $rule;

    /**
     *
     * @var \Ampersand\Core\Atom
     */
    public $src;

    /**
     *
     * @var \Ampersand\Core\Atom
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
     * 
     * @param \Ampersand\Rule\Rule $rule
     * @param string $srcAtomId
     * @param string $tgtAtomId
     */
    public function __construct(Rule $rule, $srcAtomId, $tgtAtomId){
        $this->rule = $rule;
        $this->src = new Atom($srcAtomId, $rule->srcConcept);
        $this->tgt = new Atom($tgtAtomId, $rule->tgtConcept);
    }
    
    /**
     * Function is called when object is treated as a string
     * 
     * @return string role label
     */
    public function __toString(){
        return "({$this->src},{$this->tgt})";
    }
    
    /**
     * Undocumented function
     * 
     * @throws Exception when segment expression return more that 1 tgt atom
     * @return string
     */
    public function getViolationMessage(){
        $strArr = [];
        foreach ($this->rule->getViolationSegments() as $segment){
            $tgtAtomIds = $segment->getData($this->src, $this->tgt);

            if(count($tgtAtomIds) > 1) throw new Exception("Expression of RULE segment '{$segment}' results in more than one tgt atom", 501); // 501: Not implemented
            $strArr[] = count($tgtAtomIds) ? $tgtAtomIds[0] : null;
        }

        // If empty array of strings (i.e. no violation segments defined), use default violation representation: '<src>,<tgt>'
        return $this->message = empty($strArr) ? "{$this->src},{$this->tgt}" : implode($strArr);
    }

    }

    /**
     * Get interfaces to solve the violation
     * @param string $srcOrTgt specifies to get interfaces for source concept (src), target concept (tgt) or both (null)
     * @return array
     */
    public function getInterfaces($srcOrTgt = null){
        $ampersandApp = AmpersandApp::singleton();
        
        switch ($srcOrTgt) {
            case 'src':
                return $ampersandApp->getInterfacesToReadConcepts([$this->src->concept]);
                break;
            case 'tgt':
                return $ampersandApp->getInterfacesToReadConcepts([$this->tgt->concept]);
                break;
            default:
                return $ampersandApp->getInterfacesToReadConcepts([$this->src->concept, $this->tgt->concept]);
                break;
        }
    }
}

?>