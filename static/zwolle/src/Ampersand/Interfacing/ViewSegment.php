<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class ViewSegment {

    /**
     *
     * @var int
     */
    public $seqNr;

    /**
     *
     * @var string
     */
    public $label;

    /**
     *
     * @var string
     */
    public $segType;

    /**
     *
     * @var string
     */
    public $text;

    /**
     *
     * @var string
     */
    public $expADL;

    /**
     *
     * @var string
     */
    private $expSQL;

    /**
     * Constructor of view segments
     * @param array $viewSegmentDef
     */
    public function __construct($viewSegmentDef){
        $this->seqNr = $viewSegmentDef['seqNr'];
        $this->label = $viewSegmentDef['label'];
        $this->segType = $viewSegmentDef['segType'];
        $this->text = $viewSegmentDef['text'];
        $this->expADL = $viewSegmentDef['expADL'];
        $this->expSQL = $viewSegmentDef['expSQL'];
    }

    /**
     * Returns query of view segment
     * @return string
     */
    public function getQuery(){
        return str_replace('_SESSION', session_id(), $this->expSQL); // Replace _SESSION var with current session id.
    }
}

?>