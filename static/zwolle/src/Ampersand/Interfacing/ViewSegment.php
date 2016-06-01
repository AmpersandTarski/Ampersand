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
    public $expSQL;
    
    /**
     * @var string|null $parentIfcCol the label/name of the columm of view concept if applicable (i.e. same table)
     */
    public $parentIfcCol = null;


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
        if(isset($viewSegmentDef['parentIfcCol'])) $this->parentIfcCol = $viewSegmentDef['parentIfcCol'];
    }
}

?>