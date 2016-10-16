<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;
use Ampersand\Database\Database;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class ViewSegment {

    /**
     * The view to which this segment belongs to
     * @var View $view
     */
    protected $view;
    
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
     * Constructor of view segments
     * @param array $viewSegmentDef
     */
    public function __construct($viewSegmentDef, View $view){
        $this->view = $view;
        $this->seqNr = $viewSegmentDef['seqNr'];
        $this->label = is_null($viewSegmentDef['label']) ? $viewSegmentDef['seqNr'] : $viewSegmentDef['label'];
        $this->segType = $viewSegmentDef['segType'];
        $this->text = $viewSegmentDef['text'];
        $this->expADL = $viewSegmentDef['expADL'];
        $this->expSQL = $viewSegmentDef['expSQL'];
        
        if(!($this->segType === 'Text' || $this->segType === 'Exp')) throw new Exception("Unsupported segmentType '{$this->segType}' in VIEW segment <{$this}>", 501); // 501: Not implemented
    }
    
    public function __toString(){
        return $this->view->getLabel() . ":{$this->label}";
    }
    
    /**
     * @param Atom $srcAtom
     * @return mixed
     */
    public function getData($srcAtom){
        switch ($this->segType){
            case "Text":
                return $this->text;
                break;
            case "Exp":
                try {
                    // Try to get view segment from atom query data
                    return $srcAtom->getQueryData('view_' . $this->label); // column is prefixed with view_
                }catch (Exception $e) {
                    // Column not defined, perform query
                    if($e->getCode() == 1001){ // TODO: fix this 1001 exception code handling by proper construct
                        $db = Database::singleton();
                        $srcAtomId = $db->getDBRepresentation($srcAtom);
                        
                        $query = "SELECT DISTINCT `tgt` FROM ({$this->expSQL}) AS `results` WHERE `src` = '{$srcAtomId}' AND `tgt` IS NOT NULL";
                        $tgtAtoms = array_column((array)$db->Exe($query), 'tgt');
                        return count($tgtAtoms) ? $tgtAtoms[0] : null;
                    }else{
                        throw $e;
                    }
                }
                break;
            default:
                throw new Exception("Unsupported segmentType '{$this->segType}' in VIEW segment <{$this}>", 501); // 501: Not implemented
                break;
        }
    }
}

?>