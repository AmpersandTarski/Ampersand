<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class View {
    
    /**
     * Contains all view definitions
     * @var View[]
     */
    private static $allViews; 
    
    /**
     * Name (and unique identifier) of view
     * @var string
     */
    public $label;
    
    /**
     * Specifies if this view is defined as default view for $this->concept
     * @var boolean
     */
    public $isDefault;
    
    /**
     * Specifies the concpet for which this view can defined
     * @var string
     */
    public $concept;
    
    /**
     * Array with view segments that are used to build the view
     * @var ViewSegment[]
     */
    public $segments = array();
    
    
    /**
     * View constructor
     * Private function to prevent outside instantiation of views. Use View::getView($viewLabel)
     *
     * @param array $viewDef
     */
    private function __construct($viewDef){        
        $this->label = $viewDef['label'];
        $this->forConcept = $viewDef['concept'];
        $this->isDefault = $viewDef['isDefault'];
        
        foreach($viewDef['segments'] as $segment){
            $this->segments[] = new ViewSegment($segment);
        }
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return view object
     * @param string $viewLabel
     * @throws Exception if view is not defined
     * @return View
     */
    public static function getView($viewLabel){
        if(!array_key_exists($viewLabel, $views = self::getAllViews())) throw new Exception("View '{$viewLabel}' is not defined", 500);
    
        return $views[$viewLabel];
    }
    
    /**
     * Returns array with all view objects
     * @return View[]
     */
    private static function getAllViews(){
        if(!isset(self::$allViews)) self::setAllViews();
         
        return self::$allViews;
    }
    
    /**
     * Import all view definitions from json file and create and save View objects 
     * @return void
     */
    private static function setAllViews(){
        self::$allViews = array();
    
        // import json file
        $file = file_get_contents(__DIR__ . '/../generics/views.json');
        $allViewDefs = (array)json_decode($file, true);
    
        foreach ($allViewDefs as $viewDef) self::$allViews[$viewDef['label']] = new View($viewDef);
    }
    
}

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
}

?>