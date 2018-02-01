<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Exception;
use Ampersand\Core\Atom;
use Ampersand\Interfacing\ViewSegment;
use Ampersand\Plugs\ViewPlugInterface;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class View {
    
    /**
     * Contains all view definitions
     * @var \Ampersand\Interfacing\View[]
     */
    private static $allViews; 
    
    /**
     * Dependency injection of an ViewPlug implementation
     * @var \Ampersand\Plugs\ViewPlugInterface
     */
    public $plug;
    
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
     * @var \Ampersand\Interfacing\ViewSegment[]
     */
    public $segments = [];
    
    
    /**
     * View constructor
     * Private function to prevent outside instantiation of views. Use View::getView($viewLabel)
     *
     * @param array $viewDef
     * @param \Ampersand\Plugs\ViewPlugInterface $plug
     */
    private function __construct($viewDef, ViewPlugInterface $plug){
        $this->plug = $plug;
        
        $this->label = $viewDef['label'];
        $this->forConcept = $viewDef['conceptId'];
        $this->isDefault = $viewDef['isDefault'];
        
        foreach($viewDef['segments'] as $segment){
            $this->segments[] = new ViewSegment($segment, $this);
        }
    }
    
    public function getLabel(){
        return $this->label;
    }
    
    /**
     * @param Atom $srcAtom the atom for which to get the view data
     * @return array
     */
    public function getViewData(Atom $srcAtom){
        $viewData = [];
        foreach ($this->segments as $viewSegment) $viewData[$viewSegment->getLabel()] = $viewSegment->getData($srcAtom);
        return $viewData;
    }

    /**
     * Get specific view segment
     *
     * @param string|int $label
     * @return \Ampersand\Interfacing\ViewSegment
     */
    public function getSegment($label): ViewSegment {
        foreach ($this->segments as $segment) {
            if ($segment->getLabel() == $label) return $segment;
        }
        throw new Exception("View segment '{$this->label}:{$label}' not found", 500);
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
    public static function getAllViews(){
        if(!isset(self::$allViews)) throw new Exception("View definitions not loaded yet", 500);
         
        return self::$allViews;
    }
    
    /**
     * Import all view definitions from json file and instantiate View objects 
     * 
     * @param string $fileName containing the Ampersand view definitions
     * @param \Ampersand\Plugs\ViewPlugInterface $defaultPlug
     * @return void
     */
    public static function setAllViews(string $fileName, ViewPlugInterface $defaultPlug){
        self::$allViews = [];
        
        $allViewDefs = (array)json_decode(file_get_contents($fileName), true);
        
        foreach ($allViewDefs as $viewDef){
            self::$allViews[$viewDef['label']] = new View($viewDef, $defaultPlug);
        }
    }
    
}
