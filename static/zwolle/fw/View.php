<?php

class View {
    
    /**
     * 
     * @var array
     */
    private static $allViewDefinitions; 
    
    /**
     * 
     * @var string
     */
    public $label;
    
    /**
     * 
     * @var boolean
     */
    public $isDefault;
    
    /**
     * 
     * @var string
     */
    public $concept;
    
    /**
     * 
     * @var array
     */
    public $segments = array();
    
    
    /**
     * 
     * @param string $viewLabel
     */
    public function __construct($viewLabel, Database $db){
        $view = self::getViewDefinition($viewLabel);
        
        $this->label = $view['label'];
        $this->concept = $view['concept'];
        $this->isDefault = $view['isDefault'];
        
        foreach($view['segments'] as $segment){
            $this->segments[] = new ViewSegment($segment, $this->label, $db);
        }
    }
    
    public function getView(Atom $atom){
        // Check if view can be used by atom
        if ($atom->concept != $this->concept
                && !in_array($atom->concept, Concept::getSpecializations($this->concept))) throw new Exception ("Specified view is not applicable for {$atom->id}[{$atom->concept}]",500);
        
        $viewStrs = array();
        foreach ($this->segments as $viewSegment){
            $viewStrs[$viewSegment->label] = $viewSegment->getValue($atom);
        }
        
        return $viewStrs;
        
    }
    
    /**
     * 
     * @return array:
     */
    private static function getAllViewDefinitions(){
        // If self::$allRules is not set yet, import views from json file
        if(!isset(self::$allViewDefinitions)){
            $file = file_get_contents(__DIR__ . '/../generics/views.json');
            self::$allViewDefinitions = (array)json_decode($file, true);
        }
        
        return self::$allViewDefinitions;
    }
    
    /**
     * 
     * @param string $viewLabel
     * @throws Exception when specified view does not exists
     * @return array
     */
    private static function getViewDefinition($viewLabel){
        foreach(self::getAllViewDefinitions() as $view){
            if($view['label'] == $viewLabel) return $view;
        }
        
        // If no view found and returned, throw exception
        throw new Exception ("Specified view '{$viewLabel}' does not exists", 500);
    }
}

class ViewSegment {
    
    /**
     * @var Database
     */
    private $db;
    
    /**
     * 
     * @var string
     */
    public $viewLabel;
    
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
    

    
    public function __construct($segment, $viewLabel, Database $db){
        $this->db = $db;
        
        $this->viewLabel = $viewLabel;
        
        $this->seqNr = $segment['segNr'];
        $this->label = $segment['label'];
        $this->segType = $segment['segType'];
        $this->text = $segment['text'];
        $this->expADL = $segment['expADL'];
        $this->expSQL = $segment['expSQL'];
    }
    
    
    public function getValue(Atom $atom){
        switch ($this->segType){
            case "Text" :
                return $this->text;
                break;
            case "Exp" :
                $query = "SELECT DISTINCT `tgt` FROM ({$this->expSQL}) AS `results` WHERE `src` = '{$atom->idEsc}' AND `tgt` IS NOT NULL";
                $tgtAtoms = array_column((array)$this->db->Exe($query), 'tgt');
                return count($tgtAtoms) ? $tgtAtoms[0] : null;
                break;
            default :
                throw new Exception("Unsupported segmentType '{$this->segType}' in view '{$this->viewLabel}' segment '{$this->seqNr}:{$this->label}'", 501); // 501: Not implemented
                break;
        }
    }
    
}

?>