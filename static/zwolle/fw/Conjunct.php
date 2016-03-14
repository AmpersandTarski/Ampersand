<?php

Class Conjunct {
    
    /**
     * Contains all conjunct definitions
     * @var Conjunct[]
     */
    private static $allConjuncts;
    
    /**
     * 
     * @var string
     */
    public $id;
    
    /**
     * 
     * @var string
     */
    public $query;
    
    /**
     * 
     * @var array
     */
    public $invRuleNames;
    
    /**
     * 
     * @var array
     */
    public $sigRuleNames;
    
    /**
     * Conjunct constructor
     * Private function to prevent outside instantiation of conjuncts. Use Conjunct::getConjunct($conjId)
     *
     * @param array $conjDef
     */
    private function __construct($conjDef){
        $this->id = $conjDef['Id'];
        $this->query = $conjDef['violationsSQL'];
        $this->invRuleNames = (array)$conjDef['invariantRuleNames'];
        $this->sigRuleNames = (array)$conjDef['signalRuleNames'];
    }
    
    /**
     * Returns identifier of conjunct
     * This method is required for array_unique() to work elsewhere in the code
     * @return string
     */
    public function __toString(){
        return $this->id;
    }
    
    /**
     * Check is conjunct is used by/part of a signal rule
     * @return boolean
     */
    public function isSigConj(){
        return !empty($this->sigRuleNames);
    }
    
    /**
     * Check is conjunct is used by/part of a invariant rule
     * @return boolean
     */
    public function isInvConj(){
        return !empty($this->invRuleNames);
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return conjunct object
     * @param string $conjId
     * @throws Exception if conjunct is not defined
     * @return Conjunct
     */
    public static function getConjunct($conjId){
        if(!array_key_exists($conjId, $conjuncts = self::getAllConjuncts())) throw new Exception("Conjunct '{$conjId}' is not defined", 500);
    
        return $conjuncts[$conjId];
    }
    
    /**
     * Returns array with all conjunct objects
     * @return Conjunct[]
     */
    private static function getAllConjuncts(){
        if(!isset(self::$allConjuncts)) self::setAllConjuncts();
         
        return self::$allConjuncts;
    }
    
    /**
     * Import all conjunct definitions from json file and create and save Conjunct objects
     * @return void
     */
    private static function setAllConjuncts(){
        self::$allConjuncts = array();
    
        // import json file
        $file = file_get_contents(__DIR__ . '/../generics/conjuncts.json');
        $allConjDefs = (array)json_decode($file, true);
    
        foreach ($allConjDefs as $conjDef) self::$allConjuncts[$conjDef['Id']] = new Conjunct($conjDef);
    }
    
}