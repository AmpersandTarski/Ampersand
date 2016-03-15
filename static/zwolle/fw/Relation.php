<?php

class Relation {
    
    /**
     * Contains all relation definitions
     * @var Relation[]
     */
    private static $allRelations;
    
    /**
     * 
     * @var string
     */
    public $signature;
    
    /**
     * 
     * @var string
     */
    public $name;
    
    /**
     * 
     * @var Concept
     */
    public $srcConcept;
    
    /**
     * 
     * @var Concept
     */
    public $tgtConcept;
    
    /**
     * @var boolean
     */
    public $isUni;
    
    /**
     * 
     * @var boolean
     */
    public $isTot;
    
    /**
     * 
     * @var boolean
     */
    public $isInj;
    
    /**
     * 
     * @var boolean
     */
    public $isSur;
    
    /**
     * 
     * @var Conjunct[]
     */
    public $affectedConjuncts = array();
    
    /**
     * 
     * @var Conjunct[]
     */
    private $affectedSigConjuncts = array();
    
    /**
     * 
     * @var Conjunct[]
     */
    private $affectedInvConjuncts = array();
    
    /**
     * Relation constructor
     * Private function to prevent outside instantiation of Relations. Use Relation::getRelation($relationSignature)
     *
     * @param array $relationDef
     */
    public function __construct($relationDef){
        
        $this->name = $relationDef['name'];
        $this->srcConcept = Concept::getConcept($relationDef['srcConcept']);
        $this->tgtConcept = Concept::getConcept($relationDef['tgtConcept']);
        
        // TODO: while old signature is used in interface object definitions, use this old one (rel_<r>_<srcConcept>_<tgtConcept>)
        $this->signature = "rel_{$this->name}_{$this->srcConcept->name}_{$this->tgtConcept->name}"; // $relationDef['signature'];
        
        // TODO: just import when changes are made to json file (see https://github.com/AmpersandTarski/ampersand/issues/197)
        $this->isUni = $relationDef['srcMax'] ? true : false;
        $this->isTot = $relationDef['srcMin'] ? true : false;
        $this->isInj = $relationDef['tgtMax'] ? true : false;
        $this->isSur = $relationDef['tgtMin'] ? true : false;
        
        foreach((array)$relationDef['affectedConjuncts'] as $conjId){
            $conj = Conjunct::getConjunct($conjId);
            
            $this->affectedConjuncts[] = $conj;
        
            if ($conj->isSigConj()) $this->affectedSigConjuncts[] = $conj;
            if ($conj->isInvConj()) $this->affectedInvConjuncts[] = $conj;
            if (!$conj->isSigConj() && !$conj->isInvConj()) Notifications::addInfo("Affected conjunct '{$conj->id}' (specified for relation '{$this->__toString()}') is not part of an invariant or signal rule", 'UnusedConjuncts', "There are unused conjuncts defined");
        }
    }
    
    public function __toString(){
        return "{$this->name}[{$this->srcConcept->name}*{$this->tgtConcept->name}]";
    }
    
    /**
     * Returns array with signal conjuncts that are affected by updating this Relation
     * @return Conjunct[]
     */
    public function getAffectedSigConjuncts(){
        return $this->affectedSigConjuncts;
    }
    
    /**
     * Returns array with invariant conjuncts that are affected by by updating this Relation
     * @return Conjunct[]
     */
    public function getAffectedInvConjuncts(){
        return $this->affectedInvConjuncts;
    }
    
    /**
     * Returns information about database table and columns where this Relation is stored
     * @throws Exception when table information is not available
     * @return array
     */
    public function getTableInfo(){
        global $allRelations;
        global $tableColumnInfo;

        $oldRelSignature = "rel_{$this->name}_{$this->srcConcept->name}_{$this->tgtConcept->name}";
        
        $tableName = $allRelations[$oldRelSignature]['table'];
        $srcColName = $allRelations[$oldRelSignature]['srcCol'];
        $tgtColName = $allRelations[$oldRelSignature]['tgtCol'];
        
        if(!array_key_exists($tableName, $tableColumnInfo)) throw new Exception("Table '{$table}' does not exists in tableColumnInfo", 500);
        if(!array_key_exists($srcColName, $tableColumnInfo[$tableName])) throw new Exception("Column '{$srcColName}' does not exists in table '{$tableName}'", 500);
        if(!array_key_exists($tgtColName, $tableColumnInfo[$tableName])) throw new Exception("Column '{$tgtColName}' does not exists in table '{$tableName}'", 500);
        
        $srcCol = array('header' => $srcColName
                        ,'unique' => $tableColumnInfo[$tableName][$srcColName]['unique']
                        ,'null' => $tableColumnInfo[$tableName][$srcColName]['null']
                        );
        
        $tgtCol = array('header' => $tgtColName
                        ,'unique' => $tableColumnInfo[$tableName][$tgtColName]['unique']
                        ,'null' => $tableColumnInfo[$tableName][$tgtColName]['null']
        );
        
        return array('tableName' => $tableName
                    ,'srcCol' => $srcCol
                    ,'tgtCol' => $tgtCol
                    );
    }

    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return Relation object
     * @param string $relationSignature
     * @throws Exception if Relation is not defined
     * @return Relation
     */
    public static function getRelation($relationSignature, $srcConceptName = null, $tgtConceptName = null){
        $relations = self::getAllRelations();
        
        // If relation can be found by its fullRelationSignature return the relation
        if(array_key_exists($relationSignature, $relations)){
            $relation = $relations[$relationSignature];
            
            // If srcConceptName and tgtConceptName are provided, check that they match the found relation
            if(!is_null($srcConceptName) && $relation->srcConcept->name != $srcConceptName) throw new Exception("Provided src concept [{$srcConceptName}] does not match the found relation '{$relation->__toString()}'", 500);  
            if(!is_null($tgtConceptName) && $relation->tgtConcept->name != $tgtConceptName) throw new Exception("Provided tgt concept [{$tgtConceptName}] does not match the found relation '{$relation->__toString()}'", 500);
            
            return $relation;
        }
        
        // Else try to find the relation by its name, srcConcept and tgtConcept
        if(!is_null($srcConceptName) && !is_null($tgtConceptName)){
            foreach ($relations as $relation){
                if($relation->name == $relationSignature 
                        && $relation->srcConcept->name == $srcConceptName
                        && $relation->tgtConcept->name == $tgtConceptName) return $relation;
            }
        }
        
        // Else
        throw new Exception("Relation '{$relationSignature}' is not defined", 500);
    }
    
    /**
     * Returns array with all Relation objects
     * @return Relation[]
     */
    private static function getAllRelations(){
        if(!isset(self::$allRelations)) self::setAllRelations();
         
        return self::$allRelations;
    }
    
    /**
     * Import all Relation definitions from json file and create and save Relation objects
     * @return void
     */
    private static function setAllRelations(){
        self::$allRelations = array();
    
        // import json file
        $file = file_get_contents(__DIR__ . '/../generics/relations.json');
        $allRelationDefs = (array)json_decode($file, true);
    
        foreach ($allRelationDefs as $relationDef){
            $relation = new Relation($relationDef);
            self::$allRelations[$relation->signature] = $relation; 
        }
    }
}

?>