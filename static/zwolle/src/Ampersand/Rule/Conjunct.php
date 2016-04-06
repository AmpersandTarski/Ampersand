<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;
use Ampersand\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Conjunct {
    
    /**
     * Contains all conjunct definitions
     * @var Conjunct[]
     */
    private static $allConjuncts;
    
    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
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
     * Array of arrays with violation pairs array(array('src' => $srcAtom, 'tgt' => $tgtAtom))
     * @var array $conjunctViolations
     */
    private $conjunctViolations;
    
    /**
     * Conjunct constructor
     * Private function to prevent outside instantiation of conjuncts. Use Conjunct::getConjunct($conjId)
     *
     * @param array $conjDef
     */
    private function __construct($conjDef){
        $this->logger = Logger::getLogger('FW');
        
        $this->id = $conjDef['id'];
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
    
    /**
     * Function to evaluate conjunct
     * @param boolean $cacheConjuncts
     * @return array[] array(array('src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'))
     */
    public function evaluateConjunct($cacheConjuncts = true){
        $this->logger->debug("Checking conjunct '{$this->id}' cache:" . var_export($cacheConjuncts, true));
        try{
            	
            // If conjunct is already evaluated and conjunctCach may be used -> return violations
            if(isset($this->conjunctViolations) && $cacheConjuncts){
                $this->logger->debug("Conjunct is already evaluated, getting violations from cache");
                return $this->conjunctViolations;
                	
                // Otherwise evaluate conjunct, cache and return violations
            }else{
                $db = Database::singleton();
                $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
                $violations = array();
    
                // Execute conjunct query
                $violations = (array)$db->Exe($this->query);
                
                // Cache violations in php Conjunct object
                if($cacheConjuncts) $this->conjunctViolations = $violations;
    
                if(count($violations) == 0){
                    $this->logger->debug("Conjunct '{$this->id}' holds");
                    	
                    // Remove "old" conjunct violations from database
                    $query = "DELETE FROM `$dbsignalTableName` WHERE `conjId` = '{$this->id}'";
                    $db->Exe($query);
                    	
                }else{
                    $this->logger->debug("Conjunct '{$this->id}' broken, updating violations in database");
                    
                    // Remove "old" conjunct violations from database
                    $query = "DELETE FROM `$dbsignalTableName` WHERE `conjId` = '{$this->id}'";
                    $db->Exe($query);
                    	
                    // Add new conjunct violation to database
                    $query = "INSERT IGNORE INTO `$dbsignalTableName` (`conjId`, `src`, `tgt`) VALUES ";
                    $values = array();
                    foreach ($violations as $violation) $values[] = "('{$this->id}', '".$violation['src']."', '".$violation['tgt']."')";
                    $query .= implode(',', $values);
                    $db->Exe($query);
                }
    
                return $violations;
            }
            	
        }catch (Exception $e){
            Logger::getUserLogger()->error("While checking conjunct '{$this->id}': " . $e->getMessage());
        }
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
        $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'conjuncts.json');
        $allConjDefs = (array)json_decode($file, true);
    
        foreach ($allConjDefs as $conjDef) self::$allConjuncts[$conjDef['id']] = new Conjunct($conjDef);
    }
    
    /**
     * 
     * @param Conjunct[] $conjuncts
     * @return void
     */
    public static function evaluateConjuncts($conjuncts = null, $cacheConjuncts = true){
        if(is_null($conjuncts)) $conjuncts = self::getAllConjuncts();
        
        foreach($conjuncts as $conjunct) $conjunct->evaluateConjunct($cacheConjuncts);
    }
    
}