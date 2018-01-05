<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Database\Database;
use Ampersand\Log\Logger;
use Ampersand\Misc\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Conjunct {
    
    /**
     * List of all conjuncts
     * 
     * @var Conjunct[]
     */
    private static $allConjuncts;
    
    /**
     * Logger
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * Conjunct identifier
     * 
     * @var string
     */
    public $id;
    
    /**
     * Query to evaluate conjunct (i.e. get violations)
     * 
     * @var string
     */
    private $query;
    
    /**
     * List invariant rules that use this conjunct
     * 
     * @var string[]
     */
    public $invRuleNames;
    
    /**
     * List signal rules that use this conjunct
     * 
     * @var string[]
     */
    public $sigRuleNames;
    
    /**
     * List of violation pairs 
     * array(array('src' => $srcAtom, 'tgt' => $tgtAtom))
     * 
     * @var array $conjunctViolations
     */
    private $conjunctViolations;
    
    /**
     * Conjunct constructor
     * Private function to prevent outside instantiation of conjuncts. Use Conjunct::getConjunct($conjId)
     *
     * @param array $conjDef
     */
    private function __construct(array $conjDef){
        $this->logger = Logger::getLogger('RULE');
        
        $this->id = $conjDef['id'];
        $this->query = $conjDef['violationsSQL'];
        $this->invRuleNames = (array)$conjDef['invariantRuleNames'];
        $this->sigRuleNames = (array)$conjDef['signalRuleNames'];
    }
    
    /**
     * Function is called when object is treated as a string
     * 
     * @return string identifier of conjunct
     */
    public function __toString(): string {
        return $this->id;
    }
    
    /**
     * Check is conjunct is used by/part of a signal rule
     * @return bool
     */
    public function isSigConj(): bool {
        return !empty($this->sigRuleNames);
    }
    
    /**
     * Check is conjunct is used by/part of a invariant rule
     * @return bool
     */
    public function isInvConj(): bool {
        return !empty($this->invRuleNames);
    }

    /**
     * Get list of rule names that use this conjunct
     *
     * @return string[]
     */
    public function getRuleNames(): array {
        return array_merge($this->sigRuleNames, $this->invRuleNames);
    }

    /**
     * Get query to evaluate conjunct violations
     * 
     * @return string
     */
    public function getQuery(): string {
        return str_replace('_SESSION', session_id(), $this->query); // Replace _SESSION var with current session id.
    }
    
    /**
     * Specificies if conjunct is part of UNI or INJ rule
     * Temporary fuction to be able to skip uni and inj conj
     * TODO: remove after fix for issue #535
     * 
     * @return bool
     */
    protected function isUniOrInjConj(): bool {
        return array_reduce($this->getRuleNames(), function(bool $carry, string $ruleName){
            return ($carry || in_array(substr($ruleName, 0, 3), ['UNI', 'INJ']));
        }, false);
    }
    
    /**
     * Evaluate conjunct and return array with violation pairs
     * 
     * @param bool $cacheConjuncts
     * @return array[] array(array('src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'))
     */
    public function evaluateConjunct(bool $cacheConjuncts = true): array {
        $this->logger->debug("Checking conjunct '{$this->id}' cache:" . var_export($cacheConjuncts, true));
        try{
            // Skipping evaluation of UNI and INJ conjuncts. TODO: remove after fix for issue #535
            if(Config::get('skipUniInjConjuncts', 'transactions') && $this->isUniOrInjConj()){
                $this->logger->debug("Skipping conjunct '{$this}', because it is part of a UNI/INJ rule");
                return [];
            }
            
            // If conjunct is already evaluated and conjunctCach may be used -> return violations
            elseif(isset($this->conjunctViolations) && $cacheConjuncts){
                $this->logger->debug("Conjunct is already evaluated, getting violations from cache");
                return $this->conjunctViolations;
            }

            // Otherwise evaluate conjunct, cache and return violations
            else{
                $db = Database::singleton();
                $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
                $violations = array();
    
                // Execute conjunct query
                $violations = (array)$db->Exe($this->getQuery());
                
                // Cache violations in php Conjunct object
                if($cacheConjuncts) $this->conjunctViolations = $violations;
                
                // Remove "old" conjunct violations from database
                $query = "DELETE FROM `$dbsignalTableName` WHERE `conjId` = '{$this->id}'";
                $db->Exe($query);
                
                if(count($violations) == 0){
                    $this->logger->debug("Conjunct '{$this->id}' holds");
                    
                }else{
                    $this->logger->debug("Conjunct '{$this->id}' broken, updating violations in database");
                        
                    // Add new conjunct violation to database
                    $query = "INSERT IGNORE INTO `$dbsignalTableName` (`conjId`, `src`, `tgt`) VALUES ";
                    $values = array();
                    foreach ($violations as $violation) $values[] = "('{$this->id}', '".$db->escape($violation['src'])."', '".$db->escape($violation['tgt'])."')";
                    $query .= implode(',', $values);
                    $db->Exe($query);
                }
    
                return $violations;
            }
                
        }catch (Exception $e){
            Logger::getUserLogger()->error("While checking conjunct '{$this->id}': " . $e->getMessage());
            return array();
        }
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Return conjunct object
     * 
     * @param string $conjId
     * @throws Exception if conjunct is not defined
     * @return \Ampersand\Rule\Conjunct
     */
    public static function getConjunct($conjId): Conjunct {
        if(!array_key_exists($conjId, $conjuncts = self::getAllConjuncts())) throw new Exception("Conjunct '{$conjId}' is not defined", 500);
    
        return $conjuncts[$conjId];
    }
    
    /**
     * Returns array with all conjunct objects
     * 
     * @return \Ampersand\Rule\Conjunct[]
     */
    public static function getAllConjuncts(): array {
        if(!isset(self::$allConjuncts)) throw new Exception("Conjunct definitions not loaded yet", 500);
         
        return self::$allConjuncts;
    }
    
    /**
     * Import all role definitions from json file and instantiate Conjunct objects
     * 
     * @param string $fileName containing the Ampersand conjunct definitions
     * @return void
     */
    public static function setAllConjuncts(string $fileName){
        self::$allConjuncts = array();
        
        $allConjDefs = (array)json_decode(file_get_contents($fileName), true);
    
        foreach ($allConjDefs as $conjDef) {
            self::$allConjuncts[$conjDef['id']] = new Conjunct($conjDef);
        }
    }
}