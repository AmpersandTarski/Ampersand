<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Log\Logger;
use Ampersand\Misc\Config;
use Psr\Log\LoggerInterface;
use Ampersand\Plugs\MysqlDB\MysqlDB;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Conjunct
{
    
    /**
     * List of all conjuncts
     *
     * @var \Ampersand\Rule\Conjunct[]
     */
    private static $allConjuncts;
    
    /**
     * Logger
     *
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;

    /**
     * Database to evaluate conjuncts and store violation cache
     *
     * @var \Ampersand\Plugs\MysqlDB\MysqlDB
     */
    protected $database;
    
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
     * [['src' => $srcAtom, 'tgt' => $tgtAtom]]
     *
     * @var array $conjunctViolations
     */
    private $conjunctViolations = null;
    
    /**
     * Conjunct constructor
     * Private function to prevent outside instantiation of conjuncts. Use Conjunct::getConjunct($conjId)
     *
     * @param array $conjDef
     * @param \Psr\Log\LoggerInterface $logger
     */
    private function __construct(array $conjDef, LoggerInterface $logger, MysqlDB $database)
    {
        $this->logger = $logger;

        $this->database = $database;
        
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
    public function __toString(): string
    {
        return $this->id;
    }
    
    /**
     * Check is conjunct is used by/part of a signal rule
     * @return bool
     */
    public function isSigConj(): bool
    {
        return !empty($this->sigRuleNames);
    }
    
    /**
     * Check is conjunct is used by/part of a invariant rule
     * @return bool
     */
    public function isInvConj(): bool
    {
        return !empty($this->invRuleNames);
    }

    /**
     * Get list of rule names that use this conjunct
     *
     * @return string[]
     */
    public function getRuleNames(): array
    {
        return array_merge($this->sigRuleNames, $this->invRuleNames);
    }

    /**
     * Get query to evaluate conjunct violations
     *
     * @return string
     */
    public function getQuery(): string
    {
        return str_replace('_SESSION', session_id(), $this->query); // Replace _SESSION var with current session id.
    }
    
    /**
     * Specificies if conjunct is part of UNI or INJ rule
     * Temporary fuction to be able to skip uni and inj conj
     * TODO: remove after fix for issue #535
     *
     * @return bool
     */
    protected function isUniOrInjConj(): bool
    {
        return array_reduce($this->getRuleNames(), function (bool $carry, string $ruleName) {
            return ($carry || in_array(substr($ruleName, 0, 3), ['UNI', 'INJ']));
        }, false);
    }
    
    /**
     * Evaluate conjunct and return array with violation pairs
     *
     * @param bool $fromCache
     * @return array[] [['src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>']]
     */
    public function evaluate(bool $fromCache = true): array
    {
        $this->logger->debug("Checking conjunct '{$this->id}' (fromCache:" . var_export($fromCache, true) . ")");
        try {
            // Skipping evaluation of UNI and INJ conjuncts. TODO: remove after fix for issue #535
            if (Config::get('skipUniInjConjuncts', 'transactions') && $this->isUniOrInjConj()) {
                $this->logger->debug("Skipping conjunct '{$this}', because it is part of a UNI/INJ rule");
                return [];
            } // If conjunct is already evaluated and conjunctCach may be used -> return violations
            elseif (isset($this->conjunctViolations) && $fromCache) {
                $this->logger->debug("Conjunct is already evaluated, getting violations from cache");
                return $this->conjunctViolations;
            } // Otherwise evaluate conjunct, cache and return violations
            else {
                // Execute conjunct query
                $this->conjunctViolations = (array) $this->database->execute($this->getQuery());
                
                if (($count = count($this->conjunctViolations)) == 0) {
                    $this->logger->debug("Conjunct '{$this->id}' holds");
                } else {
                    $this->logger->debug("Conjunct '{$this->id}' broken: {$count} violations");
                }

                return $this->conjunctViolations;
            }
        } catch (Exception $e) {
            Logger::getUserLogger()->error("Error while checking conjunct '{$this->id}'");
            $this->logger->error($e->getMessage());
            return [];
        }
    }

    public function saveCache()
    {
        $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');

        // Delete existing conjunct violation cache
        $query = "DELETE FROM \"{$dbsignalTableName}\" WHERE \"conjId\" = '{$this->id}'";
        $this->database->execute($query);
        
        // Save new violations (if any)
        if (!empty($this->conjunctViolations)) {
            // Add new conjunct violation to database
            $query = "INSERT IGNORE INTO \"{$dbsignalTableName}\" (\"conjId\", \"src\", \"tgt\") VALUES ";
            $values = [];
            foreach ($this->conjunctViolations as $violation) {
                $values[] = "('{$this->id}', '" . $this->database->escape($violation['src']) . "', '" . $this->database->escape($violation['tgt']) . "')";
            }
            $query .= implode(',', $values);
            $this->database->execute($query);
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
    public static function getConjunct($conjId): Conjunct
    {
        if (!array_key_exists($conjId, $conjuncts = self::getAllConjuncts())) {
            throw new Exception("Conjunct '{$conjId}' is not defined", 500);
        }
    
        return $conjuncts[$conjId];
    }
    
    /**
     * Returns array with all conjunct objects
     *
     * @return \Ampersand\Rule\Conjunct[]
     */
    public static function getAllConjuncts(): array
    {
        if (!isset(self::$allConjuncts)) {
            throw new Exception("Conjunct definitions not loaded yet", 500);
        }
         
        return self::$allConjuncts;
    }
    
    /**
     * Import all role definitions from json file and instantiate Conjunct objects
     *
     * @param string $fileName containing the Ampersand conjunct definitions
     * @param \Psr\Log\LoggerInterface $logger
     * @param \Ampersand\Plugs\MysqlDB\MysqlDB $database
     * @return void
     */
    public static function setAllConjuncts(string $fileName, LoggerInterface $logger, MysqlDB $database)
    {
        self::$allConjuncts = [];
        
        $allConjDefs = (array)json_decode(file_get_contents($fileName), true);
    
        foreach ($allConjDefs as $conjDef) {
            self::$allConjuncts[$conjDef['id']] = new Conjunct($conjDef, $logger, $database);
        }
    }
}
