<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Generator;
use Ampersand\Log\Logger;
use Ampersand\Misc\Config;
use Psr\Log\LoggerInterface;
use Ampersand\Plugs\MysqlDB\MysqlDB;
use Psr\Cache\CacheItemPoolInterface;

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
     * Cache pool that contains conjunct violations
     *
     * @var \Psr\Cache\CacheItemPoolInterface
     */
    protected static $conjunctCache;
    
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
     * Undocumented variable
     *
     * @var \Psr\Cache\CacheItemPoolInterface
     */
    protected $cachePool;
    
    /**
     * Undocumented variable
     *
     * @var \Psr\Cache\CacheItemInterface
     */
    protected $cacheItem;

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
     * Specifies if conjunct is already evaluated
     *
     * @var bool
     */
    protected $isEvaluated = false;
    
    /**
     * Conjunct constructor
     * Private function to prevent outside instantiation of conjuncts. Use Conjunct::getConjunct($conjId)
     *
     * @param array $conjDef
     * @param \Psr\Log\LoggerInterface $logger
     */
    private function __construct(array $conjDef, LoggerInterface $logger, MysqlDB $database, CacheItemPoolInterface $cachePool)
    {
        $this->logger = $logger;

        $this->database = $database;
        
        $this->id = $conjDef['id'];
        $this->query = $conjDef['violationsSQL'];
        $this->invRuleNames = (array)$conjDef['invariantRuleNames'];
        $this->sigRuleNames = (array)$conjDef['signalRuleNames'];

        $this->cachePool = $cachePool;
        $this->cacheItem = $cachePool->getItem($this->id);
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
     * Get violation pairs of this conjunct
     *
     * @param boolean $forceReEvaluation
     * @return array[] [['conjId' => '<conjId>', 'src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'], [], ..]
     */
    public function getViolations(bool $forceReEvaluation = false): array
    {
        // Skipping evaluation of UNI and INJ conjuncts. TODO: remove after fix for issue #535
        if (Config::get('skipUniInjConjuncts', 'transactions') && $this->isUniOrInjConj()) {
            $this->logger->debug("Skipping conjunct '{$this}', because it is part of a UNI/INJ rule");
            return [];
        }
        
        // If re-evaluation is forced
        if ($forceReEvaluation || !$this->cacheItem->isHit()) {
            $this->evaluate();
            return $this->cacheItem->get();
        }

        // Otherwise get from cache
        $this->logger->debug("Conjunct is already evaluated, getting violations from cache");
        return $this->cacheItem->get();
    }
    
    /**
     * Evaluate conjunct and return array with violation pairs
     *
     * @return $this
     */
    public function evaluate(): Conjunct
    {
        $this->logger->debug("Evaluating conjunct '{$this->id}'");
        
        try {
            // Execute conjunct query
            $violations = array_map(function (array $pair) {
                // Adds conjunct id to every pair
                $pair['conjId'] = $this->id;
                return $pair;
            }, $this->database->execute($this->getQuery()));

            $this->isEvaluated = true;
            $this->cacheItem->set($violations);
            $this->cachePool->saveDeferred($this->cacheItem);
            
            if (($count = count($violations)) == 0) {
                $this->logger->debug("Conjunct '{$this->id}' holds");
            } else {
                $this->logger->debug("Conjunct '{$this->id}' broken: {$count} violations");
            }

            return $this;
        } catch (Exception $e) {
            Logger::getUserLogger()->error("Error while evaluating conjunct '{$this->id}'");
            $this->logger->error($e->getMessage());

            return $this;
        }
    }

    public function persistCacheItem()
    {
        $this->cachePool->save($this->cacheItem);
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/
    
    /**
     * Get conjunct violations (if possible from cache) for given set of conjuncts
     *
     * @param \Ampersand\Rule\Conjunct[] $conjuncts
     * @return \Generator
     */
    public static function getConjunctViolations(array $conjuncts = []): Generator
    {
        // Foreach conjunct provided, check if there is a hit in cache (i.e. ->isHit())
        $hits = $nonHits = [];
        foreach ($conjuncts as $conjunct) {
            /** @var \Ampersand\Rule\Conjunct $conjunct */
            if ($conjunct->cacheItem->isHit()) {
                $hits[] = $conjunct->id;
            } else {
                $nonHits[] = $conjunct;
            }
        }

        // For all hits, use CachPoolInterface->getItems()
        foreach (self::$conjunctCache->getItems($hits) as $cacheItem) {
            /** @var \Psr\Cache\CacheItemInterface $cacheItem */
            yield from $cacheItem->get();
        }

        // For all non-hits, get violations from Conjunct object
        foreach ($nonHits as $conjunct) {
            yield from $conjunct->getViolations();
        }
    }

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
    public static function setAllConjuncts(string $fileName, LoggerInterface $logger, MysqlDB $database, CacheItemPoolInterface $cachePool)
    {
        self::$allConjuncts = [];
        self::$conjunctCache = $cachePool;
        
        $allConjDefs = (array)json_decode(file_get_contents($fileName), true);
    
        foreach ($allConjDefs as $conjDef) {
            self::$allConjuncts[$conjDef['id']] = new Conjunct($conjDef, $logger, $database, $cachePool);
        }
    }
}
