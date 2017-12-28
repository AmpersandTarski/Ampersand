<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Ampersand\Database\Database;
use Ampersand\Misc\Config;
use Ampersand\Rule\Violation;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class RuleEngine {

    /**
     * Function to check and/or get violations for a set of rules
     * By default, violations are queries from cache in the database
     *
     * @param \Ampersand\Rule\Rule[] $rules set of rules to check
     * @param bool $forceEvaluate force (re)evaluation of rule
     * @return \Ampersand\Rule\Violation[]
     */
    public static function checkRules(array $rules, bool $forceEvaluate = false): array {
        
        // Evaluate rules
        if($forceEvaluate){
            $violations = [];
            foreach ($rules as $rule){
                $violations = array_merge($violations, $rule->checkRule(true)); // cache conjunct = true, because multiple rules can share the same conjunct
            }
            return $violations;
        }
        
        // Get violations from database table
        else{
            return self::getViolationsFromDB($rules);
        }
    }
    
    /**
     * Get violations for set of rules from database cache
     * 
     * @param \Ampersand\Rule\Rule[] $rules set of rules for which to query the violations
     * @return \Ampersand\Rule\Violation[]
     */
    protected static function getViolationsFromDB(array $rules): array{
        $database = Database::singleton();
        $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');

        // Determine conjuncts to select from database
        $conjuncts = [];
        $conjunctRuleMap = []; // needed because violations are instantiated per rule (not per conjunct)
        foreach ($rules as $rule){
            foreach($rule->conjuncts as $conjunct) $conjunctRuleMap[$conjunct->id][] = $rule;
            $conjuncts = array_merge($conjuncts, $rule->conjuncts);
        }
        $conjuncts = array_unique($conjuncts); // remove duplicates
        
        // Query database
        $q = implode(',', array_map( function($conj){ return "'{$conj->id}'";}, $conjuncts)); // returns string "<conjId1>,<conjId2>,<etc>"
        $query = "SELECT * FROM `{$dbsignalTableName}` WHERE `conjId` IN ({$q})";
        $result = $database->Exe($query); // array(array('conjId' => '<conjId>', 'src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'))

        // Return violation
        $violations = [];
        foreach ($result as $row){
            foreach($conjunctRuleMap[$row['conjId']] as $rule){
                $violations[] = new Violation($rule, $row['src'], $row['tgt']);
            }
        }
        return $violations;
    }
}

?>