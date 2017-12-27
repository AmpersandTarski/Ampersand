<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Ampersand\Database\Database;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Config;
use Ampersand\AmpersandApp;
use Ampersand\Role;
use Ampersand\Interfacing\Transaction;
use Ampersand\Rule\Rule;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class RuleEngine {
    
    /**
     * Evaluate invariant rules for the provided transaction and return list of violations
     * 
     * @param \Ampersand\Interfacing\Transaction[] $transaction
     * @return \Ampersand\Rule\Violation[]
     */
    public static function checkInvariantRules(Transaction $transaction): array {
        $logger = Logger::getLogger('RULEENGINE');
        $affectedConjuncts = $transaction->getAffectedConjuncts('inv'); // Get affected invariant conjuncts
        
        $violations = [];
        foreach(Rule::getAllInvRules() as $rule){
            if(array_intersect($rule->conjuncts, $affectedConjuncts)){
                $logger->debug("Checking invariant rule '{$rule}'");
                $violations = array_merge($violations, $rule->checkRule(true)); // cache conjunct = true, because multiple rules can share the same conjunct
            }else{
                $logger->debug("Skipping invariant rule '{$rule}', because it is NOT affected in {$transaction}");
            }
        }
        
        return $violations;
    }

    /**
     * Get rules that are maintained by $role
     * If $transaction is provided then only affected rules in the transaction are checked
     *
     * @param \Ampersand\Role $role
     * @param \Ampersand\Interfacing\Transaction $transaction
     * @return \Ampersand\Rule\Rule[]
     */
    public static function getRulesToCheck(Role $role, Transaction $transaction = null): array {
        // Determine which rules to check/fix
        if(isset($transaction)){
            // Determine affected rules that must be checked by the exec engine
            $affectedConjuncts = $transaction->getAffectedConjuncts('sig');
            $ruleNames = [];
            foreach($affectedConjuncts as $conjunct) $ruleNames = array_merge($ruleNames, $conjunct->sigRuleNames);
            
            return array_map(function($ruleName){
                return Rule::getRule($ruleName);
            }, $ruleNames);
        }else{
            return $role->maintains();
        }
    }
    
    /**
     * 
     * @return \Ampersand\Rule\Violation[]
     */
    public static function getSignalViolationsFromDB(){
        $logger = Logger::getLogger('RULEENGINE');
        $ampersandApp = AmpersandApp::singleton();
        $database = Database::singleton();
        $dbsignalTableName = Config::get('dbsignalTableName', 'mysqlDatabase');
        
        $conjuncts = array();
        $conjunctRuleMap = array();
        foreach ($ampersandApp->getRulesToMaintain() as $rule){
            foreach($rule->conjuncts as $conjunct) $conjunctRuleMap[$conjunct->id][] = $rule;
            $conjuncts = array_merge($conjuncts, $rule->conjuncts);
        }
        $conjuncts = array_unique($conjuncts); // remove duplicates
        
        $violations = array();
        if(count($conjuncts) > 0){
            $q = implode(',', array_map( function($conj){ return "'{$conj->id}'";}, $conjuncts)); // returns string "<conjId1>,<conjId2>,<etc>"
            $query = "SELECT * FROM `{$dbsignalTableName}` WHERE `conjId` IN ({$q})";
            $result = $database->Exe($query); // array(array('conjId' => '<conjId>', 'src' => '<srcAtomId>', 'tgt' => '<tgtAtomId>'))
            foreach ($result as $row){
                foreach($conjunctRuleMap[$row['conjId']] as $rule){
                   $violations[] = new Violation($rule, $row['src'], $row['tgt']);
                }
            }
        }else{
            $logger->debug("No conjuncts to check (it can be that this role does not maintain any rule)");
        }
        return $violations;
    }
}

?>