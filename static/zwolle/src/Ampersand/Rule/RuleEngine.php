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

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class RuleEngine {
    
    /**
     * Evaluate invariant rules for the provided affected concepts and relations
     * @param Concept[] $affectedConcepts
     * @param Relation[] $affectedRelations
     * @param boolean $cacheConjuncts
     * @return boolean if invariant rules hold
     */
    public static function checkInvariantRules($affectedConcepts = [], $affectedRelations = [], $cacheConjuncts = true){
        $logger = Logger::getLogger('RULE');
        $conjuncts = self::getAffectedConjuncts($affectedConcepts, $affectedRelations, 'inv'); // Get affected invariant conjuncts
    
        // check invariant rules
        $logger->debug("Checking invariant rules for provided conjuncts: " . implode(', ', array_column($conjuncts, 'id')));
        
        $invariantRulesHold = true;
        foreach ($conjuncts as $conjunct){
            if($conjunct->isInvConj()){
                foreach ($conjunct->evaluateConjunct($cacheConjuncts) as $violation){
                    // If a conjunct is broken (i.e. returns 1 or more violation pairs) mark that invariant rules do not hold
                    $invariantRulesHold = false;
                    foreach ($conjunct->invRuleNames as $ruleName) Notifications::addInvariant(new Violation(Rule::getRule($ruleName), $violation['src'], $violation['tgt']));
                }
            }else{
                $logger->error("Conjunct '{$conjunct->id}' provided to be checked for invariant violations, but this is not an invariant conjunct");
            }
        }
        return $invariantRulesHold;
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
            $affectedConjuncts = self::getAffectedConjuncts($transaction->getAffectedConcepts(), $transaction->getAffectedRelations(), 'sig');
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
     * @param boolean $cacheConjuncts
     * @return void
     */
    public static function checkProcessRules($cacheConjuncts = true){
        $logger = Logger::getLogger('RULE');
        $ampersandApp = AmpersandApp::singleton();
        
        $logger->debug("Checking process rules for active roles: " . implode(', ', array_column($ampersandApp->getActiveRoles(), 'label')));
        
        foreach ($ampersandApp->getRulesToMaintain() as $rule){
            $violations = $rule->getViolations($cacheConjuncts);
            foreach ($violations as $violation) Notifications::addSignal($violation);
        }    
    }
    
    /**
     * 
     * @param Concept[] $affectedConcepts
     * @param Relation[] $affectedRelations
     * @param string $ruleType
     * @return Conjunct[]
     */
    public static function getAffectedConjuncts($affectedConcepts, $affectedRelations, $ruleType = 'both'){
        
        $affectedConjuncts = array();
        
        // Get conjuncts for Concepts
        foreach($affectedConcepts as $concept){
            // Invariant conjuncts
            if($ruleType == 'inv' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedInvConjuncts());
            // Signal conjuncts
            if($ruleType == 'sig' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $concept->getAffectedSigConjuncts());
        }
        
        // Get conjuncts for Relations
        foreach($affectedRelations as $relation){
            // Invariant conjuncts
            if($ruleType == 'inv' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedInvConjuncts());
            // Signal conjuncts
            if($ruleType == 'sig' || $ruleType == 'both') $affectedConjuncts = array_merge($affectedConjuncts, $relation->getAffectedSigConjuncts());
        }
        
        return array_unique($affectedConjuncts); // remove duplicate entries.
    }
    
    /**
     * 
     * @return Violation[]
     */
    public static function getSignalViolationsFromDB(){
        $logger = Logger::getLogger('RULE');
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