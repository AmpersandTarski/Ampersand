<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Core\Concept;
use Ampersand\Log\Logger;
use Ampersand\Config;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Rule {

    /**
     * Contains all rule definitions
     * @var Rule[]
     */
    private static $allRules;
    
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
     * Specifies the file and line number of the Ampersand script where this rule is defined
     * @var string
     */
    public $origin;
    
    /**
     * Specifies the formalized rule in adl
     * @var string
     */
    public $ruleAdl;
    
    /**
     * Specifies the source concept of this rule
     * @var Concept
     */
    public $srcConcept;
    
    /**
     * Specifies the target concept of this rule
     * @var Concept
     */
    public $tgtConcept;
    
    /**
     * Specifies the meaning of this rule (provided in natural language by the Ampersand engineer)
     * @var string
     */
    public $meaning;
    
    /**
     * Specifies the violation message to display (provided in natural language by the Ampersand engineer)
     * @var string
     */
    public $message;
    
    /**
     * Array of conjuncts of which this rule is made of
     * @var Conjunct[]
     */
    public $conjuncts;
    
    /**
     * Array with segments to build violation messages
     * @var array
     */
    public $violationSegments;
    
    /**
     * 
     * @var boolean
     */
    public $isSignal = null;
    
    /**
     * 
     * @var boolean
     */
    public $isInvariant = null;

    /**
     * 
     * @var array
     */
    private $violations;
    
    /**
     * Rule constructor
     * Private function to prevent outside instantiation. Use Rule::getRule($ruleName)
     *
     * @param array $ruleDef
     * @param boolean $type
    */
    private function __construct($ruleDef, $type = null){
        $this->logger = Logger::getLogger('FW');
        
        $this->id = $ruleDef['name'];
        
        $this->origin = $ruleDef['origin'];
        $this->ruleAdl = $ruleDef['ruleAdl'];
        
        $this->srcConcept = Concept::getConcept($ruleDef['srcConcept']);
        $this->tgtConcept = Concept::getConcept($ruleDef['tgtConcept']);
        
        $this->meaning = $ruleDef['meaning'];
        $this->message = $ruleDef['message'];
        
        // Conjuncts
        foreach($ruleDef['conjunctIds'] as $conjId){
            $this->conjuncts[] = Conjunct::getConjunct($conjId);
        }
        
        $this->violationSegments = (array)$ruleDef['pairView'];
        
        switch($type){
            case 'sig' :
                $this->isSignal = true;
                break;
            case 'inv' :
                $this->isInvariant = true;
                break;
            case null :
                break;
            default : throw new Exception ("Unknown/unsupported rule type. Allowed types are signal or invariant", 500);
        }
    }
    
    public function __toString(){
        return $this->id;
    }
    
    /**
     * 
     * @return string
     */
    public function getViolationMessage(){
        return $this->message ? $this->message : "Violation of rule '{$this->id}'";
    }
    
    /**
     *
     * @param boolean $cacheConjuncts
     * @return Violation[]
     */
    private function checkRule($cacheConjuncts = true){
        $this->logger->debug("Checking rule '{$this->id}'");
         
        try{
            $violations = array();
    
            // Evaluate conjuncts of this rule
            foreach($this->conjuncts as $conjunct) 
                foreach ($conjunct->evaluateConjunct($cacheConjuncts) as $violation) 
                    $violations[] = new Violation($this, $violation['src'], $violation['tgt']);
            	
            // If no violations => rule holds
            if(empty($violations)) $this->logger->debug("Rule '{$this->id}' holds");
            
            // Cache violations when requested
            if($cacheConjuncts) $this->violations = $violations;
    
            return $violations;
            
        }catch (Exception $e){
            Logger::getUserLogger()->error("While evaluating rule '{$this->id}': {$e->getMessage()}");
        }
    }
    
    /**
     * 
     * @param string $cacheConjuncts
     * @return Violation[]
     */
    public function getViolations($cacheConjuncts = true){
        if(isset($this->violations) && $cacheConjuncts) return $this->violations;
        else return $this->checkRule($cacheConjuncts);
        
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/

    /**
     * Return rule object
     * @param string $ruleName
     * @throws Exception if rule is not defined
     * @return Rule
     */
    public static function getRule($ruleName){
        if(!array_key_exists($ruleName, $rules = self::getAllRules())) throw new Exception("Rule '{$ruleName}' is not defined", 500);

        return $rules[$ruleName];
    }
    
    /**
     * Returns array with all invariant rule objects
     * @return Rule[]
     */
    public static function getAllInvRules(){
        $invRules = array();
        foreach (self::getAllRules() as $rule){
            if($rule->isInvariant) $invRules[] = $rule;            
        }
        return $invRules;
    }
    
    /**
     * Returns array with all signal rule objects
     * @return Rule[]
     */
    public static function getAllSigRules(){
        $sigRules = array();
        foreach (self::getAllRules() as $rule){
            if($rule->isSignal) $sigRules[] = $rule;
        }
        return $sigRules;
    }

    /**
     * Returns array with all rule objects
     * @return Rule[]
     */
    public static function getAllRules(){
        if(!isset(self::$allRules)) self::setAllRules();
         
        return self::$allRules;
    }

    /**
     * Import all rule definitions from json file and create and save Rule objects
     * @return void
     */
    private static function setAllRules(){
        self::$allRules = array();

        // import json file
        $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'rules.json');
        $allRuleDefs = (array)json_decode($file, true);
        
        // Signal rules
        foreach ($allRuleDefs['signals'] as $ruleDef){
            $rule = new Rule($ruleDef, 'sig');
            self::$allRules[$rule->id] = $rule;
        }
        
        // Invariant rules
        foreach ($allRuleDefs['invariants'] as $ruleDef){
            $rule = new Rule($ruleDef, 'inv');
            self::$allRules[$rule->id] = $rule;
        }
    }

}

?>