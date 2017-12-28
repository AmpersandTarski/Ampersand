<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Exception;
use Ampersand\Core\Concept;
use Ampersand\Log\Logger;
use Ampersand\Misc\Config;
use Ampersand\Database\Database;
use Ampersand\Plugs\ViewPlugInterface;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Rule {

    /**
     * List of all rules
     * 
     * @var \Ampersand\Rule\Rule[]
     */
    private static $allRules;
    
    /**
     * Logger
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;

    /**
     * Dependency injection of an ViewPlug implementation
     * 
     * @var \Ampersand\Plugs\ViewPlugInterface
     */
    public $plug;

    /**
     * Rule identifier
     * 
     * @var string
     */
    public $id;
    
    /**
     * The file and line number of the Ampersand script where this rule is defined
     * 
     * @var string
     */
    protected $origin;
    
    /**
     * The formalized rule in adl
     * 
     * @var string
     */
    protected $ruleAdl;
    
    /**
     * The source concept of this rule
     * 
     * @var \Ampersand\Core\Concept
     */
    public $srcConcept;
    
    /**
     * The target concept of this rule
     * 
     * @var \Ampersand\Core\Concept
     */
    public $tgtConcept;
    
    /**
     * The meaning of this rule (provided in natural language by the Ampersand engineer)
     * 
     * @var string
     */
    protected $meaning;
    
    /**
     * The violation message to display (provided in natural language by the Ampersand engineer)
     * 
     * @var string
     */
    protected $message;
    
    /**
     * List of conjuncts of which this rule is made of
     * 
     * @var \Ampersand\Rule\Conjunct[]
     */
    public $conjuncts;
    
    /**
     * List with segments to build violation messages
     * 
     * @var \Ampersand\Rule\ViolationSegment[]
     */
    protected $violationSegments = [];
    
    /**
     * 
     * @var bool
     */
    protected $isSignal = null;
    
    /**
     * 
     * @var bool
     */
    protected $isInvariant = null;
    
    /**
     * Rule constructor
     * Private function to prevent outside instantiation. Use Rule::getRule($ruleName)
     *
     * @param array $ruleDef
     * @param \Ampersand\Plugs\ViewPlugInterface $plug
     * @param string $type specifies if it is a signal (sig) or invariant (inv) rule
    */
    private function __construct(array $ruleDef, ViewPlugInterface $plug, string $type = null){
        $this->logger = Logger::getLogger('RULE');

        $this->plug = $plug;
        
        $this->id = $ruleDef['name'];
        
        $this->origin = $ruleDef['origin'];
        $this->ruleAdl = $ruleDef['ruleAdl'];
        
        $this->srcConcept = Concept::getConcept($ruleDef['srcConceptId']);
        $this->tgtConcept = Concept::getConcept($ruleDef['tgtConceptId']);
        
        $this->meaning = $ruleDef['meaning'];
        $this->message = $ruleDef['message'];
        
        // Conjuncts
        foreach($ruleDef['conjunctIds'] as $conjId){
            $this->conjuncts[] = Conjunct::getConjunct($conjId);
        }
        
        // Violation segments
        foreach((array)$ruleDef['pairView'] as $segment){
            $this->violationSegments[] = new ViolationSegment($segment, $this);
        }
        
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
    
    /**
     * Function is called when object is treated as a string
     * 
     * @return string
     */
    public function __toString(): string {
        return $this->id;
    }
    
    /**
     * Get message to tell that a rule is broken
     * 
     * @return string
     */
    public function getViolationMessage(): string {
        return $this->message ? $this->message : "Violation of rule '{$this->id}'";
    }

    /**
     * Get list of all violation segment definitions for this rule
     *
     * @return \Ampersand\Rule\ViolationSegment[]
     */
    public function getViolationSegments(): array {
        return $this->violationSegments;
    }
    
    /**
     * Check rule and return violations
     * 
     * @param bool $cacheConjuncts
     * @return \Ampersand\Rule\Violation[]
     */
    public function checkRule(bool $cacheConjuncts = true): array {
        $this->logger->debug("Checking rule '{$this->id}'");
         
        try{
            $violations = [];
    
            // Evaluate conjuncts of this rule
            foreach($this->conjuncts as $conjunct) 
                foreach ($conjunct->evaluateConjunct($cacheConjuncts) as $violation) 
                    $violations[] = new Violation($this, $violation['src'], $violation['tgt']);
                
            // If no violations => rule holds
            if(empty($violations)) $this->logger->debug("Rule '{$this}' holds");
    
            return $violations;
            
        }catch (Exception $e){
            Logger::getUserLogger()->error("While evaluating rule '{$this}': {$e->getMessage()}");
        }
    }
    
    /**********************************************************************************************
     *
     * Static functions
     *
     *********************************************************************************************/

    /**
     * Get rule with a given rule name
     * 
     * @param string $ruleName
     * @throws Exception if rule is not defined
     * @return Rule
     */
    public static function getRule($ruleName): Rule {
        if(!array_key_exists($ruleName, $rules = self::getAllRules())) throw new Exception("Rule '{$ruleName}' is not defined", 500);

        return $rules[$ruleName];
    }
    
    /**
     * Get list with all invariant rules
     * 
     * @return Rule[]
     */
    public static function getAllInvRules(): array {
        $invRules = array();
        foreach (self::getAllRules() as $rule){
            if($rule->isInvariant) $invRules[] = $rule;            
        }
        return $invRules;
    }
    
    /**
     * Get list with all signal rules
     * @return Rule[]
     */
    public static function getAllSigRules(): array {
        $sigRules = array();
        foreach (self::getAllRules() as $rule){
            if($rule->isSignal) $sigRules[] = $rule;
        }
        return $sigRules;
    }

    /**
     * Get list with all rules
     * 
     * @return Rule[]
     */
    public static function getAllRules(): array {
        if(!isset(self::$allRules)) self::setAllRules();
         
        return self::$allRules;
    }

    /**
     * Import all rule definitions from json file and instantiate Rule objects
     * 
     * @return void
     */
    private static function setAllRules(){
        self::$allRules = array();

        // import json file
        $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'rules.json');
        $allRuleDefs = (array)json_decode($file, true);

        $plug = Database::singleton();
        
        // Signal rules
        foreach ($allRuleDefs['signals'] as $ruleDef){
            $rule = new Rule($ruleDef, $plug, 'sig');
            self::$allRules[$rule->id] = $rule;
        }
        
        // Invariant rules
        foreach ($allRuleDefs['invariants'] as $ruleDef){
            $rule = new Rule($ruleDef, $plug, 'inv');
            self::$allRules[$rule->id] = $rule;
        }
    }

}

?>