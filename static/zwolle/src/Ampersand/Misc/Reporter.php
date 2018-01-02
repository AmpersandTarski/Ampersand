<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Misc;

use Exception;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\IO\AbstractWriter;
use Ampersand\Rule\Conjunct;

class Reporter {

    /**
     * Writer 
     * 
     * @var \Ampersand\IO\AbstractWriter
     */
    protected $writer;

    public function __construct(AbstractWriter $writer){
        $this->writer = $writer;
    }

    public function __toString(){
        return $this->writer->getContent();
    }

    /**
     * Write and return relation definition report
     * 
     * Specifies multiplicity constraints, related conjuncts and other aspects of all relations
     *
     * @return array
     */
    public function reportRelationDefinitions(){
        $content = array_map(function($relation){
            $relArr = [];
            
            $relArr['signature'] = $relation->signature;
            
            // Get multiplicity constraints
            $constraints = [];
            if($relation->isUni) $constraints[] = "[UNI]";
            if($relation->isTot) $constraints[] = "[TOT]";
            if($relation->isInj) $constraints[] = "[INJ]";
            if($relation->isSur) $constraints[] = "[SUR]";
            $relArr['constraints'] = empty($constraints) ? "no constraints" : implode(',', $constraints);
            
            $relArr['affectedConjuncts'] = [];
            foreach($relation->affectedConjuncts as $conjunct){
                $relArr['affectedConjuncts'][$conjunct->id] = [];
                foreach ($conjunct->invRuleNames as $ruleName) $relArr['affectedConjuncts'][$conjunct->id]['invRules'][] = $ruleName;
                foreach ($conjunct->sigRuleNames as $ruleName) $relArr['affectedConjuncts'][$conjunct->id]['sigRules'][] = $ruleName;
            }
            $relArr['srcOrTgtTable'] = $relation->getMysqlTable()->tableOf;
            
            return $relArr;
        }, Relation::getAllRelations());

        $this->writer->write($content);
        
        return $content;
    }

    /**
     * Write and return interface report
     * 
     * Specifies aspects for all interfaces (incl. subinterfaces), like path, label, crud-rights, etc
     * 
     * @return array
     */
    public function reportInterfaceDefinitions(){
        $content = [];
        foreach (InterfaceObject::getAllInterfaces() as $key => $ifc) {
            $content = array_merge($content, $ifc->getInterfaceFlattened());
        }
        
        $content = array_map(function(InterfaceObject $ifc){
            return array( 'path' => $ifc->getPath()
                        , 'label' => $ifc->label
                        , 'crudC' => $ifc->crudC()
                        , 'crudR' => $ifc->crudR()
                        , 'crudU' => $ifc->crudU()
                        , 'crudD' => $ifc->crudD()
                        , 'src' => $ifc->srcConcept->name
                        , 'tgt' => $ifc->tgtConcept->name
                        , 'view' => $ifc->getView()->label
                        , 'relation' => $ifc->relation->signature
                        , 'flipped' => $ifc->relationIsFlipped
                        , 'ref' => $ifc->getRefToIfcId()
                        , 'root' => $ifc->isRoot()
                        , 'public' => $ifc->isPublic()
                        , 'roles' => implode(',', $ifc->ifcRoleNames)
                    );
            
        }, $content);

        $this->writer->write($content);

        return $content;
    }

    /**
     * Write and return conjunct usage report
     * 
     * Specifies which conjuncts are used by which rules, grouped by invariants, signals, and unused conjuncts
     *
     * @return array
     */
    public function reportConjunctUsage(){
        $content = [];
        foreach(Conjunct::getAllConjuncts() as $conj){        
            if($conj->isInvConj()) $content['invConjuncts'][] = $conj->__toString();
            if($conj->isSigConj()) $content['sigConjuncts'][] = $conj->__toString();
            if(!$conj->isInvConj() && !$conj->isSigConj()) $content['unused'][] = $conj->__toString();
        }

        $this->writer->write($content);

        return $content;
    }

    /**
     * Write and return conjunct performance report
     *
     * @param Conjunct[] $conjuncts
     * @return array
     */
    public function reportConjunctPerformance(array $conjuncts){
        $content = [];
        
        // run all conjuncts (from - to)
        foreach($conjuncts as $conjunct){
            /** @var \Ampersand\Rule\Conjunct $conjunct */
            $startTimeStamp = microtime(true); // true means get as float instead of string
            $conjunct->evaluateConjunct(false);
            $endTimeStamp = microtime(true);
            set_time_limit ((int) ini_get('max_execution_time')); // reset time limit counter
            
            $content = array( 'id' => $conjunct->id
                    , 'start' => round($startTimeStamp, 6)
                    , 'end' => round($endTimeStamp, 6)
                    , 'duration' => round($endTimeStamp - $startTimeStamp, 6)
                    , 'invariantRules' => implode(';', $conjunct->invRuleNames)
                    , 'signalRules' => implode(';', $conjunct->sigRuleNames)
            );
        }
        
        usort($content, function($a, $b){ 
            return $b['duration'] <=> $a['duration']; // uses php7 spaceship operator
        });

        $this->writer->write($content);

        return $content;
    }

}