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
use Ampersand\Core\Relation;

class Reporter
{

    /**
     * Writer
     *
     * @var \Ampersand\IO\AbstractWriter
     */
    protected $writer;

    public function __construct(AbstractWriter $writer)
    {
        $this->writer = $writer;
    }

    /**
     * Write relation definition report
     * Specifies multiplicity constraints, related conjuncts and other
     * aspects of all relations
     *
     * @return \Ampersand\Misc\Reporter
     */
    public function reportRelationDefinitions(): Reporter
    {
        $content = array_map(function (Relation $relation) {
            $relArr = [];
            
            $relArr['signature'] = $relation->signature;
            
            // Get multiplicity constraints
            $constraints = [];
            if ($relation->isUni) {
                $constraints[] = "[UNI]";
            }
            if ($relation->isTot) {
                $constraints[] = "[TOT]";
            }
            if ($relation->isInj) {
                $constraints[] = "[INJ]";
            }
            if ($relation->isSur) {
                $constraints[] = "[SUR]";
            }
            $relArr['constraints'] = empty($constraints) ? "no constraints" : implode(',', $constraints);
            
            $relArr['affectedConjuncts'] = [];
            foreach ($relation->getRelatedConjuncts() as $conjunct) {
                $relArr['affectedConjuncts'][$conjunct->id] = [];
                foreach ($conjunct->invRuleNames as $ruleName) {
                    $relArr['affectedConjuncts'][$conjunct->id]['invRules'][] = $ruleName;
                }
                foreach ($conjunct->sigRuleNames as $ruleName) {
                    $relArr['affectedConjuncts'][$conjunct->id]['sigRules'][] = $ruleName;
                }
            }
            $relArr['srcOrTgtTable'] = $relation->getMysqlTable()->tableOf;
            
            return $relArr;
        }, Relation::getAllRelations());

        $this->writer->write($content);
        
        return $this;
    }

    /**
     * Write interface report
     * Specifies aspects for all interfaces (incl. subinterfaces), like path, label,
     * crud-rights, etc
     *
     * @return \Ampersand\Misc\Reporter
     */
    public function reportInterfaceDefinitions(): Reporter
    {
        $content = [];
        foreach (InterfaceObject::getAllInterfaces() as $key => $ifc) {
            $content = array_merge($content, $ifc->getInterfaceFlattened());
        }
        
        $content = array_map(function (InterfaceObject $ifc) {
            return [ 'path' => $ifc->getPath()
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
                   ];
        }, $content);

        $this->writer->write($content);

        return $this;
    }

    /**
     * Write interface issue report
     * Currently focussed on CRUD rights
     *
     * @return \Ampersand\Misc\Reporter
     */
    public function reportInterfaceIssues(): Reporter
    {
        $content = [];
        foreach (InterfaceObject::getAllInterfaces() as $key => $interface) {
            foreach ($interface->getInterfaceFlattened() as $ifc) {
                if ($ifc->crudU() && !$ifc->isEditable()) {
                    $content[] = [ 'interface' => $ifc->getPath()
                                 , 'message' => "Update rights (crUd) specified while interface expression is not an editable relation!"
                                 ];
                }

                if ($ifc->crudC() && !$ifc->tgtConcept->isObject()) {
                    $content[] = [ 'interface' => $ifc->getPath()
                                 , 'message' => "Create rights (Crud) specified while target concept is a scalar. This has no affect!"
                                 ];
                }

                if ($ifc->crudD() && !$ifc->tgtConcept->isObject()) {
                    $content[] = [ 'interface' => $ifc->getPath()
                                 , 'message' => "Delete rights (cruD) specified while target concept is a scalar. This has no affect!"
                                 ];
                }

                if (!$ifc->crudR()) {
                    $content[] = [ 'interface' => $ifc->getPath()
                                 , 'message' => "No read rights specified. Are you sure?"
                                 ];
                }
                
                // Check for unsupported patchReplace functionality due to missing 'old value'. Related with issue #318. TODO: still needed??
                if ($ifc->isEditable() && $ifc->crudU() && !$ifc->tgtConcept->isObject() && $ifc->isUni()) {
                    // Only applies to editable relations
                    // Only applies to crudU, because issue is with patchReplace, not with add/remove
                    // Only applies to scalar, because objects don't use patchReplace, but Remove and Add
                    // Only if interface expression (not! the relation) is univalent, because else a add/remove option is used in the UI
                    if ((!$ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'tgt')
                            || ($ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'src')) {
                        $content[] = [ 'interface' => $ifc->getPath()
                                     , 'message' => "Unsupported edit functionality due to combination of factors. See issue #318"
                                     ];
                    }
                }
            }
        }

        if (empty($content)) {
            $content[] = ['No issues found'];
        }

        $this->writer->write($content);
        
        return $this;
    }

    /**
     * Write conjunct usage report
     * Specifies which conjuncts are used by which rules, grouped by invariants,
     * signals, and unused conjuncts
     *
     * @return \Ampersand\Misc\Reporter
     */
    public function reportConjunctUsage(): Reporter
    {
        $content = [];
        foreach (Conjunct::getAllConjuncts() as $conj) {
            if ($conj->isInvConj()) {
                $content['invConjuncts'][] = $conj->__toString();
            }
            if ($conj->isSigConj()) {
                $content['sigConjuncts'][] = $conj->__toString();
            }
            if (!$conj->isInvConj() && !$conj->isSigConj()) {
                $content['unused'][] = $conj->__toString();
            }
        }

        $this->writer->write($content);

        return $this;
    }

    /**
     * Write conjunct performance report
     *
     * @param \Ampersand\Rule\Conjunct[] $conjuncts
     * @return \Ampersand\Misc\Reporter
     */
    public function reportConjunctPerformance(array $conjuncts): Reporter
    {
        $content = [];
        
        // run all conjuncts (from - to)
        foreach ($conjuncts as $conjunct) {
            /** @var \Ampersand\Rule\Conjunct $conjunct */
            $startTimeStamp = microtime(true); // true means get as float instead of string
            $conjunct->evaluate();
            $endTimeStamp = microtime(true);
            set_time_limit((int) ini_get('max_execution_time')); // reset time limit counter
            
            $content = [ 'id' => $conjunct->id
                       , 'start' => round($startTimeStamp, 6)
                       , 'end' => round($endTimeStamp, 6)
                       , 'duration' => round($endTimeStamp - $startTimeStamp, 6)
                       , 'invariantRules' => implode(';', $conjunct->invRuleNames)
                       , 'signalRules' => implode(';', $conjunct->sigRuleNames)
                       ];
        }
        
        usort($content, function ($a, $b) {
            return $b['duration'] <=> $a['duration']; // uses php7 spaceship operator
        });

        $this->writer->write($content);

        return $this;
    }
}
