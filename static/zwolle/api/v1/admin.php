<?php

use Ampersand\Misc\Config;
use Ampersand\Database\Database;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Conjunct;
use Ampersand\Rule\Rule;
use Ampersand\Core\Relation;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\IO\CSVWriter;
use Ampersand\Transaction;
use Ampersand\AmpersandApp;
use Ampersand\Rule\RuleEngine;

global $app;

$app->get('/admin/installer', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Reinstallation of application not allowed in production environment", 403);
    
    $defaultPop = filter_var($app->request->params('defaultPop'), FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE); 
    if(is_null($defaultPop)) $defaultPop = true;

    $ampersandApp = AmpersandApp::singleton();
    $ampersandApp->reinstall($defaultPop);

    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    // Checks
    $logger = Logger::getUserLogger();
    foreach (InterfaceObject::getAllInterfaces() as $key => $interface) {
        foreach($interface->getInterfaceFlattened() as $ifc){
            if($ifc->crudU() && !$ifc->isEditable()) $logger->warning("Update rights (crUd) specified while interface expression is not an editable relation! Ifc:". $ifc->getPath());
            if($ifc->crudC() && !$ifc->tgtConcept->isObject()) $logger->warning("Create rights (Crud) specified while target concept is a scalar. This has no affect! Ifc:". $ifc->getPath());
            if($ifc->crudD() && !$ifc->tgtConcept->isObject()) $logger->warning("Delete rights (cruD) specified while target concept is a scalar. This has no affect! Ifc:". $ifc->getPath());
            if(!$ifc->crudR()) $logger->warning("No read rights specified. Are you sure? Ifc:". $ifc->getPath());
            
            // Check for unsupported patchReplace functionality due to missing 'old value'. Related with issue #318. TODO: still needed??
            if($ifc->isEditable() && $ifc->crudU() && !$ifc->tgtConcept->isObject() && $ifc->isUni()){
                // Only applies to editable relations
                // Only applies to crudU, because issue is with patchReplace, not with add/remove
                // Only applies to scalar, because objects don't use patchReplace, but Remove and Add
                // Only if interface expression (not! the relation) is univalent, because else a add/remove option is used in the UI
                
                if((!$ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'tgt')
                        || ($ifc->relationIsFlipped && $ifc->relation()->getMysqlTable()->tableOf == 'src'))
                    $logger->warning("Unsupported edit functionality due to combination of factors for (sub)interface: " . $ifc->getPath());
            }
        }
    }

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    $content = Notifications::getAll(); // Return all notifications

    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/admin/execengine/run', function () use ($app){
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    // Check for required role
    if(!$ampersandApp->hasRole(Config::get('allowedRolesForRunFunction','execEngine'))) throw new Exception("You do not have access to run the exec engine", 401);
        
    \Ampersand\Rule\ExecEngine::run(true);
    
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Run completed");
    else Logger::getUserLogger()->warning("Run completed but transaction not committed");

    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
        
    $result = array('notifications' => Notifications::getAll());
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/checks/rules/evaluate/all', function() use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Evaluation of all rules not allowed in production environment", 403);
    
    foreach (RuleEngine::checkRules(Rule::getAllInvRules(), true) as $violation) Notifications::addInvariant($violation);
    
    foreach (RuleEngine::checkRules(Rule::getAllSigRules(), true) as $violation) Notifications::addSignal($violation);
    
    $content = Notifications::getAll(); // Return all notifications
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

//TODO: refactor function using new JSON import/export format and functionality
$app->get('/admin/export/all', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Export not allowed in production environment", 403);
    
    $allAtoms = array();
    foreach (Concept::getAllConcepts() as $concept){
        $allAtoms[$concept->name] = array_map(function($atom){
            return $atom->id;
        }, $concept->getAllAtomObjects());
    }
    
    $allLinks = array();
    foreach (Relation::getAllRelations() as $rel){
        $allLinks[$rel->signature] = $rel->getAllLinks();
    }
    
    $strFileContent = '<?php' . PHP_EOL
                     .'$allAtoms = ' . var_export($allAtoms, true) . ';' . PHP_EOL
                     .'$allLinks = ' . var_export($allLinks, true) . ';' .PHP_EOL
                     .'?>';
    
    file_put_contents(Config::get('absolutePath') . Config::get('logPath') . "export-" . date('Y-m-d_H-i-s') . ".php", $strFileContent);
});

//TODO: refactor function using new JSON import/export format and functionality
$app->get('/admin/import', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Import not allowed in production environment", 403);
    $logger = Logger::getLogger('ADMIN');

    $file = $app->request->params('file'); if(is_null($file)) throw new Exception("Import file not specified",500);
    
    $ampersandApp = AmpersandApp::singleton();

    include_once (Config::get('absolutePath') . Config::get('logPath') . "{$file}");
    
    // check if all concepts and relations are defined
    foreach((array)$allAtoms as $cpt => $atoms) if(!empty($atoms)) Concept::getConcept($cpt);
    foreach((array)$allLinks as $rel => $links) if(!empty($links)) Relation::getRelation($rel);
    
    foreach((array)$allAtoms as $cpt => $atoms){
        $logger->info("Importing atoms for concept {$cpt}");
        if(empty($atoms)) continue;
        
        $concept = Concept::getConcept($cpt);
        $total = count($atoms); 
        $i = 1;
        foreach($atoms as $atomId){
            $logger->debug("Importing {$cpt}: atom {$i}/{$total}");
            $i++;
            
            $atom = new Atom($atomId, $concept);
            $atom->add();
        }
    }
    
    foreach ((array)$allLinks as $rel => $links){
        $logger->info("Importing links for relation {$rel}");
        if(empty($links)) continue;
        
        foreach($links as $link) $link->add();
    }
    
    $transaction = Transaction::getCurrentTransaction()->close(true);
    if($transaction->isCommitted()) Logger::getUserLogger()->notice("Imported successfully");
    
    $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

    $content = Notifications::getAll(); // Return all notifications
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    
});


$app->get('/admin/performance/conjuncts', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Performance tests are not allowed in production environment", 403);
    
    // Defaults
    $groupBy = $app->request->params('groupBy'); if(is_null($groupBy)) $groupBy = 'conjuncts';
    $from = $app->request->params('from'); if(is_null($from)) $from = 0;
    $to = $app->request->params('to'); if(is_null($to)) $to = 10;
    
    $performanceArr = array();
    
    // run all conjuncts (from - to)
    for ($i = $from; $i <= $to; $i++){
        $conjunct = Conjunct::getConjunct('conj_' . $i);
        $startTimeStamp = microtime(true); // true means get as float instead of string
        $conjunct->evaluateConjunct(false);
        $endTimeStamp = microtime(true);
    
        $performanceArr[$conjunct->id] = array( 'id' => $conjunct->id
                , 'start' => round($startTimeStamp, 6)
                , 'end' => round($endTimeStamp, 6)
                , 'duration' => round($endTimeStamp - $startTimeStamp, 6)
                , 'invariantRules' => implode(';', $conjunct->invRuleNames)
                , 'signalRules' => implode(';', $conjunct->sigRuleNames)
        );
    }
    
    switch ($groupBy){
        case 'conjuncts' :
            $content = array_values($performanceArr);
            break;
        case 'rules' :
            $ruleArr = array();
            foreach(Rule::getAllRules() as $rule){
                $duration = 0;
                $conjunctIds = array();
                foreach($rule->conjuncts as $conjunct){
                    $duration += $performanceArr[$conjunct->id]['duration'];
                    $conjunctIds[] = $conjunct->id;
                }
                $ruleArr[] = array('ruleName' => $rule->id
                        , 'duration' => $duration
                        , 'conjuncts' => implode(';', $conjunctIds)
                );
            }
            $content = $ruleArr;
            break;
        case 'relations' :
            $relArr = array();
            $conjunctIds = array();
            foreach(Relation::getAllRelations() as $relation){
                $duration = 0;
                foreach($relation->affectedConjuncts as $conjunct){
                    $duration += $performanceArr[$conjunct->id]['duration'];
                    $conjunctIds = $conjunct->id;
                }
                $relArr[] = array('relationSignature' => $relation->__toString()
                        , 'duration' => $duration
                        , 'conjuncts' => implode(';', $conjunctIds)
                );
            }
            $content = $relArr;
            break;
        default :
            throw new Exception ("Unknown groupBy argument", 500);
            break;
    }
    
    usort($content, function($a, $b){ 
        // return $b['duration'] <=> $a['duration']; // uses php7 spaceship operator
        if($b['duration'] < $a['duration']) return -1;
        elseif($b['duration'] == $a['duration']) return 0;
        elseif($b['duration'] > $a['duration']) return 1;
    });
    
    // Output
    $output = new CSVWriter();
    $output->addColumns(array_keys($content[0]));
    foreach ($content as $row) $output->addRow($row);
    $output->render('conj-performance-report.csv');
    
    // print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    
});

$app->get('/admin/report/relations', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);
    
    $content = array();
    foreach(Relation::getAllRelations() as $relation){
        $relArr = array();
        
        $relArr['signature'] = $relation->signature;
        
        $relArr['constraints'] .= $relation->isUni ? "[UNI]" : "";
        $relArr['constraints'] .= $relation->isTot ? "[TOT]" : "";
        $relArr['constraints'] .= $relation->isInj ? "[INJ]" : "";
        $relArr['constraints'] .= $relation->isSur ? "[SUR]" : "";
        if(empty($relArr['constraints'])) $relArr['constraints'] = "no constraints";
        
        $relArr['affectedConjuncts'] = array();
        foreach($relation->affectedConjuncts as $conjunct){
            $relArr['affectedConjuncts'][$conjunct->id] = array();
            foreach ($conjunct->invRuleNames as $ruleName) $relArr['affectedConjuncts'][$conjunct->id]['invRules'][] = $ruleName;
            foreach ($conjunct->sigRuleNames as $ruleName) $relArr['affectedConjuncts'][$conjunct->id]['sigRules'][] = $ruleName;
        }
        $relArr['srcOrTgtTable'] = $relation->getMysqlTable()->tableOf;
        
        $content[] = $relArr;
    }
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/admin/report/conjuncts', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);
    
    $content = array();
    foreach(Conjunct::getAllConjuncts() as $conj){        
        if($conj->isInvConj()) $content['invConjuncts'][] = $conj->__toString();
        if($conj->isSigConj()) $content['sigConjuncts'][] = $conj->__toString();
        if(!$conj->isInvConj() && !$conj->isSigConj()) $content['unused'][] = $conj->__toString();
    }
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/admin/report/interfaces', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);
    
    $arr = array();
    foreach (InterfaceObject::getAllInterfaces() as $key => $ifc) {
        $arr = array_merge($arr, $ifc->getInterfaceFlattened());
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
        
    }, $arr);
    
    // Output
    $output = new CSVWriter();
    $output->addColumns(array_keys($content[0]));
    foreach ($content as $row) $output->addRow($row);
    $output->render('ifc-report.csv');
    
    // print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>