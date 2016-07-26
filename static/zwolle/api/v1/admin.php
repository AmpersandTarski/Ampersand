<?php

use Ampersand\Config;
use Ampersand\Database\Database;
use Ampersand\Session;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Conjunct;
use Ampersand\Rule\Rule;
use Ampersand\Core\Relation;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;

global $app;

$app->get('/admin/installer', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("Database reinstall not allowed in production environment", 403);
	
	$defaultPop = filter_var($app->request->params('defaultPop'), FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE); 
    if(is_null($defaultPop)) $defaultPop = true;

	Database::createDB();

	$db = Database::singleton();
	$db->reinstallDB($defaultPop);

	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$content = Notifications::getAll(); // Return all notifications

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->get('/admin/export/all', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Export not allowed in production environment", 403);
    
    $allAtoms = array();
    foreach (Concept::getAllConcepts() as $concept){
        $allAtoms[$concept->name] = $concept->getAllAtomIds();
    }
    
    $allLinks = array();
    foreach (Relation::getAllRelations() as $rel){
        $allLinks[$rel->signature] = $rel->getAllLinks();
    }
    
    $strFileContent = '<?php' . PHP_EOL
                     .'$allAtoms = ' . var_export($allAtoms, true) . ';' . PHP_EOL
                     .'$allLinks = ' . var_export($allLinks, true) . ';' .PHP_EOL
                     .'?>';
    
    file_put_contents(Config::get('logPath') . "/export-" . date('Y-m-d_H-i-s') . ".php", $strFileContent);
});

$app->get('/admin/import', function () use ($app){
    if(Config::get('productionEnv')) throw new Exception ("Import not allowed in production environment", 403);

    $file = $app->request->params('file'); if(is_null($file)) throw new Exception("Import file not specified",500);
    
    $database = Database::singleton();
    
    include_once (Config::get('logPath') . "/{$file}");
    
    // check if all concepts and relations are defined
    foreach((array)$allAtoms as $cpt => $atoms) if(!empty($atoms)) Concept::getConcept($cpt);
    foreach((array)$allLinks as $rel => $links) if(!empty($links)) Relation::getRelation($rel);
    
    foreach((array)$allAtoms as $cpt => $atoms){        
        foreach($atoms as $atomId){
            $atom = new Atom($atomId, $cpt);
            $atom->addAtom();
        }
    }
    
    foreach ((array)$allLinks as $rel => $links){
        if(!empty($links)) $relation = Relation::getRelation($rel);
        
        foreach($links as $link){
            if(is_null($link['src']) || is_null($link['tgt'])) continue; // skip
            
            $relation->addLink(new Atom($link['src'], $relation->srcConcept->name), new Atom($link['tgt'], $relation->tgtConcept->name));
        }
    }
    
    $database->closeTransaction("Imported successfully", true);	
    
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
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
	
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

?>