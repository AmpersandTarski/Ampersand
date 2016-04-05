<?php

use Ampersand\Config;
use Ampersand\Database\Database;
use Ampersand\Session;
use Ampersand\Log\Notifications;
use Ampersand\Rule\Conjunct;
use Ampersand\Rule\Rule;
use Ampersand\Core\Relation;

global $app;

$app->get('/admin/installer', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("Database reinstall not allowed in production environment", 403);

	Database::createDB();

	$db = Database::singleton();
	$db->reinstallDB();

	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$content = Notifications::getAll(); // Return all notifications

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});


$app->get('/admin/performance/conjuncts', function () use ($app){
	if(Config::get('productionEnv')) throw new Exception ("Performance tests are not allowed in production environment", 403);
	
	// Defaults
	$groupBy = $app->request->params('groupBy'); if(is_null($groupBy)) $groupBy = 'conjuncts';
	$from = $app->request->params('from'); if(is_null($groupBy)) $from = 0;
	$to = $app->request->params('to'); if(is_null($to)) $to = 10;
	
	$performanceArr = array();
	
	// run all conjuncts (from - to)
	for ($i = $from; $i <= $to; $i++){
		$conjunct = Conjunct::getConjunct('conj_' . $i);
		$startTimeStamp = microtime(true); // true means get as float instead of string
		$conjunct->evaluateConjunct(false);
		$endTimeStamp = microtime(true);
	
		$performanceArr[$conjunct->id] = array( 'id' => $conjunct->id
				, 'start' => $startTimeStamp
				, 'end' => $endTimeStamp
				, 'duration' => $endTimeStamp - $startTimeStamp
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

?>