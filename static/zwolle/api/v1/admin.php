<?php

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
		$conj = RuleEngine::getConjunct('conj_' . $i);
		$startTimeStamp = microtime(true); // true means get as float instead of string
		RuleEngine::checkConjunct('conj_' . $i, false);
		$endTimeStamp = microtime(true);
	
		$performanceArr['conj_'.$i] = array( 'id' => 'conj_' . $i
				, 'start' => $startTimeStamp
				, 'end' => $endTimeStamp
				, 'duration' => $endTimeStamp - $startTimeStamp
				, 'invariantRules' => implode(';', $conj['invariantRuleNames'])
				, 'signalRules' => implode(';', $conj['signalRuleNames'])
		);
	}
	
	switch ($groupBy){
		case 'conjuncts' :
			$content = array_values($performanceArr);
			break;
		case 'rules' :
			$ruleArr = array();
			foreach(RuleEngine::getAllRules() as $rule){
				$duration = 0;
				foreach($rule['conjunctIds'] as $conj){
					$duration += $performanceArr[$conj]['duration'];
				}
				$ruleArr[] = array('ruleName' => $rule['name']
						, 'duration' => $duration
						, 'conjuncts' => implode(';', $rule['conjunctIds'])
				);
			}
			$content = $ruleArr;
			break;
		case 'relations' :
			$relArr = array();
			foreach(Relation::getAllRelations() as $sig => $rel){
				$duration = 0;
				$conjuncts = array_merge($rel['affectedInvConjunctIds'], $rel['affectedSigConjunctIds']);
				foreach($conjuncts as $conj){
					$duration += $performanceArr[$conj]['duration'];
				}
				$relArr[] = array('relationSignature' => $sig
						, 'duration' => $duration
						, 'conjuncts' => implode(';', $conjuncts)
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

?>