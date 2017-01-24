<?php

use Ampersand\Config;
use Ampersand\Extension\ExecEngine\ExecEngine;
use Ampersand\Log\Notifications;
use Ampersand\Session;

global $app;

// Path to API is 'api/v1/execengine/import'
$app->get('/execengine/run', function () use ($app){
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	// Check sessionRoles if allowedRolesForRunFunction is specified
    if(!$session->hasAccess(Config::get('allowedRolesForRunFunction','execEngine'))) throw new Exception("You do not have access to run the exec engine", 401);
		
	ExecEngine::run(true);
	
	$session->database->closeTransaction('Run completed', true);
		
	$result = array('notifications' => Notifications::getAll());
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>