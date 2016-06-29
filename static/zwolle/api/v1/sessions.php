<?php

use Ampersand\Session;
use Ampersand\AngularApp;
use Ampersand\Log\Notifications;
use Ampersand\Config;
use Ampersand\Rule\RuleEngine;

global $app;

$app->get('/sessions/:sessionId/navbar', function ($sessionId) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
    
    foreach(RuleEngine::getSignalViolationsFromDB() as $violation) Notifications::addSignal($violation);
	
	$content = array ('top' => AngularApp::getNavBarIfcs('top')
					 ,'new' => AngularApp::getNavBarIfcs('new')
					 ,'refreshMenu' => AngularApp::getMenuItems('refresh')
					 ,'extMenu' => AngularApp::getMenuItems('ext')
					 ,'roleMenu' => AngularApp::getMenuItems('role')
					 ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()
					                             ,'switchAutoCommit' => Config::get('interfaceAutoCommitChanges', 'transactions')
					                             ,'cacheGetCalls' => Config::get('interfaceCacheGetCalls', 'transactions')
                                                 ,'switchAutoSave' => Config::get('interfaceAutoSaveChanges', 'transactions')
                                                 )
					 ,'notifications' => Notifications::getAll()
					 ,'session' => array ( 'id' => $session->id
					 ,'loggedIn' => $session->sessionUserLoggedIn())
					 ,'sessionRoles' => array_values($session->getSessionRoles()) // return numeric array
					 ,'sessionVars' => $session->getSessionVars()
					 );
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/sessions/:sessionId/notifications', function ($sessionId) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	foreach(RuleEngine::getSignalViolationsFromDB() as $violation) Notifications::addSignal($violation);
	
	$content = Notifications::getAll();
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->delete('/sessions/:sessionId', function ($sessionId) use ($app) {
	$session = Session::singleton();
	
	// Checks
	if($sessionId != $session->id) throw new Exception ("You can only destroy your own session", 403);
	
	// Destroy session
	$session->destroySession();
	
	// Return result
	$content = array('notifications' => Notifications::getAll());
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);	
});

?>