<?php

use Ampersand\Session;
use Ampersand\Log\Notifications;
use Ampersand\Config;
use Ampersand\Rule\RuleEngine;

global $app;

$app->get('/sessions/:sessionId/navbar', function ($sessionId) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	// top level interfaces
	$top = array();
	foreach ($session->getInterfacesForNavBar() as $ifc){
		$top[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
	}
	
	// new interfaces
	$new = array();
	foreach ($session->getInterfacesToCreateAtom() as $ifc){
		$new[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
	}
	
	$content = array ('top' => $top
					 ,'new' => $new
					 ,'refreshMenu' => $GLOBALS['navBar']['refreshMenu']
					 ,'appMenu' => $GLOBALS['navBar']['appMenu']
					 ,'roleMenu' => $GLOBALS['navBar']['roleMenu']
					 ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()
					 ,'switchAutoCommit' => Config::get('interfaceAutoCommitChanges', 'transactions')
					 ,'cacheGetCalls' => Config::get('interfaceCacheGetCalls', 'transactions'))
					 ,'notifications' => Notifications::getAll()
					 ,'session' => array ( 'id' => $session->id
					 , 'loggedIn' => $session->sessionUserLoggedIn())
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
	if($sessionId != session_id()) throw new Exception ("You can only destroy your own session", 403);
	
	// Destroy session
	$session->destroySession();
	
	// Return result
	$content = array('notifications' => Notifications::getAll());
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);	
});

?>