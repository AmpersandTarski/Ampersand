<?php

use Ampersand\Config;
use Ampersand\Core\Concept;
use Ampersand\Session;
use Ampersand\Core\Atom;
use Ampersand\Log\Notifications;
use Ampersand\Interfacing\InterfaceObject;

global $app;

/**************************************************************************************************
 *
 * resource calls WITHOUT interfaces
 *
 *************************************************************************************************/

$app->get('/resources', function() use ($app) {
	if(Config::get('productionEnv')) throw new Exception ("List of all resource types is not available in production environment", 403);
	
	$content = array_keys(Concept::getAllConcepts()); // Return list of all concepts
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/resources/:resourceType', function ($resourceType) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	$concept = Concept::getConcept($resourceType);
	
	// Checks
	if(!$session->isEditableConcept($concept)) throw new Exception ("You do not have access for this call", 403);
	
	// Get list of all atoms for $resourceType (i.e. concept)
	$content = $concept->getAllAtomObjects(); 
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/resources/:resourceType/:resourceId', function ($resourceType, $resourceId) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
    
	$resource = new Atom($resourceId, $resourceType);
	
	// Checks
	if(!$session->isEditableConcept($resource->concept)) throw new Exception ("You do not have access for this call", 403);

	// Get specific resource (i.e. atom)
	if(!$resource->atomExists()) throw new Exception("Resource '{$resource->id}[{$resource->concept->name}]' not found", 404);
	
	$content = $resource->getAtom();

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


/**************************************************************************************************
 *
 * resource calls WITH interfaces
 *
 *************************************************************************************************/

$app->get('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);

	$atom = new Atom($resourceId, $resourceType);
	$atomOrIfc = $atom->walkIfcPath($ifcPath);

	$content = $atomOrIfc->getContent($options);
	
	// If force list option is provided, make sure to return an array
	if($options['forceList'] && isAssoc($content)) $content = array($content);

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->put('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
});

$app->patch('/resources/:resourceType/:resourceId(/:ifcPath+)', function ($resourceType, $resourceId, $ifcPath = array()) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$topLevelIfcId = $app->request->params('topLevelIfc');
	$options = $app->request->params();
	
	$session->activateRoles($roleIds);
	   
	if(empty($ifcPath) && empty($topLevelIfcId)) throw new Exception ("Parameter 'topLevelIfc' is required to return data when no interface path is specified", 400);
	
	$ifcPath = implode ('/', $ifcPath);
	
	$atom = new Atom($resourceId, $resourceType);
	$atom->topLevelIfcId = $topLevelIfcId;
	
	// Create atom if not exists and crudC is allowed
	if(!$atom->atomExists() && InterfaceObject::getInterface($topLevelIfcId)->crudC) $atom->addAtom();
	
	$atomOrIfc = $atom->walkIfcPath($ifcPath);
	
	// Perform patch(es)
	$content = $atomOrIfc->patch($app->request->getBody(), $options);
	
	// Return result
	$result = array ( 'patches'				=> $app->request->getBody()
					, 'content' 			=> $content
					, 'notifications' 		=> Notifications::getAll()
					, 'invariantRulesHold'	=> $session->database->getInvariantRulesHold()
					, 'requestType'			=> $session->database->getRequestType()
					);
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
	
});

$app->post('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);

	$atom = new Atom($resourceId, $resourceType);
	$atomOrIfc = $atom->walkIfcPath($ifcPath);
	
	// Perform create
	$content = $atomOrIfc->create($app->request->getBody(), $options);

	// Return result
	$result = array ( 'content' 			=> $content
					, 'notifications' 		=> Notifications::getAll()
					, 'invariantRulesHold'	=> $session->database->getInvariantRulesHold()
					, 'requestType'			=> $session->database->getRequestType()
					);

	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->delete('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);

	$atom = new Atom($resourceId, $resourceType);
	$atomOrIfc = $atom->walkIfcPath($ifcPath);

	// Perform delete
	$atomOrIfc->delete($options);

	// Return result
	$result = array ( 'notifications' 		=> Notifications::getAll()
					, 'invariantRulesHold'	=> $session->database->getInvariantRulesHold()
					, 'requestType'			=> $session->database->getRequestType()
					);

	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

?>