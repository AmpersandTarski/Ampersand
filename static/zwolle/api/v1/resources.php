<?php

/** RESOURCE CALLS without INTERFACES ***************************************************************/

$app->get('/resources', function() use ($app) {
	if(Config::get('productionEnv')) throw new Exception ("List of all resource types is not available in production environment", 403);
	
	$content = array_keys(Concept::getAllConcepts()); // Return list of all concepts
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/resources/:resourceType', function ($resourceType) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	// Checks
	if(!in_array($resourceType, $session->getEditableConcepts())) throw new Exception ("You do not have access for this call", 403);
	
	// Get list of all atoms for $resourceType (i.e. concept)
	$content = Concept::getAllAtomObjects($resourceType); 
	
	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/resources/:resourceType/:resourceId', function ($resourceType, $resourceId) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	// Checks
	if(!in_array($resourceType, $session->getEditableConcepts())) throw new Exception ("You do not have access for this call", 403);

	// Get specific resource (i.e. atom)
	$resource = new Atom($resourceId, $resourceType);
	if(!$resource->atomExists()) throw new Exception("Resource '{$resource->id}[{$resource->concept}]' not found", 404);
	
	$content = $resource->getAtom();

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


/** RESOURCE CALLS with INTERFACES ***************************************************************/

$app->get('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();

	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);

	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);

	$atom = new Atom($resourceId, $resourceType);
	$pathInfo = $atom->walkIfcPath($ifcPath);

	// Checks
	if(!$pathInfo['ifc']->crudR) throw new Exception ("Read not allowed for '$ifcPath'", 405);

	$pathEntry = '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifcPath;
	$content = $pathInfo['srcAtom']->getContent($pathInfo['ifc'], $pathEntry, $pathInfo['tgtAtom']->id, $options);
	
	// If force list option is provided, make sure to return an array
	// Interfaces that have an object as tgt already return an array (when ifc is univalent or not)
	if($options['forceList'] && $pathInfo['ifc']->univalent && !$pathInfo['ifc']->tgtConceptIsObject) $content = array($content);
	if($options['forceList'] && empty($content)) $content = array();

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->put('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
});

$app->patch('/resources/:resourceType/:resourceId(/:ifcPath+)', function ($resourceType, $resourceId, $ifcPath = array()) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	$options = $app->request->params();
	
	$atom = new Atom($resourceId, $resourceType);
	
	if(!empty($ifcPath)){
		$ifcPath = implode ('/', $ifcPath);
		$pathInfo = $atom->walkIfcPath($ifcPath);
		$pathEntry = '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifcPath;
		
		// Checks
		if(is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot patch '$ifcPath'. Missing resource identifier", 405);
		
		// Perform patch(es)	
		$pathInfo['tgtAtom']->patch($pathInfo['ifc'], $pathEntry, $app->request->getBody(), $options);
	
		// Get content of patched atom TODO: make sure that content is also returned when database was not committed
		$content = $pathInfo['srcAtom']->getContent($pathInfo['ifc'], $pathEntry, $pathInfo['tgtAtom']->id, $options);
	
	}else{		
		// Checks
		if(is_null($app->request->params('topLevelIfc'))) throw new Exception ("Top level interface required, but not specified", 400);
		
		// Perform patch(es)
		$atom->patch(null, '', $app->request->getBody(), $options);
		
		// Get content of patched atom TODO: make sure that content is also returned when database was not committed
		$ifc = new InterfaceObject($app->request->params('topLevelIfc'));
		$pathEntry = '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifc->id;
		$content = $atom->getContent($ifc, $pathEntry, null, $options);
	
	}
	
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
	$pathInfo = $atom->walkIfcPath($ifcPath);
	
	// Checks
	if(!is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot create in '$ifcPath'. Path ends with resource", 405);
	if(!$pathInfo['ifc']->crudC) throw new Exception ("Create not allowed for '$ifcPath'", 405);
	if(!$pathInfo['ifc']->tgtConceptIsObject) throw new Exception ("Cannot create non-object {$pathInfo['ifc']->tgtConcept}. Use PUT or PATCH instead", 405);
	
	// Perform create
	$pathEntry = '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifcPath;
	$content = $pathInfo['srcAtom']->create($pathInfo['ifc'], $pathEntry, $app->request->getBody(), $options);

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

	$pathInfo = $atom->walkIfcPath($ifcPath);

	// Checks
	if(is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot delete from '$ifcPath'. Missing resource identifier", 405);
	if(!$pathInfo['ifc']->crudD) throw new Exception ("Delete not allowed for '$ifcPath'", 405);
	if(!$pathInfo['ifc']->tgtConceptIsObject) throw new Exception ("Cannot delete non-object {$pathInfo['ifc']->tgtConcept}. Use PUT or PATCH instead", 405);

	// Perform delete
	$pathInfo['tgtAtom']->delete($options);

	// Return result
	$result = array ( 'notifications' 		=> Notifications::getAll()
					, 'invariantRulesHold'	=> $session->database->getInvariantRulesHold()
					, 'requestType'			=> $session->database->getRequestType()
					);

	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

?>