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
	if($options['forceList'] && $pathInfo['ifc']->univalent) $content = array($content); 

	print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

});

$app->put('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
});

$app->patch('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);
	
	$atom = new Atom($resourceId, $resourceType);
	$pathInfo = $atom->walkIfcPath($ifcPath);
	
	// Checks
	if(is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot patch '$ifcPath'. Missing resource identifier", 405);
		
	// Perform patch(es)
	$pathEntry = '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifcPath;
	$content = $pathInfo['tgtAtom']->patch($pathInfo['ifc'], $pathEntry, $app->request->getBody(), $options);
	
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
	
	// TODO: File upload
	/* START FILE UPLOAD (copied from old API		
		if (is_uploaded_file($_FILES['file']['tmp_name'])){
			$tmp_name = $_FILES['file']['tmp_name'];
			$new_name = time() . '_' . $_FILES['file']['name'];
			$target = Config::get('uploadPath') . '/' . $new_name;
			$result = move_uploaded_file($tmp_name, $target);
		
			if($result) Notifications::addSuccess("File '".$new_name."' uploaded");
			else Notifications::addError("Error in file upload");
		}else{
			Notifications::addError('No file uploaded');
		}
			
		$newAtom = $session->database->addAtomToConcept(Concept::createNewAtomId('Upload'), 'Upload');
		$session->database->editUpdate('fileName', false, $newAtom, 'Upload', $new_name, 'FileName');
		$session->database->editUpdate('originalFileName', false, $newAtom, 'Upload', $_FILES['file']['name'], 'FileName');
		$session->database->commitTransaction();
	
	END FILE UPLOAD */
	
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