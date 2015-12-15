<?php 

require_once (__DIR__ . '/../../fw/includes.php');

// Create and configure Slim app (version 2.x)
$app = new \Slim\Slim(array(
    'debug' => !Config::get('productionEnv')
));

$app->add(new \Slim\Middleware\ContentTypes());
$app->response->headers->set('Content-Type', 'application/json');

// Error handler
$app->error(function (Exception $e) use ($app) {
	$app->response->setStatus($e->getCode());
	print json_encode(array('error' => $e->getCode(), 'msg' => $e->getMessage()));
});

// Not found handler
$app->notFound(function () use ($app) {
	$app->response->setStatus(404);
	print json_encode(array('error' => 404, 'msg' => "Not found"));
});

// Define app routes
$app->get('/hello/:name+', function ($name) {
	print json_encode(array("Hello", $name));
});


$app->get('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
	
	$options = $app->request->params();
	$ifcPath = implode ('/', $ifcPath);
	
	$atom = new Atom($resourceId, $resourceType);
	
	$pathInfo = $atom->walkIfcPath($ifcPath);
	
	$pathEntry = Config::get('serverURL') . Config::get('apiPath') . '/resources/' . $resourceType . '/' . $resourceId . '/' . $ifcPath;
	
	$content = $pathInfo['srcAtom']->getContentNew($pathInfo['ifc'], $pathEntry, $pathInfo['tgtAtom'], $options);
	
	print json_encode($content, JSON_PRETTY_PRINT);
	
});

$app->put('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
});

$app->patch('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
});

$app->post('/resources/:resourceType/:resourceId/:ifcPath+', function ($resourceType, $resourceId, $ifcPath) use ($app) {
	throw new Exception ("Not implemented yet", 501);
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
	
	$result = $pathInfo['tgtAtom']->delete($options);
	
	print json_encode($result, JSON_PRETTY_PRINT);
	
});

// Run app
$app->run();
	
?>