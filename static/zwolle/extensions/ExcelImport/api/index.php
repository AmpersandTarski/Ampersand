<?php

register_shutdown_function('shutdown');
function shutdown(){
	$error = error_get_last();
	if ($error['type'] === E_ERROR) {
		$protocol = (isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : 'HTTP/1.0');
		http_response_code(500);
		header($protocol . ' 500 ' . $error['message']);
		print json_encode(array('error' => 500, 'msg' => $error['message']));
		exit;
	}
}

require_once (__DIR__ . '/../../../fw/includes.php');

// Create and configure Slim app (version 2.x)
$app = new \Slim\Slim(array(
    'debug' => !Config::get('debugMode')
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

$app->post('/import', function () use ($app){
	$session = Session::singleton();
	
	$roleIds = $app->request->params('roleIds');
	$session->activateRoles($roleIds);
			
	// Check sessionRoles if allowedRolesForExcelImport is specified
	$allowedRoles = Config::get('allowedRolesForExcelImport','excelImport');
	if(!is_null($allowedRoles)){
		$ok = false;
	
		foreach($session->getSessionRoles() as $role){
			if(in_array($role->label, $allowedRoles)) $ok = true;
		}
		if(!$ok) throw new Exception("You do not have access to import excel files", 401);
	}
	
	if (is_uploaded_file($_FILES['file']['tmp_name'])){
		// Parse:
		$parser = new ImportExcel($_FILES['file']['tmp_name']);
		$result = $parser->ParseFile();
		unlink($_FILES['file']['tmp_name']);
	}else{
	    Notifications::addError('No file uploaded');
	}
	
	$result = array('notifications' => $result, 'files' => $_FILES);
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// Run app
$app->run();

?>