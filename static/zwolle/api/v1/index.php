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

include (__DIR__ . '/resources.php'); // API calls starting with '/resources/'
include (__DIR__ . '/admin.php'); // API calls starting with '/admin/'
include (__DIR__ . '/sessions.php'); // API calls starting with '/admin/'

// Run app
$app->run();
	
?>