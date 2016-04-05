<?php

use Ampersand\Config;
use Ampersand\Log\Notifications;

require_once (__DIR__ . '/../../src/bootstrap.php');

// Code to add special http response codes that are not supported by Slim
class NewResponse extends \Slim\Http\Response {
    public static function addResponseCode($code, $message){
        parent::$messages[$code] = "{$code} {$message}";
    }
}
NewResponse::addResponseCode(440, "Login Timeout");

// Create and configure Slim app (version 2.x)
$app = new \Slim\Slim(array(
    'debug' => Config::get('debugMode')
));

$app->add(new \Slim\Middleware\ContentTypes());
$app->response->headers->set('Content-Type', 'application/json');

// Error handler
$app->error(function (Exception $e) use ($app) {
	$app->response->setStatus($e->getCode());
	try{
	    $notifications = Notifications::getAll();
	    print json_encode(array('error' => $e->getCode(), 'msg' => $e->getMessage(), 'notifications' => $notifications));
	}catch(Exception $b){
	    print json_encode(array('error' => $b->getCode(), 'msg' => $b->getMessage(), 'notifications' => array()));
	}
	
});

// Not found handler
$app->notFound(function () use ($app) {
	$app->response->setStatus(404);
	print json_encode(array('error' => 404, 'msg' => "API call not found"));
});

include (__DIR__ . '/resources.php'); // API calls starting with '/resources/'
include (__DIR__ . '/admin.php'); // API calls starting with '/admin/'
include (__DIR__ . '/sessions.php'); // API calls starting with '/sessions/'
include (__DIR__ . '/interfaces.php'); // API calls starting with '/interfaces/'

foreach((array)$GLOBALS['api']['files'] as $apiFile) include_once ($apiFile); // include api path added by extensions

// Run app
$app->run();
	
?>