<?php

use Ampersand\Misc\Config;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Slim\App;
use Slim\Http\Request;
use Slim\Http\Response;
use Slim\Container;

require_once (__DIR__ . '/../../src/bootstrap.php');

/** 
 * @var \Pimple\Container $container
 */
global $container;

$apiContainer = new Container();

// Custom NotFound handler
$apiContainer['notFoundHandler'] = function($c){
    return function(Request $request, Response $response) use ($c) {
        return $c['response']
            ->withStatus(404)
            ->withHeader('Content-Type', 'application/json')
            ->write(json_encode(
                [ 'error' => 404
                , 'msg' => "API endpoint not found: {$request->getMethod()} {$request->getUri()}. Note! virtual path is case sensitive"
                ]
            ));
    };
};

$apiContainer['errorHandler'] = function ($c) use ($container) {
    return function (Request $request, Response $response, Exception $e) use ($c, $container) {
        try{
            Logger::getLogger("API")->error($e->getMessage());
            
            switch ($e->getCode()) {
                case 401: // Unauthorized
                case 403: // Forbidden
                    if(Config::get('loginEnabled') && !$container['ampersand_app']->getSession()->sessionUserLoggedIn()){
                        $code = 401;
                        $message = "Please login to access this page";
                    }else{
                        $code = 403;
                        $message = "You do not have access to this page";
                    }
                    break;
                default:
                    $code = $e->getCode();
                    $message = $e->getMessage();
                    break;
            }
            
            return $c['response']->withJson(['error' => $code, 'msg' => $message, 'notifications' => Notifications::getAll()], $code, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);

        }catch(Exception $b){
            Logger::getLogger("API")->error($b->getMessage());

            return $c['response']->withJson(['error' => $b->getCode(), 'msg' => $b->getMessage(), 'notifications' => []], 500, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
        }
    };
};

// Settings
$apiContainer->get('settings')->replace(['displayErrorDetails' => Config::get('debugMode')]);

// Create and configure Slim app (version 3.x)
$app = new App($apiContainer);

// Add middleware to set default content type for response
$app->add(function (Request $req,  Response $res, callable $next) {
    $res = $res->withHeader('Content-Type', 'application/json;charset=utf-8');
    $newResponse = $next($req, $res);
    return $newResponse;
});

include (__DIR__ . '/resources.php'); // API calls starting with '/resource/'
include (__DIR__ . '/admin.php'); // API calls starting with '/admin/'
include (__DIR__ . '/app.php'); // API calls starting with '/app/'

foreach((array)$GLOBALS['api']['files'] as $apiFile) include_once ($apiFile); // include api path added by extensions

// Run app
$app->run();
