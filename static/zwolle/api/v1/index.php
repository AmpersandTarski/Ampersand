<?php

use Ampersand\Misc\Config;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Slim\App;
use Slim\Http\Request;
use Slim\Http\Response;
use Slim\Container;
use function Ampersand\Misc\stackTrace;

require_once(__DIR__ . '/../../src/bootstrap.php');

/**
 * @var \Pimple\Container $container
 */
global $container;

$apiContainer = new Container();
$apiContainer['appContainer'] = $container;

// Custom NotFound handler when API path-method is not found
// The application can also return a Resource not found, this is handled by the errorHandler below
$apiContainer['notFoundHandler'] = function ($c) {
    return function (Request $request, Response $response) {
        return $response
            ->withStatus(404)
            ->withHeader('Content-Type', 'application/json')
            ->write(json_encode(
                [ 'error' => 404
                , 'msg' => "API endpoint not found: {$request->getMethod()} {$request->getUri()}. Note! virtual path is case sensitive"
                ]
            ));
    };
};

$apiContainer['errorHandler'] = function ($c) {
    return function (Request $request, Response $response, Exception $exception) use ($c) {
        try {
            Logger::getLogger("API")->error($exception->getMessage());
            $debugMode = Config::get('debugMode');
            
            switch ($exception->getCode()) {
                case 401: // Unauthorized
                case 403: // Forbidden
                    if (Config::get('loginEnabled') && !$c->appContainer['ampersand_app']->getSession()->sessionUserLoggedIn()) {
                        $code = 401;
                        $message = "Please login to access this page";
                    } else {
                        $code = 403;
                        $message = "You do not have access to this page";
                    }
                    break;
                case 404: // Not found
                    $code = $debugMode ? 500 : 404;
                    $message = $exception->getMessage();
                    break;
                case 500:
                    $code = 500;
                    $message = $debugMode ? $exception->getMessage() : "An error occured (debug information in server log files)";
                    break;
                default:
                    $code = $exception->getCode();
                    $message = $exception->getMessage();
                    break;
            }

            // Convert invalid HTTP status code to 500
            if (!is_integer($code) || $code < 100 || $code > 599) {
                $code = 500;
            }
            
            return $response->withJson(
                [ 'error' => $code
                , 'msg' => $message
                , 'notifications' => Notifications::getAll()
                , 'html' => $debugMode ? stackTrace($exception) : null
                ],
                $code,
                JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES
            );
        } catch (Throwable $throwable) { // catches both errors and exceptions
            Logger::getLogger("API")->critical($throwable->getMessage());
            return $response->withJson(
                [ 'error' => 500
                , 'msg' => Config::get('debugMode') ? $throwable->getMessage() : "Something went wrong in returning an error message"
                , 'html' => "Please contact the application administrator for more information"
                ],
                500,
                JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES
            );
        }
    };
};

$apiContainer['phpErrorHandler'] = function ($c) {
    return function (Request $request, Response $response, Error $error) {
        try {
            Logger::getLogger("API")->critical($error->getMessage());
            $debugMode = Config::get('debugMode');

            return $response->withJson(
                [ 'error' => 500
                , 'msg' => $debugMode ? $error->getMessage() : "An error occured (debug information in server log files)"
                , 'notifications' => Notifications::getAll()
                , 'html' => $debugMode ? stackTrace($error) : "Please contact the application administrator for more information"
                ],
                500,
                JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES
            );
        } catch (Throwable $throwable) { // catches both errors and exceptions
            Logger::getLogger("API")->critical($throwable->getMessage());
            return $response->withJson(
                [ 'error' => 500
                , 'msg' => Config::get('debugMode') ? $throwable->getMessage() : "Something went wrong in returning an error message"
                , 'html' => "Please contact the application administrator for more information"
                ],
                500,
                JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES
            );
        }
    };
};

// Settings
$apiContainer->get('settings')->replace(['displayErrorDetails' => Config::get('debugMode')]);

// Create and configure Slim app (version 3.x)
$api = new App($apiContainer);

// Add middleware to set default content type for response
$api->add(function (Request $req, Response $res, callable $next) {
    $res = $res->withHeader('Content-Type', 'application/json;charset=utf-8');
    $newResponse = $next($req, $res);
    return $newResponse;
});

$middleWare1 = function (Request $request, Response $response, callable $next) {
    // Overwrite default media type parser for application/json
    $request->registerMediaTypeParser('application/json', function ($input) {
        $data = json_decode($input, false); // set accoc param to false, this will return php stdClass object instead of array for json objects {}
        switch (json_last_error()) {
            case JSON_ERROR_NONE:
                return $data;
                break;
            case JSON_ERROR_DEPTH:
                throw new Exception("JSON error: Maximum stack depth exceeded", 400);
                break;
            case JSON_ERROR_STATE_MISMATCH:
                throw new Exception("JSON error: Underflow or the modes mismatch", 400);
                break;
            case JSON_ERROR_CTRL_CHAR:
                throw new Exception("JSON error: Unexpected control character found", 400);
                break;
            case JSON_ERROR_SYNTAX:
                throw new Exception("JSON error: Syntax error, malformed JSON", 400);
                break;
            case JSON_ERROR_UTF8:
                throw new Exception("JSON error: Malformed UTF-8 characters, possibly incorrectly encoded", 400);
                break;
            default:
                throw new Exception("JSON error: Unknown error in JSON content", 400);
                break;
        }
    });
    return $next($request, $response);
};

include(__DIR__ . '/resources.php'); // API calls starting with '/resource/'
include(__DIR__ . '/admin.php'); // API calls starting with '/admin/'
include(__DIR__ . '/app.php'); // API calls starting with '/app/'

foreach ((array)$GLOBALS['apiFiles'] as $apiFile) {
    include_once($apiFile); // include api path added by extensions
}

// Add middleware to initialize the AmpersandApp
$api->add(function (Request $req, Response $res, callable $next) {
    /** @var \Slim\App $this */
    $ampersandApp = $this['appContainer']['ampersand_app'];
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp->init(); // initialize Ampersand application

    return $next($req, $res);
})->run();
