<?php

use Ampersand\Misc\Config;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;
use Slim\App;
use Slim\Http\Request;
use Slim\Http\Response;
use Slim\Container;

require_once(__DIR__ . '/../../src/bootstrap.php');

/**
 * @var \Pimple\Container $container
 */
global $container;

$apiContainer = new Container();

function stackTrace(Throwable $throwable): string
{
    $html = sprintf('<div><strong>Type:</strong> %s</div>', get_class($throwable));
    
    if (($code = $throwable->getCode())) {
        $html .= sprintf('<div><strong>Code:</strong> %s</div>', $code);
    }

    if (($message = $throwable->getMessage())) {
        $html .= sprintf('<div><strong>Message:</strong> %s</div>', htmlentities($message));
    }

    if (($file = $throwable->getFile())) {
        $html .= sprintf('<div><strong>File:</strong> %s</div>', $file);
    }

    if (($line = $throwable->getLine())) {
        $html .= sprintf('<div><strong>Line:</strong> %s</div>', $line);
    }

    if (($trace = $throwable->getTraceAsString())) {
        $html .= '<h2>Trace</h2>';
        $html .= sprintf('<pre>%s</pre>', htmlentities($trace));
    }
    return $html;
}

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

$apiContainer['errorHandler'] = function ($c) use ($container) {
    return function (Request $request, Response $response, Exception $exception) use ($container) {
        try {
            Logger::getLogger("API")->error($exception->getMessage());
            $debugMode = Config::get('debugMode');
            
            switch ($exception->getCode()) {
                case 401: // Unauthorized
                case 403: // Forbidden
                    if (Config::get('loginEnabled') && !$container['ampersand_app']->getSession()->sessionUserLoggedIn()) {
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
                    $message = $debugMode ? $exception->getMessage() : "An error occured. Sorry for the temporary inconvenience";
                    break;
                default:
                    $code = $exception->getCode();
                    $message = $exception->getMessage();
                    break;
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
                , 'msg' => $debugMode ? $error->getMessage() : "An error occured. Sorry for the temporary inconvenience"
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
$app = new App($apiContainer);

// Add middleware to set default content type for response
$app->add(function (Request $req, Response $res, callable $next) {
    $res = $res->withHeader('Content-Type', 'application/json;charset=utf-8');
    $newResponse = $next($req, $res);
    return $newResponse;
});

$middleWare1 = function (Request $request, Response $response, callable $next) {
    // Overwrite default media type parser for application/json
    $request->registerMediaTypeParser('application/json', function ($input) {
        return json_decode($input, false); // set accoc param to false, this will return php stdClass object instead of array for json objects {}
    });
    return $next($request, $response);
};

include(__DIR__ . '/resources.php'); // API calls starting with '/resource/'
include(__DIR__ . '/admin.php'); // API calls starting with '/admin/'
include(__DIR__ . '/app.php'); // API calls starting with '/app/'

foreach ((array)$GLOBALS['api']['files'] as $apiFile) {
    include_once($apiFile); // include api path added by extensions
}

// Run app
$app->run();
