<?php

use Ampersand\AmpersandApp;
use Pimple\Container;
use Ampersand\AngularApp;
use Ampersand\Log\Logger;

register_shutdown_function(function () {
    $error = error_get_last();
    if ($error['type'] & (E_ERROR | E_PARSE)) {
        $protocol = (isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : 'HTTP/1.0');
        http_response_code(500);
        header("{$protocol}  500 {$error['message']}");
        print json_encode(['error' => 500
                           , 'msg' => "{$error['message']} in {$error['file']}:{$error['line']}"
                           ]);
        exit;
    }
});

// Check PHP version
if (version_compare(PHP_VERSION, '7.0.0', '<')) {
    throw new Exception("PHP version >= 7.0 required. You are on " . PHP_VERSION, 500);
}

// PHP SESSION : Start a new, or resume the existing, PHP session
ini_set("session.use_strict_mode", true); // prevents a session ID that is never generated
session_start();

// Composer Autoloader
require_once(__DIR__ . '/../lib/autoload.php');

// New Pimple Dependency Injection Container
$container = new Container();
$container['ampersand_app'] = function ($c) {
    return new AmpersandApp($c['default_plug'], Logger::getLogger('APPLICATION'));
};
$container['angular_app'] = function ($c) {
    return new AngularApp($c['ampersand_app'], Logger::getLogger('APP'));
};

// Include/set default settings
require_once(__DIR__ . '/defaultSettings.php');

// Include project specific settings (i.e. localSettings.php file)
require_once(__DIR__ . '/../localSettings.php');
if (!defined('LOCALSETTINGS_VERSION') || AmpersandApp::REQ_LOCALSETTINGS_VERSION > LOCALSETTINGS_VERSION) {
    throw new Exception("New version of localSettings.php required. Please update to format of v" . number_format(AmpersandApp::REQ_LOCALSETTINGS_VERSION, 1), 500);
}

// More bootstrapping
require_once(__DIR__ . '/bootstrap/ExecEngineFunctions.php');
require_once(__DIR__ . '/bootstrap/NavigationMenu.php');
// require_once(__DIR__ . '/bootstrap/ExecEngineDateTime.php');
// require_once(__DIR__ . '/bootstrap/ExecEngineWarshall.php');
