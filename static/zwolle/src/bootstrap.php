<?php

use Ampersand\Ampersand;
use Ampersand\Session;

register_shutdown_function(function (){
    $error = error_get_last();
    if ($error['type'] === E_ERROR) {
        $protocol = (isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : 'HTTP/1.0');
        http_response_code(500);
        header($protocol . ' 500 ' . $error['message']);
        print json_encode(array('error' => 500, 'msg' => $error['message'] . " in " . $error['file'] . ":" . $error['line']));
        exit;
    }
});

// Check PHP version
if (version_compare(PHP_VERSION, '5.4.0', '<') || version_compare(PHP_VERSION, '7.0.0', '>=')) {
    throw new Exception("PHP version >= 5.4 but < 7.0 required. You are on " . PHP_VERSION, 500);
}

// PHP SESSION : Start a new, or resume the existing, PHP session
session_start();

// Composer Autoloader
require_once (__DIR__ . '/../lib/autoload.php');

// Include/set default settings
require_once (__DIR__ . '/defaultSettings.php');

// Include project specific settings (i.e. localSettings.php file)
require_once (__DIR__ . '/../localSettings.php');
if(!defined('LOCALSETTINGS_VERSION') || Ampersand::REQ_LOCALSETTINGS_VERSION > LOCALSETTINGS_VERSION) throw new Exception("New version of localSettings.php required. Please update to format of v" . number_format (Ampersand::REQ_LOCALSETTINGS_VERSION, 1), 500);

// Start Ampersand Session
Session::singleton();

?>