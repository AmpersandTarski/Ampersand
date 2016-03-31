<?php

register_shutdown_function('shutdown');
function shutdown(){
    $error = error_get_last();
    if ($error['type'] === E_ERROR) {
        $protocol = (isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : 'HTTP/1.0');
        http_response_code(500);
        header($protocol . ' 500 ' . $error['message']);
        print json_encode(array('error' => 500, 'msg' => $error['message'] . " in " . $error['file'] . ":" . $error['line']));
        exit;
    }
}

// PHP SESSION : Start a new, or resume the existing, PHP session
session_start();

if (version_compare(PHP_VERSION, '5.4.0', '<')) {
	throw new Exception("PHP version >= 5.4 required. You are on " . PHP_VERSION, 500);
}
if (version_compare(PHP_VERSION, '7.0.0', '>=')) {
    throw new Exception("PHP version 7 is not yet supported " . PHP_VERSION, 500);
}

/* Composer Autoload third-party libraries */
if(file_exists(__DIR__ . '/../lib/autoload.php')) require_once (__DIR__ . '/../lib/autoload.php');

/* FUNCTIONS OF NEWER VERSIONS OF PHP */
require_once (__DIR__ . '/functions/array_column.php'); //TODO: can be removed after PHP update >= 5.5

/* OTHER GENERIC FUNCTIONS */
require_once (__DIR__ . '/functions/getDirectoryList.php');
require_once (__DIR__ . '/functions/isAssoc.php');

/* INCLUDES OF AMPERSAND FRAMEWORK */
require_once (__DIR__ . '/Logger.php');
require_once (__DIR__ . '/Config.php');
require_once (__DIR__ . '/Hooks.php');

require_once (__DIR__ . '/AngularApp.php');
require_once (__DIR__ . '/Atom.php');
require_once (__DIR__ . '/Concept.php');
require_once (__DIR__ . '/Conjunct.php');
require_once (__DIR__ . '/Database.php');
require_once (__DIR__ . '/InterfaceObject.php');
require_once (__DIR__ . '/Notifications.php');
require_once (__DIR__ . '/Relation.php');
require_once (__DIR__ . '/Role.php');
require_once (__DIR__ . '/Rule.php');
require_once (__DIR__ . '/RuleEngine.php');
require_once (__DIR__ . '/Session.php');
require_once (__DIR__ . '/View.php');
require_once (__DIR__ . '/Violation.php');

require_once (__DIR__ . '/../localSettings.php');

// Check version of localSettings.php
$requiredVersion = 1.2;
if(!defined('LOCALSETTINGS_VERSION') || $requiredVersion > LOCALSETTINGS_VERSION) throw new Exception("New version of localSettings.php required. Please update to v" . number_format ($requiredVersion, 1) . " or higher", 500);

\Ampersand\Logger::getLogger('FW')->addDebug("###### SCRIPT START #########################");

?>