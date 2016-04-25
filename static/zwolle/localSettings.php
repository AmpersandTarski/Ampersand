<?php

use Ampersand\Log\Logger;
use Ampersand\Log\NotificationHandler;
use Ampersand\Config;

define ('LOCALSETTINGS_VERSION', 1.5);

date_default_timezone_set('Europe/Amsterdam');

/**************************************************************************************************
 * LOGGING functionality
 *************************************************************************************************/
error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", false);
//Config::set('debugMode', 'global', false); // default = true

// Log file handler
$fileHandler = new \Monolog\Handler\RotatingFileHandler(__DIR__ . '/log/debug.log', 0, \Monolog\Logger::DEBUG);
//$fileHandler->pushProcessor(new \Monolog\Processor\WebProcessor()); // Adds IP adres and url info to log records
Logger::registerGenericHandler($fileHandler);

// Browsers debuggers
//$browserHandler = new \Monolog\Handler\ChromePHPHandler(\Monolog\Logger::DEBUG); // Log handler for Google Chrome
//$browserHandler = new \Monolog\Handler\FirePHPHandler(\Monolog\Logger::DEBUG); // Log handler for Firebug in Mozilla Firefox
//Logger::registerGenericHandler($browserHandler);

// User log handler
Logger::registerHandlerForChannel('USERLOG', new NotificationHandler(\Monolog\Logger::INFO));


/**************************************************************************************************
 * SERVER settings
 *************************************************************************************************/
// Config::set('serverURL', 'global', 'http://www.yourdomain.nl'); // defaults to http://localhost/<ampersand context name>
// Config::set('apiPath', 'global', '/api/v1'); // relative path to api


/**************************************************************************************************
 * DATABASE settings
 *************************************************************************************************/
// Config::set('dbHost', 'mysqlDatabase', 'localhost');
// Config::set('dbUser', 'mysqlDatabase', 'ampersand');
// Config::set('dbPassword', 'mysqlDatabase', 'ampersand');
// Config::set('dbName', 'mysqlDatabase', '');


/**************************************************************************************************
 * LOGIN FUNCTIONALITY
 * 
 * The login functionality requires the ampersand SIAM module
 * The module can be downloaded at: https://github.com/AmpersandTarski/ampersand-models/tree/master/SIAM
 * Copy and rename the SIAM_Module-example.adl into SIAM_Module.adl
 * Include this file into your project
 * Uncomment the config setting below
 *************************************************************************************************/
// Config::set('loginEnabled', 'global', true);


/**************************************************************************************************
 * EXTENSIONS
 *************************************************************************************************/
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php'); // Enable ExecEngine
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php'); // Enable ExcelImport


?>