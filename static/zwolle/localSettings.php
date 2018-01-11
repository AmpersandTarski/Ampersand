<?php

use Ampersand\Log\Logger;
use Ampersand\Log\NotificationHandler;
use Ampersand\Misc\Config;
use Ampersand\Plugs\MysqlDB\MysqlDB;

define ('LOCALSETTINGS_VERSION', 1.6);

date_default_timezone_set('Europe/Amsterdam');

/**************************************************************************************************
 * LOGGING functionality
 *************************************************************************************************/
error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", true);

/**************************************************************************************************
 * Execution time limit is set to a default of 30 seconds. Use 0 to have no time limit. (not advised)
 *************************************************************************************************/
set_time_limit (30);

//Config::set('debugMode', 'global', true); // production mode = false

// Log file handler
$fileHandler = new \Monolog\Handler\RotatingFileHandler(__DIR__ . '/log/error.log', 0, \Monolog\Logger::WARNING);
//$fileHandler->pushProcessor(new \Monolog\Processor\WebProcessor()); // Adds IP adres and url info to log records
Logger::registerGenericHandler($fileHandler);

if(Config::get('debugMode')){
    $fileHandler = new \Monolog\Handler\RotatingFileHandler(__DIR__ . '/log/debug.log', 0, \Monolog\Logger::DEBUG);
    Logger::registerGenericHandler($fileHandler);
    
    // Browsers debuggers
    //$browserHandler = new \Monolog\Handler\ChromePHPHandler(\Monolog\Logger::DEBUG); // Log handler for Google Chrome
    //$browserHandler = new \Monolog\Handler\FirePHPHandler(\Monolog\Logger::DEBUG); // Log handler for Firebug in Mozilla Firefox
    //Logger::registerGenericHandler($browserHandler);
}

$execEngineHandler = new \Monolog\Handler\RotatingFileHandler(__DIR__ . '/log/execengine.log', 0, \Monolog\Logger::INFO);
Logger::registerHandlerForChannel('EXECENGINE', $execEngineHandler);

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
$container['mysql_database'] = function($c) {
    $dbHost = Config::get('dbHost', 'mysqlDatabase');
    $dbUser = Config::get('dbUser', 'mysqlDatabase');
    $dbPass = Config::get('dbPassword', 'mysqlDatabase');
    $dbName = Config::get('dbName', 'mysqlDatabase');
    return new MysqlDB($dbHost, $dbUser, $dbPass, $dbName, Logger::getLogger('DATABASE'));
};
$container['default_plug'] = function ($c) {
    return $c['mysql_database'];
};

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
// Config::set('allowedRolesForImporter', 'global', []); // list of roles that have access to the importer


/**************************************************************************************************
 * EXEC ENGINE 
 *************************************************************************************************/
// Config::set('execEngineRoleNames', 'execEngine', ['ExecEngine']);
// Config::set('autoRerun', 'execEngine', true);
// Config::set('maxRunCount', 'execEngine', 10);


/**************************************************************************************************
 * EXTENSIONS
 *************************************************************************************************/

