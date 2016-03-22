<?php
define ('LOCALSETTINGS_VERSION', 1.2);

error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", false);
date_default_timezone_set('Europe/Amsterdam');

// Config::set('debugMode', 'global', false); // default = true

/************ Server URL config ********************/
// Config::set('serverURL', 'global', 'http://www.yourdomain.nl'); // defaults to http://localhost/<ampersand context name>
// Config::set('apiPath', 'global', '/api/v1'); // relative path to api

/* ********** MySQL database config ****************/
// Config::set('dbHost', 'mysqlDatabase', 'localhost');
// Config::set('dbUser', 'mysqlDatabase', 'ampersand');
// Config::set('dbPassword', 'mysqlDatabase', 'ampersand');
// Config::set('dbName', 'mysqlDatabase', '');

/************ LOGIN FUNCTIONALITY **************
 * The login functionality requires the ampersand SIAM module
 * The module can be downloaded at: https://github.com/AmpersandTarski/ampersand-models/tree/master/SIAM
 * Copy and rename the SIAM_Module-example.adl into SIAM_Module.adl
 * Include this file into your project
 * Uncomment the config setting below
 */
// Config::set('loginEnabled', 'global', true);

/************ EXTENSIONS ***********************/
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php'); // Enable ExecEngine
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php'); // Enable ExcelImport

?>