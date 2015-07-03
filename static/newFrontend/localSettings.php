<?php
error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", false);

/************ CONFIG ********************/
define ('API_PATH', 'http://localhost/' . $contextName . '/api/v1/'); // $contextName is defeind in the (generated!) file __DIR__ . '/generics/Generics.php'

define ('JSONLD_TYPE_PATH', API_PATH . 'concept/');
define ('JSONLD_ID_PATH', API_PATH . 'resource/');
define ('JSONLD_CONTEXT_PATH', API_PATH . 'interface/');

define ('LOG_ENABLED', true);

/************ DB CONFIG ****************/
$DB_host = 'localhost';
$DB_user = 'ampersand';
$DB_pass = 'ampersand';
$DB_name = $dbName; // from Generics.php

/************ EXTENSIONS ***************
 *
 *	Define global variables, settings, etc using:
 *	$GLOBALS['ext']['<extension name']...
 *
 */

/************ LOGIN FUNCTIONALITY ***************
 * Enable/disable built-in login functionality
 * 
 * Requires an &-INTERFACE defintion "SessionRoles" with expr.signature [SESSION*Role]
 * 
 */
define ('LOGIN_ENABLED', false);

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$ext['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';
	
	// Config params for SendEmail function of ExecEngine (now using Gmail) 
	// For more info, see __DIR__.'/extensions/ExecEngine/functions/SendEmail.php'
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['from'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['username'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['password'] = '';
	
	// Config params for SendSMS function of ExecEngine (using MessageBird.com)
	// For more info, see __DIR__.'/extensions/ExecEngine/functions/SendSMS.php'
	$sender = $GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['sender'] = ''; // Set the sender, could be a number (16 numbers) or letters (11 characters)
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['username'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['password'] = '';

// Enable ExecImport
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');

// Enable other stuff, e.g. extensions such as DndTree
// require_once(__DIR__ . '/extensions/DndTree/DndTree.php');

?>