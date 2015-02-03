<?php

/************ CONFIG ********************/
define ('DEFAULT_ROLEID', '0');
define ('JSONLD_CONTEXT_PATH', 'http://localhost/CB/api/v1/contexts/');
define ('JSONLD_RESOURCE_PATH', 'http://localhost/CB/api/v1/concept/');

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

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$ext['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';
	
	// Config params for SendEmail function of ExecEngine (now using Gmail) More settings in SendEmail.php
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['from'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['username'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['password'] = '';
	
	// Config params for SendSMS function of ExecEngine (using MessageBird.com)
	$sender = $GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['sender'] = ''; // Set the sender, could be a number (16 numbers) or letters (11 characters)
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['username'] = '';
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['password'] = '';

// Enable ExecImport
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');

?>