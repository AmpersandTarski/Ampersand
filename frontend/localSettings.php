<?php

/************ CONFIG ********************/
define ('DEFAULT_ROLEID', '0');

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
	
	// Config params for SendEmail function of ExecEngine
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['from'] = 'noreply.ampersand@gmail.com';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['username'] = 'noreply.ampersand@gmail.com';
	$GLOBALS['ext']['ExecEngine']['functions']['SendEmail']['password'] = 'dawrRwARH6YxHYe2828D';
	
	// Config params for SendSMS function of ExecEngine
	$sender = $GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['sender'] = 'AmProtoType'; // Set the sender, could be a number (16 numbers) or letters (11 characters)
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['username'] = 'Naisunev';
	$GLOBALS['ext']['ExecEngine']['functions']['SendSMS']['password'] = 'Urb4nFl00d';

// Enable ExecImport
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');



?>