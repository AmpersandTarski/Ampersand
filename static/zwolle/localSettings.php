<?php
define ('LOCALSETTINGS_VERSION', 1.0);

error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", false);

/************ CONFIG ********************/
define ('HOME', 'http://localhost/' . $contextName);
define ('API_PATH', 'http://localhost/' . $contextName . '/api/v1/'); // $contextName is defined in the (generated!) file __DIR__ . '/generics/Generics.php'

define ('JSONLD_TYPE_PATH', API_PATH . 'concept/');
define ('JSONLD_ID_PATH', API_PATH . 'resource/');
define ('JSONLD_CONTEXT_PATH', API_PATH . 'interface/');

define ('UPLOAD_DIR', __DIR__ . '/uploads/');
$GLOBALS['api']['allowedMimeTypes'] = array('application/vnd.ms-excel'
										   ,'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
										   ,'application/excel'
										   ,'application/pdf'
										   ,'text/xml'
										   );

/************ DB CONFIG ****************/
$DB_host = 'localhost';
$DB_user = 'ampersand';
$DB_pass = 'ampersand';
$DB_name = $dbName; // from Generics.php
define ('CHECK_DEF_POP', true); // For debugging: FALSE: commit the initial population, regardless if it has invariant violations.
define ('COMMIT_INV_VIOLATIONS', false); // For debugging: TRUE: always commit changes, even when there are invariant violations.

/* For your convenience, the lines below can be copied straight into your source file (but outside a PATTERN or PROCESS).
{- ----------- LOGIN FUNCTIONALITY -------------
 * Enable/disable built-in login functionality
 * **In 'localSettings.php', the variable 'LOGIN_ENABLED' must be assigned the value 'true'**
 *
 * Requires &-INTERFACE defintion: -}
	INTERFACE "SessionRoles" FOR NobodyInParticular : '_SESSION';V[SESSION*Role] BOX [ "ignored" : I]
	{- Notes: 
	 1) The name of the interface MUST be 'SessionRoles'
	 2) The 'FOR NobodyInParticular' ensures that the INTERFACE does not show up in a menu.
	 3) The SRC concept of the expression between the ':' and 'BOX' MUST be SESSION
	 4) The population of the TGT-concept of that expression must contain all roles that are used 
	      in the &-script for MAINTAINing rules or INTERFACEs. 
	      One way to ensure this is to include the following statement before every 'MAINTAINS' statement:
	        POPULATION Role CONTAINS [ "<Rolename>" ] -- as per Stef's solution on issue #63
	 5) The text 'BOX [ "ignored" : I ]' is not used, so it can be replaced with anything that the parser accepts.  
	 6) REPRESENT Role TYPE <something> is not allowed: A 'Role' may not be a scalar (it must be an Object-type)
	-}
	
	INTERFACE "SessionUser" FOR NobodyInPartical : rel[SESSION*User] BOX [ "ignored" : I]
	{- Notes:
	 1) The name of the interface must be 'SessionUser'
	 2) The 'FOR NobodyInParticular' ensures that the INTERFACE does not show up in a menu.
	 3) The SRC concept of the expression between the ':' and 'BOX' MUST be SESSION
	 4) The TGT of the interface expression must be the User
	 5) The text 'BOX [ "ignored" : I ]' is not used, so it can be replaced with anything that the parser accepts.
	 6) REPRESENT User TYPE <something> is not allowed: A 'User' may not be a scalar (it must be an Object-type)
	-}
*/
define ('LOGIN_ENABLED', false);
//require_once(__DIR__ . '/extensions/Login/Login.php');
$GLOBALS['ext']['Login']['google']['redirectUrl'] = '';
$GLOBALS['ext']['Login']['google']['clientId'] = '';
$GLOBALS['ext']['Login']['google']['clientSecret'] = '';
$GLOBALS['ext']['Login']['google']['tokenUrl'] = '';
$GLOBALS['ext']['Login']['google']['apiUrl'] = '';
$GLOBALS['ext']['Login']['google']['scope'] = '';


/************ EXTENSIONS ***************
 *
 *	Define global variables, settings, etc using:
 *	$GLOBALS['ext']['<extension name']...
 *
 */

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$GLOBALS['ext']['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';
$GLOBALS['ext']['ExecEngine']['MaxRunCount'] = 10;

	
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

?>