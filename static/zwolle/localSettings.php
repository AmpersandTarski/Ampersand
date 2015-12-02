<?php
define ('LOCALSETTINGS_VERSION', 1.2);

error_reporting(E_ALL & ~E_NOTICE);
ini_set("display_errors", false);

/************ Server URL config ********************/
// Config::set('serverURL', 'global', 'http://www.yourdomain.nl'); // defaults to http://localhost/<ampersand context name>
// Config::set('apiPath', 'global', '/api/v1'); // relative path to api

/************ MySQL database config ****************/
// Config::set('dbHost', 'mysqlDatabase', 'localhost');
// Config::set('dbUser', 'mysqlDatabase', 'ampersand');
// Config::set('dbPassword', 'mysqlDatabase', 'ampersand');
// Config::set('dbName', 'mysqlDatabase', '');


/* For your convenience, the lines below can be copied straight into your source file (but outside a PATTERN or PROCESS).
{- ----------- LOGIN FUNCTIONALITY -------------
 * Enable/disable built-in login functionality -- see comments in 'localSettings.php'
 * **In 'localSettings.php', enable login with Config::set('loginEnabled', 'global', true)**
 * This requires the following &-INTERFACE defintions:
-} INTERFACE "SessionRoles" FOR NobodyInParticular : sessionRoles[SESSION*Role] BOX [ "ignored" : I]
{- Notes: 
   1) The name of the interface MUST be 'SessionRoles'
   2) The 'FOR NobodyInParticular' ensures that the INTERFACE does not show up in a menu.
   3) The expression between the ':' and 'BOX', must provide all pairs (s,r), where r is an activated role in SESSION s.
   4) The SRC-concept of the expression between the ':' and 'BOX' MUST be 'SESSION'
   5) The TGT-concept of that expression between the ':' and 'BOX' MUST be 'Role'
   6) In order to ensure that the concept 'Role' contains all <Rolename>s that are used within the FrontEnd, e.g. for
      - MAINTAINing rules 
      - accessing INTERFACEs
      - using extensions (e.g. ExcelImport, or the ExecEngine API, as specified in the config files):
      you should include statements such as the following, and make sure all <Rolename>s are mentioned:
      ** POPULATION Role CONTAINS [ "<Rolename1>", "<Rolename2>", ... ]**
   7) The text 'BOX [ "ignored" : I ]' is not used, so it can be replaced with anything that the parser accepts.  
   8) REPRESENT Role TYPE <something> is not allowed: A 'Role' may not be a scalar (it must be an Object-type)
-} INTERFACE "SessionUser" FOR NobodyInParticular : '_SESSION';V[SESSION*User] BOX [ "ignored" : I]
{- Notes:
   1) The name of the interface must be 'SessionUser'
   2) The 'FOR NobodyInParticular' ensures that the INTERFACE does not show up in a menu.
   3) The SRC-concept of the expression between the ':' and 'BOX' MUST be 'SESSION'
   4) The TGT-concept of the interface expression must be 'User'
   5) The text 'BOX [ "ignored" : I ]' is not used, so it can be replaced with anything that the parser accepts.
   6) REPRESENT User TYPE <something> is not allowed: A 'User' may not be a scalar (it must be an Object-type)
-}
*/
// Config::set('loginEnabled', 'global', false);

/************ EXTENSIONS ***************/
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php'); // Enable ExecEngine
//If you use the login features, the ExecEngine function is only available to users that have a specific role. The line here below configures the role 'ExecEngineer' for this. If you so wish, you can change this role name, or add others in the array. If you do not use the login features, then you can leave this line as is.
//Config::set('allowedRolesForRunFunction', 'ExecEngine', array('ExecEngineer'));
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php'); // Enable ExcelImport
//If you use the login features, the ExcelImport function is only available to users that have a specific role. The line here below configures the role 'ExcelImporter' for this. If you so wish, you can change this role name, or add others in the array. If you do not use the login features, then you can leave this line as is.
//Config::set('allowedRolesForExcelImport', 'excelImport', array('ExcelImporter'));
//require_once(__DIR__ . '/extensions/OAuthLogin/OAuthLogin.php'); // Enable OAuthLogin, supported identityProviders are 'google' and 'linkedin'
	//Config::set('identityProviders', 'OAuthLogin', array('google' => array('redirectUrl' => '', 'clientId' => '' ,'clientSecret' => '' ,'tokenUrl' => '' ,'apiUrl' => '' ,'scope' => ''), 'linkedin' => array()));

?>