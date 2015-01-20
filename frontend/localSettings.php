<?php

require_once (__DIR__.'/ampersand/Generics.php');

// define ('IP', 'http://localhost/[APP]');

/************ CONFIG ********************/
define ('DEFAULT_ROLEID', '0');

/************ DB CONFIG ****************/
$DB_host = 'localhost';
$DB_user = 'ampersand';
$DB_pass = 'ampersand';
$DB_name = $dbName; // from Generics.php


/************ EXTENSIONS ****************/

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$ext['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';

// Enable ExecImport
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');



?>