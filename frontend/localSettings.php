<?php

define ('IP', 'http://localhost/CB');

/************ EXTENSIONS ****************/

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$ext['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';

require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');



?>