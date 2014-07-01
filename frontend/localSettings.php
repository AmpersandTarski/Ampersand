<?php

// define ('IP', 'http://localhost/[APP]');

/************ EXTENSIONS ****************/

// Enable ExecEngine
require_once(__DIR__ . '/extensions/ExecEngine/ExecEngine.php');
$ext['ExecEngine']['ExecEngineRoleName'] = 'ExecEngine';

// Enable ExecImport
require_once(__DIR__ . '/extensions/ExcelImport/ExcelImport.php');

// Enable AmpersandViewer
require_once(__DIR__ . '/viewers/AmpersandViewer/AmpersandViewer.php');

// Enable AngularJSViewer
require_once(__DIR__ . '/viewers/AngularJSViewer/AngularJSViewer.php');


?>