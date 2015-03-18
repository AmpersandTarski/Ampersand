<?php
date_default_timezone_set('Europe/London');
// Adding social logins in future versions may require stuff from libraries:
// require_once (__DIR__ . '/lib/somefile.php');

$apps[] = array('name' => 'Login',  'link' => '#/ext/Login/',  'icon' => 'glyphicon glyphicon-log-in');  // activeer app extension in framework
$apps[] = array('name' => 'Logout', 'link' => '#/ext/Logout/', 'icon' => 'glyphicon glyphicon-log-out'); // activeer app extension in framework

// UI
// $GLOBALS['hooks']['after_Viewer_load_cssFiles'][] = 'extensions/Login/ui/css/style.css';
// $GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/ExcelImport/ui/js/angular-file-upload.min.js';
// $GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/ExcelImport/ui/js/ExcelImport.js';

class Login
{
	
}

?>
