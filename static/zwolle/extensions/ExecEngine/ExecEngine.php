<?php

namespace Ampersand\Extension\ExecEngine;

use Ampersand\Hooks;
use Ampersand\AngularApp;
use Ampersand\Config;
use Ampersand\Role;

// Define hooks
$hook1 = array('class' => '\Ampersand\Extension\ExecEngine\ExecEngine', 'function' => 'run', 'filename' => 'ExecEngine.php', 'filepath' => 'extensions/ExecEngine', 'params' => array());
Hooks::addHook('preCloseTransaction', $hook1);

// UI
AngularApp::addMenuItem('ext', 'extensions/ExecEngine/ui/views/MenuItem.html',
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForRunFunction','execEngine');
        return $app->hasActiveRole($roles);
    });
AngularApp::addJS('extensions/ExecEngine/ui/js/ExecEngine.js');

// API
$GLOBALS['api']['files'][] = __DIR__ . DIRECTORY_SEPARATOR . 'api' . DIRECTORY_SEPARATOR . 'run.php';

// Config (can be overwritten in localSettings.php)
Config::set('execEngineRoleName', 'execEngine', 'ExecEngine');
Config::set('autoRerun', 'execEngine', true);
Config::set('maxRunCount', 'execEngine', 10);

// ExecEngine code
require_once (__DIR__ . '/src/ExecEngine.php');

// Load the ExecEngine functions
$files = \Ampersand\Helper\getDirectoryList(__DIR__ . '/functions');
foreach ($files as $file){
    if (substr($file,-3) !== 'php') continue;
    require_once $path = __DIR__ . '/functions/' . $file;
}

?>