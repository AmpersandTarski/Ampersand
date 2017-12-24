<?php

namespace Ampersand\Extension\ExecEngine;

use Ampersand\Hooks;
use Ampersand\AngularApp;
use Ampersand\Config;
use Ampersand\Role;

// Define hooks
$hook1 = array('class' => '\Ampersand\Rule\ExecEngine', 'function' => 'run', 'filename' => 'ExecEngine.php', 'filepath' => 'src/Ampersand/Rule', 'params' => array());
Hooks::addHook('preCloseTransaction', $hook1);

// Load the ExecEngine functions
$files = \Ampersand\Helper\getDirectoryList(__DIR__ . '/functions');
foreach ($files as $file){
    if (substr($file,-3) !== 'php') continue;
    require_once $path = __DIR__ . '/functions/' . $file;
}

?>