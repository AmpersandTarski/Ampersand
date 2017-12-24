<?php

namespace Ampersand\Extension\ExecEngine;

use Ampersand\Hooks;
use Ampersand\AngularApp;
use Ampersand\Config;
use Ampersand\Role;

// Load the ExecEngine functions
$files = \Ampersand\Helper\getDirectoryList(__DIR__ . '/functions');
foreach ($files as $file){
    if (substr($file,-3) !== 'php') continue;
    require_once $path = __DIR__ . '/functions/' . $file;
}

?>