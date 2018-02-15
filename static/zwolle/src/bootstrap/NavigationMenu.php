<?php

use Ampersand\Misc\Config;

/** 
 * @var \Pimple\Container $container
 */
global $container;

// Navigation menu settings
$angularApp = $container['angular_app'];
$angularApp->addMenuItem('refresh', 'app/components/admin/installer-menu-item.html', 
    function($app){
        return !Config::get('productionEnv');
    });

$angularApp->addMenuItem('refresh', 'app/components/admin/check-rules-menu-item.html',
    function($app){
        return !Config::get('productionEnv');
    });

$angularApp->addMenuItem('refresh', 'app/components/admin/execengine-menu-item.html',
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForRunFunction','execEngine');
        return $app->hasActiveRole($roles);
    });

$angularApp->addMenuItem('ext', 'app/components/importer/menu-item.html', 
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForImporter');
        return $app->hasActiveRole($roles);
    });

$angularApp->addMenuItem('ext', 'app/components/admin/exporter-menu-item.html',
    function($app){
        return !Config::get('productionEnv');
    });
