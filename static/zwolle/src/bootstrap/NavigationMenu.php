<?php

/** 
 * @var \Pimple\Container $container
 */
global $container;

// Navigation menu settings
$angularApp = $container['angular_app'];
$angularApp->addMenuItem('refresh', 'app/views/menu/installer.html', 
    function($app){
        return !Config::get('productionEnv');
    });

$angularApp->addMenuItem('refresh', 'app/views/menu/checkAllRules.html',
    function($app){
        return !Config::get('productionEnv');
    });

$angularApp->addMenuItem('refresh', 'app/views/menu/execEngine.html',
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForRunFunction','execEngine');
        return $app->hasActiveRole($roles);
    });

$angularApp->addMenuItem('ext', 'app/views/menu/importer.html', 
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForImporter');
        return $app->hasActiveRole($roles);
    });

$angularApp->addMenuItem('ext', 'app/views/menu/exporter.html',
    function($app){
        return !Config::get('productionEnv');
    });
