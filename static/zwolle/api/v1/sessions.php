<?php

use Ampersand\Session;
use Ampersand\Log\Notifications;
use Ampersand\Misc\Config;

/**
 * @var \Slim\Slim $app
 */
global $app;

/** 
 * @var \Pimple\Container $container
 */
global $container;

$app->get('/admin/sessions/delete/all', function () use ($app, $container) {
    if(Config::get('productionEnv')) throw new Exception ("Deleting all sessions is not allowed in production environment", 403);
    throw new Exception("Not implemented", 501);
});

$app->get('/admin/sessions/delete/expired', function () use ($app, $container) {
    Session::deleteExpiredSessions();
});

$app->put('/session/roles', function () use ($app, $container){
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];

    $roles = (array) $app->request()->getBody();
    $ampersandApp->setActiveRoles($roles);

    print json_encode($ampersandApp->getSessionRoles(), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/session/navbar', function () use ($app, $container) {
    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];
    
    $ampersandApp->checkProcessRules();
    
    $session = $ampersandApp->getSession();
    $content = array ('top' => $angularApp->getNavBarIfcs('top')
                     ,'new' => $angularApp->getNavBarIfcs('new')
                     ,'refreshMenu' => $angularApp->getMenuItems('refresh')
                     ,'extMenu' => $angularApp->getMenuItems('ext')
                     ,'roleMenu' => $angularApp->getMenuItems('role')
                     ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()
                                                 ,'cacheGetCalls' => Config::get('interfaceCacheGetCalls', 'transactions')
                                                 ,'switchAutoSave' => Config::get('interfaceAutoSaveChanges', 'transactions')
                                                 )
                     ,'notifications' => Notifications::getAll()
                     ,'session' => array ('id' => $session->getId()
                                         ,'loggedIn' => $session->sessionUserLoggedIn()
                                         )
                     ,'sessionRoles' => $ampersandApp->getSessionRoles()
                     ,'sessionVars' => $session->getSessionVars()
                     );
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/session/notifications', function () use ($app, $container) {
     /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];
    
    $ampersandApp->checkProcessRules();
    
    $content = Notifications::getAll();
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});
