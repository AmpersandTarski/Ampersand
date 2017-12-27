<?php

use Ampersand\Session;
use Ampersand\AngularApp;
use Ampersand\AmpersandApp;
use Ampersand\Log\Notifications;
use Ampersand\Config;
use Ampersand\Interfacing\Transaction;

global $app;

$app->get('/admin/sessions/delete/all', function () use ($app) {
    throw new Exception("Not implemented", 501);
    if(Config::get('productionEnv')) throw new Exception ("Deleting all sessions is not allowed in production environment", 403);
});

$app->get('/admin/sessions/delete/expired', function () use ($app) {
    Session::deleteExpiredSessions();
    Transaction::getCurrentTransaction()->close(true);
});

$app->get('/sessions/:sessionId/navbar', function ($sessionId) use ($app) {
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    $ampersandApp->checkProcessRules();
    
    $session = $ampersandApp->getSession();
    $content = array ('top' => AngularApp::getNavBarIfcs('top')
                     ,'new' => AngularApp::getNavBarIfcs('new')
                     ,'refreshMenu' => AngularApp::getMenuItems('refresh')
                     ,'extMenu' => AngularApp::getMenuItems('ext')
                     ,'roleMenu' => AngularApp::getMenuItems('role')
                     ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()
                                                 ,'cacheGetCalls' => Config::get('interfaceCacheGetCalls', 'transactions')
                                                 ,'switchAutoSave' => Config::get('interfaceAutoSaveChanges', 'transactions')
                                                 )
                     ,'notifications' => Notifications::getAll()
                     ,'session' => array ('id' => $session->getId()
                                         ,'loggedIn' => $session->sessionUserLoggedIn()
                                         )
                     ,'sessionRoles' => array_values($ampersandApp->getAllowedRoles()) // return numeric array
                     ,'sessionVars' => $session->getSessionVars()
                     );
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});


$app->get('/sessions/:sessionId/notifications', function ($sessionId) use ($app) {
    $ampersandApp = AmpersandApp::singleton();
    
    $roleIds = $app->request->params('roleIds');
    $ampersandApp->activateRoles($roleIds);
    
    $ampersandApp->checkProcessRules();
    
    $content = Notifications::getAll();
    
    print json_encode($content, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

?>