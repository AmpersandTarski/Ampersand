<?php

use Ampersand\Log\Notifications;
use Ampersand\Misc\Config;
use Slim\Http\Request;
use Slim\Http\Response;

/**
 * @var \Slim\Slim $app
 */
global $app;

/**
 * @var \Pimple\Container $container
 */
global $container;

$app->group('/app', function () use ($container) {

    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];

    /** @var \Ampersand\AngularApp $angularApp */
    $angularApp = $container['angular_app'];

    $this->patch('/roles', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $ampersandApp->setActiveRoles((array) $request->getParsedBody());
        return $response->withJson($ampersandApp->getSessionRoles(), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/navbar', function (Request $request, Response $response, $args = []) use ($ampersandApp, $angularApp) {
        $ampersandApp->checkProcessRules();
        
        $session = $ampersandApp->getSession();
        $content =  ['top' => $angularApp->getMenuItems('top')
                    ,'new' => $angularApp->getMenuItems('new')
                    ,'refresh' => $angularApp->getMenuItems('refresh')
                    ,'ext' => $angularApp->getMenuItems('ext')
                    ,'role' => $angularApp->getMenuItems('role')
                    ,'defaultSettings' => ['notify_showSignals'        => Config::get('defaultShowSignals', 'notifications')
                                          ,'notify_showInfos'          => Config::get('defaultShowInfos', 'notifications')
                                          ,'notify_showSuccesses'      => Config::get('defaultShowSuccesses', 'notifications')
                                          ,'notify_autoHideSuccesses'  => Config::get('defaultAutoHideSuccesses', 'notifications')
                                          ,'notify_showErrors'         => Config::get('defaultShowErrors', 'notifications')
                                          ,'notify_showWarnings'       => Config::get('defaultShowWarnings', 'notifications')
                                          ,'notify_showInvariants'     => Config::get('defaultShowInvariants', 'notifications')
                                          ,'autoSave'                  => Config::get('interfaceAutoSaveChanges', 'transactions')
                                          ]
                    ,'notifications' => Notifications::getAll()
                    ,'session' =>   ['id' => $session->getId()
                                    ,'loggedIn' => $session->sessionUserLoggedIn()
                                    ]
                    ,'sessionRoles' => $ampersandApp->getSessionRoles()
                    ,'sessionVars' => $session->getSessionVars()
                    ];
        return $response->withJson($content, 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/notifications', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $ampersandApp->checkProcessRules();
        return $response->withJson(Notifications::getAll(), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });
})->add($middleWare1);
