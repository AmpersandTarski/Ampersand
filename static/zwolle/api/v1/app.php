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
 * @phan-closure-scope \Slim\App
 */
$app->group('/app', function () {
    // Inside group closure, $this is bound to the instance of Slim\App
    /** @var \Slim\App $this */

    $this->patch('/roles', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this['appContainer']['ampersand_app'];

        $ampersandApp->setActiveRoles((array) $request->getParsedBody());
        return $response->withJson($ampersandApp->getSessionRoles(), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/navbar', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this['appContainer']['ampersand_app'];
        /** @var \Ampersand\AngularApp $angularApp */
        $angularApp = $this['appContainer']['angular_app'];

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

    $this->get('/notifications', function (Request $request, Response $response, $args = []) {
        /** @var \Ampersand\AmpersandApp $ampersandApp */
        $ampersandApp = $this['appContainer']['ampersand_app'];

        $ampersandApp->checkProcessRules();
        return $response->withJson(Notifications::getAll(), 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });
})->add($middleWare1);
