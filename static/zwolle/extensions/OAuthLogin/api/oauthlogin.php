<?php

use Ampersand\Misc\Config;
use Ampersand\Extension\OAuthLogin\OAuthLoginController;
use Slim\Http\Request;
use Slim\Http\Response;
use Ampersand\Log\Notifications;

/**
 * @var \Slim\App $app
 */
global $app;

/**
 * @var \Pimple\Container $container
 */
global $container;

/**
 * @phan-closure-scope \Slim\App
 */
$app->group('/oauthlogin', function () use ($container) {
    // Inside group closure, $this is bound to the instance of Slim\App
    /** @var \Slim\App $this */

    /** @var \Ampersand\AmpersandApp $ampersandApp */
    $ampersandApp = $container['ampersand_app'];

    $this->get('/login', function (Request $request, Response $response, $args = []) {
        // Get configured identity providers
        $identityProviders = Config::get('identityProviders', 'OAuthLogin');
        if (is_null($identityProviders)) {
            throw new Exception("No identity providers specified for OAuthLogin extension", 500);
        }
        if (!is_array($identityProviders)) {
            throw new Exception("Identity providers must be specified as array", 500);
        }
        
        // Prepare list with identity providers for the UI
        $idps = [];
        foreach ($identityProviders as $idpSettings) {
            $auth_url = [ 'auth_base' => $idpSettings['authBase']
                        , 'arguments' => [ 'client_id' => $idpSettings['clientId']
                                         , 'response_type' => 'code'
                                         , 'redirect_uri' => $idpSettings['redirectUrl']
                                         , 'scope' => $idpSettings['scope']
                                         , 'state' => $idpSettings['state']
                                         ]
                        ];
            $url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
            
            $idps[] = [ 'name' => $idpSettings['name']
                      , 'loginUrl' => $url
                      , 'logo' => $idpSettings['logoUrl']
                      ];
        }
        
        return $response->withJson(['identityProviders' => $idps, 'notifications' => Notifications::getAll()], 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/logout', function (Request $request, Response $response, $args = []) use ($ampersandApp) {
        $ampersandApp->logout();
        $ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
        return $response->withJson(['notifications' => Notifications::getAll()], 200, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
    });

    $this->get('/callback/{idp}', function (Request $request, Response $response, $args = []) {
        $code = $request->getQueryParam('code');
        switch ($args['idp']) {
            case 'google':
            case 'linkedin':
            case 'github':
                $isLoggedIn = OAuthLoginController::authenticate($code, $args['idp']);
                break;
            default:
                throw new Exception("Unsupported identity provider", 400);
        }

        $url = $isLoggedIn ? Config::get('redirectAfterLogin', 'OAuthLogin') : Config::get('redirectAfterLoginFailure', 'OAuthLogin');
        return $response->withRedirect($url, 403);
    });
});
