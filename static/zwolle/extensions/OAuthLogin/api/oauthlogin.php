<?php

use Ampersand\Session;
use Ampersand\Config;
use Ampersand\Log\Notifications;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Extension\OAuthLogin\OAuthLoginController;

global $app;

// Path to API is 'api/v1/oauthlogin/login'
$app->get('/oauthlogin/login', function () use ($app){
    Session::singleton();
    
    $idps = array();
    $identityProviders = Config::get('identityProviders', 'OAuthLogin');
    
    if(is_null($identityProviders)) throw new Exception ("No identity providers specified for OAuthLogin extension", 500);
    
    foreach ($identityProviders as $idpSettings) {
        $auth_url = array( 'auth_base' => $idpSettings['authBase']
                         , 'arguments' => array( 'client_id' => $idpSettings['clientId']
                                                , 'response_type' => 'code'
                                                , 'redirect_uri' => $idpSettings['redirectUrl']
                                                , 'scope' => $idpSettings['scope']
                                                , 'state' => $idpSettings['state']
                                                )
                         );
        $url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
        
        $idps[] = array( 'name' => $idpSettings['name'] 
                       , 'loginUrl' => $url
                       , 'logo' => $idpSettings['logoUrl']
                       );
    }
    
    // Return
    $result = array('identityProviders' => $idps, 'notifications' => Notifications::getAll());
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// Path to API is 'api/v1/oauthlogin/logout'
$app->get('/oauthlogin/logout', function () use ($app){
    $session = Session::singleton();
    
    $session->sessionAtom->deleteAtom();
        
    $session->database->closeTransaction('Logout successfull', true);
        
    $result = array('notifications' => Notifications::getAll());
    
    print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// Path to API is 'api/v1/oauthlogin/callback/google'
$app->get('/oauthlogin/callback/google', function () use ($app){
    $code = $app->request->params('code');
    OAuthLoginController::callback($code, 'google');
});

// Path to API is 'api/v1/oauthlogin/callback/linkedin'
$app->get('/oauthlogin/callback/linkedin', function () use ($app){
    // TODO: add check $state variable, to prevent CSPF attack
    $code = $app->request->params('code');
    OAuthLoginController::callback($code, 'linkedin');
});

// Path to API is 'api/v1/oauthlogin/callback/github'
$app->get('/oauthlogin/callback/github', function () use ($app){
    // TODO: add check $state variable, to prevent CSPF attack
    $code = $app->request->params('code');
    OAuthLoginController::callback($code, 'github');
});
?>