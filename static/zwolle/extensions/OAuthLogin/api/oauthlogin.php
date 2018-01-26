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

// Path to API is 'api/v1/oauthlogin/callback/github'
$app->get('/oauthlogin/callback/:idp', function ($idp) use ($app){
    // TODO: add check $state variable, to prevent CSPF attack
    if($error = $app->request->params('error')) {
        // $error_description = $app->request->params('error');
        // $error_uri = $app->request->params('error_uri');
        throw new Exception("An error occured: {$error}");
    } else {
        switch ($idp) {
            case 'google':
            case 'linkedin':
            case 'github':
                OAuthLoginController::callback($app->request->params('code'), $idp);
                break;
            default:
                throw new Exception("Unsupported identity provider", 500);
        }
    }
});
?>