<?php

use Ampersand\Session;
use Ampersand\Config;
use Ampersand\Log\Notifications;
use Ampersand\Core\Atom;
use Ampersand\Extension\OAuthLogin\OAuthLoginController;

global $app;

// Path to API is 'api/v1/oauthlogin/login'
$app->get('/oauthlogin/login', function () use ($app){
	Session::singleton();
	
	$idps = array();
	$identityProviders = Config::get('identityProviders', 'OAuthLogin');
	
	if(is_null($identityProviders)) throw new Exception ("No identity providers specified for OAuthLogin extension", 500);
	
	// Google
	if($identityProviders['google']){
		$auth_url = array( 'auth_base' => $identityProviders['google']['authBase']
						 , 'arguments' => array( 'client_id' => $identityProviders['google']['clientId']
												, 'response_type' => 'code'
												, 'redirect_uri' => $identityProviders['google']['redirectUrl']
												, 'scope' => $identityProviders['google']['scope'])
						 );
		$url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
		
		$idps[] = array( 'name' => 'Google' 
					   , 'loginUrl' => $url
					   , 'logo' => 'extensions/OAuthLogin/ui/images/logo-google.png'
					   );
	}
	
	// LinkedIn
	if($identityProviders['linkedin']){
		$auth_url = array( 'auth_base' => $identityProviders['linkedin']['authBase']
						 , 'arguments' => array( 'client_id' => $identityProviders['linkedin']['clientId']
												, 'response_type' => 'code'
												, 'redirect_uri' => $identityProviders['linkedin']['redirectUrl']
												, 'scope' => $identityProviders['linkedin']['scope']
												, 'state' => $identityProviders['linkedin']['state'])
						 );
		$url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
		
		$idps[] = array( 'name' => 'LinkedIn'
					   , 'loginUrl' => $url
					   , 'logo' => 'extensions/OAuthLogin/ui/images/logo-linkedin.png'
					   );
	}
	
	// Return
	$result = array('identityProviders' => $idps, 'notifications' => Notifications::getAll());
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

// Path to API is 'api/v1/oauthlogin/logout'
$app->get('/oauthlogin/logout', function () use ($app){
	$session = Session::singleton();
	
	$session->database->deleteAtom(new Atom(session_id(), 'SESSION'));
		
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

?>