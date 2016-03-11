<?php

register_shutdown_function('shutdown');
function shutdown(){
	$error = error_get_last();
	if ($error['type'] === E_ERROR) {
		$protocol = (isset($_SERVER['SERVER_PROTOCOL']) ? $_SERVER['SERVER_PROTOCOL'] : 'HTTP/1.0');
		http_response_code(500);
		header($protocol . ' 500 ' . $error['message']);
		print json_encode(array('error' => 500, 'msg' => $error['message']));
		exit;
	}
}

require_once (__DIR__ . '/../../../fw/includes.php');

// Create and configure Slim app (version 2.x)
$app = new \Slim\Slim(array(
    'debug' => Config::get('debugMode')
));

$app->add(new \Slim\Middleware\ContentTypes());
$app->response->headers->set('Content-Type', 'application/json');

// Error handler
$app->error(function (Exception $e) use ($app) {
	$app->response->setStatus($e->getCode());
	print json_encode(array('error' => $e->getCode(), 'msg' => $e->getMessage()));
});

// Not found handler
$app->notFound(function () use ($app) {
	$app->response->setStatus(404);
	print json_encode(array('error' => 404, 'msg' => "Not found"));
});

$app->get('/login', function () use ($app){
	$session = Session::singleton();
	
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

$app->get('/logout', function () use ($app){
	$session = Session::singleton();
	
	$session->database->deleteAtom(new Atom(session_id(), 'SESSION'));
		
	$session->database->closeTransaction('Logout successfull', false, true);
		
	$result = array('notifications' => Notifications::getAll());
	
	print json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
});

$app->get('/callback/google', function () use ($app){
	$code = $app->request->params('code');
	OAuthLoginController::callback($code, 'google');
});

$app->get('/callback/linkedin', function () use ($app){
	// TODO: add check $state variable, to prevent CSPF attack
	$code = $app->request->params('code');
	OAuthLoginController::callback($code, 'linkedin');
});

// Run app
$app->run();

?>