<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;

class LoginApi{
	
	/**
	 * @url GET login
	 */
	public function login(){
		try{
			// Check if already logged in
			
			
			// Return google login url
			$auth_url = array(
					'auth_base' => 'https://accounts.google.com/o/oauth2/auth',
					'arguments' => array(
							'client_id' => $GLOBALS['ext']['Login']['google']['clientId'],
							'response_type' => 'code',
							'redirect_uri' => $GLOBALS['ext']['Login']['google']['redirectUrl'],
							'scope' => $GLOBALS['ext']['Login']['google']['scope'],
					)
			);
			
			// build url
			$url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
			
			return array('loginUrl' => $url, 'notifications' => Notifications::getAll());
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url GET logout
	 */
	public function logout(){
		try{
			
			$session = Session::singleton();
			$db = Database::singleton();
			
			$db->deleteAtom(session_id(), 'SESSION');
			
			$db->closeTransaction('Logout successfull', false, true, false);
			
			return array('notifications' => Notifications::getAll());
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url GET callback
	 * @param string $code
	 */
	public function callback($code){
		try{
			if(empty($code)) throw new Exception("Oops. Someting went wrong during login. Please try again", 401);
			
			$session = Session::singleton();
			$db = Database::singleton();
			
			// instantiate authController
			$client_id 		= $GLOBALS['ext']['Login']['google']['clientId'];
			$client_secret 	= $GLOBALS['ext']['Login']['google']['clientSecret'];
			$redirect_uri 	= $GLOBALS['ext']['Login']['google']['redirectUrl'];
			$token_url 		= $GLOBALS['ext']['Login']['google']['tokenUrl'];
			$api_url 		= $GLOBALS['ext']['Login']['google']['apiUrl'];
					
			$authController = new OAuthController($client_id,$client_secret,$redirect_uri,$token_url);
			
			// request token
			if($authController->requestToken($code)){
				// request data
				if($authController->requestData($api_url)){
						
					// Verify email/role here
					$email = $authController->getData()->email;
					
					// Get user with $email
					
					// Set sessionUser
					$interface = new InterfaceObject('EmailUser');
					$atom = new Atom($email, 'Email');
					$users = array_keys((array)$atom->getContent($interface, true));
					
					if(empty($users)) throw new Exception("No user registered with email $email", 401);
					if(count($users) > 1) throw new Exception("Multiple users registered with email $email", 401);
					
					foreach ($users as $userId){
						$db->editUpdate('sessionUser', false, session_id(), 'SESSION', $userId, 'User');
					}
					
					$db->closeTransaction('Login successfull', false, true, false);
					
				}
			}
			
			header('Location: '.HOME);
			exit;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>