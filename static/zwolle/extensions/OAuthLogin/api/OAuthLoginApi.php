<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;

class OAuthLoginApi{
	
	/**
	 * @url GET login
	 */
	public function login(){
		try{
			$idps = array();
			$identityProviders = Config::get('identityProviders', 'OAuthLogin');
			
			if(is_null($identityProviders)) throw new Exception ("No identity providers specified for OAuthLogin extension", 500);
			
			// Google
			if($identityProviders['google']){
				$auth_url = array(
						'auth_base' => $identityProviders['google']['authBase'],
						'arguments' => array(
								'client_id' => $identityProviders['google']['clientId'],
								'response_type' => 'code',
								'redirect_uri' => $identityProviders['google']['redirectUrl'],
								'scope' => $identityProviders['google']['scope']
						)
				);
				$url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
				
				$idps[] = array( 'name' => 'Google' 
							   , 'loginUrl' => $url
							   , 'logo' => 'extensions/OAuthLogin/ui/images/logo-google.png'
							   );
			}
			
			// LinkedIn
			if($identityProviders['linkedin']){
				$auth_url = array(
						'auth_base' => $identityProviders['linkedin']['authBase'],
						'arguments' => array(
								'client_id' => $identityProviders['linkedin']['clientId'],
								'response_type' => 'code',
								'redirect_uri' => $identityProviders['linkedin']['redirectUrl'],
								'scope' => $identityProviders['linkedin']['scope'],
								'state' => $identityProviders['linkedin']['state']
						)
				);
				$url = $auth_url['auth_base'] . '?' . http_build_query($auth_url['arguments']);
				
				$idps[] = array( 'name' => 'LinkedIn'
							   , 'loginUrl' => $url
							   , 'logo' => 'extensions/OAuthLogin/ui/images/logo-linkedin.png'
							   );
			}
			
			// Return
			return array('identityProviders' => $idps, 'notifications' => Notifications::getAll());
			
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
	 * @url GET callback/google
	 * @param string $code
	 */
	public function googleCallback($code){
		$this->callback($code, 'google');
	}
	
	/**
	 * @url GET callback/linkedin
	 * @param string $code
	 */
	public function linkedinCallback($code){
		// TODO: add check $state variable, to prevent CSPF attack
		
		$this->callback($code, 'linkedin');
		
	}
	
	
	private function callback($code, $idp){
		try{
			$identityProviders = Config::get('identityProviders', 'OAuthLogin');
			
			if(empty($code)) throw new Exception("Oops. Someting went wrong during login. Please try again", 401);
			
			$session = Session::singleton();
			$db = Database::singleton();
			
			if(!isset($identityProviders[$idp])) throw new Exception("Unknown identity provider", 500);
			
			$client_id 		= $identityProviders[$idp]['clientId'];
			$client_secret 	= $identityProviders[$idp]['clientSecret'];
			$redirect_uri 	= $identityProviders[$idp]['redirectUrl'];
			$token_url 		= $identityProviders[$idp]['tokenUrl'];
			$api_url 		= $identityProviders[$idp]['apiUrl'];
			$emailField		= $identityProviders[$idp]['emailField'];

			// instantiate authController
			$authController = new OAuthLoginController($client_id,$client_secret,$redirect_uri,$token_url);
			
			// request token
			if($authController->requestToken($code)){
				// request data
				if($authController->requestData($api_url)){
					
					// Verify email/role here
					$email = $authController->getData()->$emailField;
					
					// Get user with $email
					
					// Set sessionUser
					$interface = new InterfaceObject('EmailUser');
					$atom = new Atom($email, 'Email');
					$users = array_keys((array)$atom->getContent($interface, true));
					
					// create new user
					if(empty($users)){
						$newUser = Concept::createNewAtom('User');
						$db->addAtomToConcept($newUser, 'User');
						$db->editUpdate('userEmail', false, $newUser, 'User', $email, 'Email');
						
						// add to Organization
						$domain = explode('@', $email)[1];
						$interface = new InterfaceObject('DomainOrgs');
						$atom = new Atom($domain, 'Domain');
						$orgs = array_keys((array)$atom->getContent($interface, true));
						
						foreach ($orgs as $org){
							$db->editUpdate('userOrganization', false, $newUser, 'User', $org, 'Organization');
						}
						
						$users[] = $newUser;
						
					}
					
					if(count($users) > 1) throw new Exception("Multiple users registered with email $email", 401);
					
					foreach ($users as $userId){
						// Set sessionUser
						$db->editUpdate('sessionUser', false, session_id(), 'SESSION', $userId, 'User');
						
						// Timestamps
						$db->editUpdate('userLastLoginTimeStamp', false, $userId, 'User', date(DATE_ISO8601), 'DateTime');
						$db->editUpdate('userLoginTimeStamp', false, $userId, 'User', date(DATE_ISO8601), 'DateTime');
					}
					
					$db->closeTransaction('Login successfull', false, true, false);
					
				}
			}
			
			header('Location: '. Config::get('serverURL'));
			exit;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>