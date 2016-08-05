<?php

namespace Ampersand\Extension\OAuthLogin;

use Exception;
use Ampersand\AngularApp;
use Ampersand\Config;
use Ampersand\Session;
use Ampersand\Database\Database;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Core\Relation;

// UI
AngularApp::addMenuItem('role', 'extensions/OAuthLogin/ui/views/MenuItem.html', function($session){ return true;});
AngularApp::addJS('extensions/OAuthLogin/ui/js/LoginModule.js');

// API
$GLOBALS['api']['files'][] = __DIR__ . DIRECTORY_SEPARATOR . 'api' . DIRECTORY_SEPARATOR . 'oauthlogin.php';

class OAuthLoginController {

	private $token_url;
	private $client_id;
	private $client_secret;
	private $redirect_uri;

	private $tokenObj;
	private $dataObj;

	function __construct($client_id,$client_secret,$redirect_uri,$token_url){
		$this->client_id = $client_id;
		$this->client_secret = $client_secret;
		$this->redirect_uri = $redirect_uri;
		$this->token_url = $token_url;
	}

	public function requestToken($code){
		// Setup token request
		$token_request = array(
				'token_url' => $this->token_url,
				'arguments' => array(
						'client_id' => $this->client_id,
						'client_secret' => $this->client_secret,
						'grant_type' => 'authorization_code',
						'code' => $code,
						'redirect_uri' => $this->redirect_uri
				)
		);

		// Make HTTP POST request to OAUTH host to get token
		$curl = curl_init();
		curl_setopt_array($curl, 
			array( CURLOPT_RETURNTRANSFER => 1
				 , CURLOPT_URL => $token_request['token_url']
				 , CURLOPT_USERAGENT => Config::get('contextName')
				 , CURLOPT_POST => 1
				 , CURLOPT_POSTFIELDS => http_build_query ($token_request['arguments'])
				 , CURLOPT_HTTPHEADER => array('Content-Type: application/x-www-form-urlencoded', 'Accept: application/json')
                 , CURLOPT_CAINFO => __DIR__ . '/cacert.pem'
				 )
			);

		// Send the request & save response to $resp
		$token_resp = curl_exec($curl);

		// Check if response is received:
		if(!$token_resp) throw new Exception('Error: "' . curl_error($curl) . '" - Code: ' . curl_errno($curl), 500);

		// Close request to clear up some resources
		curl_close($curl);
        
		// Decode token JSON response to stdObj and return
		$this->tokenObj = json_decode($token_resp);
		
		if(!isset($this->tokenObj->access_token)){
			
			$error = "Error: Someting went wrong getting token, '" . $this->tokenObj->error . "'";
			if(isset($this->tokenObj->error_description)){
				$error .= " Description: '" . $this->tokenObj->error_description . "'";
			}
			throw new Exception($error, 500);
		}
		
		return true;
	}

	public function requestData($api_url){
		
		if(!isset($this->tokenObj)) throw new Exception("Error: No token set", 500);
		
		// Do a HTTP HEADER request to the API_URL
		$curl = curl_init();
		curl_setopt_array($curl, 
			array( CURLOPT_RETURNTRANSFER => 1
				 , CURLOPT_URL => $api_url
				 , CURLOPT_USERAGENT => Config::get('contextName')
				 , CURLOPT_HTTPHEADER => array('Authorization: Bearer ' . $this->tokenObj->access_token, 'x-li-format: json')
                 , CURLOPT_CAINFO => __DIR__ . '/cacert.pem'
				 )
			);

		// Execute request
		$data_resp = curl_exec($curl);
		
		// Check if response is received:
		if(!$data_resp) throw new Exception('Error: "' . curl_error($curl) . '" - Code: ' . curl_errno($curl), 500);
		
		// Close request to clear up some resources
		curl_close($curl);

		// Return data
		return $this->dataObj = json_decode($data_resp);
	}

	public function getToken(){
		if(!isset($this->tokenObj)) return false;
		else return $this->tokenObj;
	}

	public function getData(){
		if(!isset($this->dataObj)) return false;
		else return $this->dataObj;
	}
	
	public static function callback($code, $idp){
		$identityProviders = Config::get('identityProviders', 'OAuthLogin');

		if(empty($code)) throw new Exception("Oops. Someting went wrong during login. Please try again", 401);

		if(!isset($identityProviders[$idp])) throw new Exception("Unknown identity provider", 500);

		$client_id 		= $identityProviders[$idp]['clientId'];
		$client_secret 	= $identityProviders[$idp]['clientSecret'];
		$redirect_uri 	= $identityProviders[$idp]['redirectUrl'];
		$token_url 		= $identityProviders[$idp]['tokenUrl'];
		$api_url 		= $identityProviders[$idp]['apiUrl'];

		// instantiate authController
		$authController = new OAuthLoginController($client_id,$client_secret,$redirect_uri,$token_url);

		// request token
		if($authController->requestToken($code)){
			// request data
			if($authController->requestData($api_url)){
                // Get email here
                $email = null;
                switch ($idp) {
                    case 'linkedin':
                        // Linkedin provides primary emailaddress only. This is always a verified address.
                        $email = $authController->getData()->emailAddress;
                        break;
                    case 'google':
                        $email = $authController->getData()->email;
                        if(!$authController->getData()->verified_email) throw new Exception("Google emailaddress is not verified", 500);
                        break;
                    case 'github':
                        foreach ($authController->getData() as $data) {
                            if($data->primary && $data->verified) $email = $data->email;
                        }
                        if(is_null($email)) throw new Exception("Github primary emailaddress is not verified", 500);
                        break;
                    default:
                        throw new Exception("Unknown identity provider", 500);
                        break;
                }
                
				$authController->login($email);

			}
		}

		header('Location: '. Config::get('redirectAfterLogin', 'OAuthLogin'));
		exit;
	}
    
    private function login($email){
        if(empty($email)) throw new Exception("No emailaddress provided to login", 500);
        
        $session = Session::singleton();
        $db = Database::singleton();
        
        $conceptUserID = Concept::getConceptByLabel('UserID');
        $conceptDomain = Concept::getConceptByLabel('Domain');
        $conceptDateTime = Concept::getConceptByLabel('DateTime');
        $conceptOrg = Concept::getConceptByLabel('Organization');
        $conceptAccount = Concept::getConceptByLabel('Account');
        $conceptSession = Concept::getConceptByLabel('SESSION');
        
        // Set sessionUser
        $atom = new Atom($email, $conceptUserID);
        $accounts = $atom->ifc('AccountForUserid')->getTgtAtoms();

        // create new user
        if(empty($accounts)){
            $newAccount = Concept::getConceptByLabel('Account')->createNewAtom();
            
            // Save email as accUserid
            $relAccUserid = Relation::getRelation('accUserid', $newAccount->concept, $conceptUserID);
            $relAccUserid->addLink($newAccount, new Atom($email, $conceptUserID), false, 'OAuthLoginExtension');

            // If possible, add account to organization(s) based on domain name
            $domain = explode('@', $email)[1];
            $atom = new Atom($domain, $conceptDomain);
            $orgs = $atom->ifc('DomainOrgs')->getTgtAtoms();
            $relAccOrg = Relation::getRelation('accOrg', $newAccount->concept, $conceptOrg);
            foreach ($orgs as $org){
                $relAccOrg->addLink($newAccount, $org, false, 'OAuthLoginExtension');
            }
            
            // Account created, add to $accounts list (used lateron)
            $accounts[] = $newAccount;

        }

        if(count($accounts) > 1) throw new Exception("Multiple users registered with email $email", 401);
        
        $relSessionAccount = Relation::getRelation('sessionAccount', $conceptSession, $conceptAccount);
        $relAccMostRecentLogin = Relation::getRelation('accMostRecentLogin', $conceptAccount, $conceptDateTime);
        $relAccLoginTimestamps = Relation::getRelation('accLoginTimestamps', $conceptAccount, $conceptDateTime);
        
        foreach ($accounts as $account){				    
            // Set sessionAccount
            $relSessionAccount->addLink($session->sessionAtom, $account, false, 'OAuthLoginExtension');

            // Timestamps
            $ts = new Atom(date(DATE_ISO8601), $conceptDateTime);
            $relAccMostRecentLogin->addLink($account, $ts, false, 'OAuthLoginExtension');
            $relAccLoginTimestamps->addLink($account, $ts, false, 'OAuthLoginExtension');
        }

        $db->closeTransaction('Login successfull', true);
    }
}
?>