<?php

// UI
AngularApp::addJS('extensions/OAuthLogin/ui/js/LoginModule.js');
$GLOBALS['navBar']['roleMenu'][] = array ('url' => 'extensions/OAuthLogin/ui/views/MenuItem.html');

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
				 , CURLOPT_HTTPHEADER => array('Content-Type: application/x-www-form-urlencoded')
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
}
?>