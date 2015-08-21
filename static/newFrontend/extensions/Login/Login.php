<?php
// UI
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/Login/ui/js/LoginModule.js';

class OAuthController {

	private $token_url;
	private $client_id;
	private $client_secret;
	private $redirect_uri;

	private $tokenObj;
	private $dataObj;

	public $exception;

	function __construct($client_id,$client_secret,$redirect_uri,$token_url){
		// start PHP session
		session_start();

		$this->client_id = $client_id;
		$this->client_secret = $client_secret;
		$this->redirect_uri = $redirect_uri;
		$this->token_url = $token_url;
	}

	public function requestToken($code){
		// setup token request
		$token_request = array(
				'token_url' => $this->token_url,
				'arguments' => array(
						'client_id' => $this->client_id,
						'client_secret' => $this->client_secret,
						'grant_type' => 'authorization_code',
						'code' => $code,
						//'code' => '4/5V_rEqL-6_yCNDUXeL0I302o40ANb-aH29_yGL74yXU#',
						'redirect_uri' => $this->redirect_uri
				)
		);

		// make HTTP POST request to OAUTH host to get token
		$curl = curl_init();
		curl_setopt_array($curl, array(
		CURLOPT_RETURNTRANSFER => 1,
		CURLOPT_URL => $token_request['token_url'],
		CURLOPT_USERAGENT => 'Semantic tooling tester',
		CURLOPT_POST => 1,
		CURLOPT_POSTFIELDS => $token_request['arguments'],
		));

		// Send the request & save response to $resp
		$token_resp = curl_exec($curl);

		// check if response is received:
		if(!$token_resp){
			// throw error if necessary
			$this->exception = 'Error: "' . curl_error($curl) . '" - Code: ' . curl_errno($curl);
			return false;
		}

		// Close request to clear up some resources
		curl_close($curl);

		// decode token JSON response to stdObj and return
		$this->tokenObj = json_decode($token_resp);
		if(isset($this->tokenObj->access_token)){
			return true;
		} else {
			$error = "Error: Someting went wrong getting token, \"".$this->tokenObj->error."\".";
			if(isset($this->tokenObj->error_description)){
				$error .= " Description: \"".$this->tokenObj->error_description."\"";
			}
			$this->exception = $error;
			return false;
		}
	}

	public function requestData($api_url){
		if(!isset($this->tokenObj)){
			$this->exception = 'Error: No token set.';
			return false;
		}
		// Do a HTTP HEADER request to the API_URL
		$curl = curl_init();
		curl_setopt_array($curl, array(
		CURLOPT_RETURNTRANSFER => 1,
		CURLOPT_URL => $api_url,
		CURLOPT_USERAGENT => 'Semantic tooling tester',
		CURLOPT_HTTPHEADER => array(
		'Authorization: ' . $this->tokenObj->token_type . ' ' . $this->tokenObj->access_token,
		),
		));

		// execute request
		$data_resp = curl_exec($curl);
		// check if response is received:
		if(!$data_resp){
			$this->exception = 'Error: "' . curl_error($curl) . '" - Code: ' . curl_errno($curl);
			return false;
		}

		return $this->dataObj = json_decode($data_resp);
	}

	public function setSessionData(){
		if(!isset($this->dataObj)){
			$this->exception = 'Error: No data to set session.';
			return false;
		}
		$_SESSION["usr_email"] = $this->dataObj->email;
		$_SESSION["usr_pic"] = $this->dataObj->picture;
	}

	public function logout(){
		session_destroy();
	}

	public function getToken(){
		if(!isset($this->tokenObj)){
			$this->exception = 'Error: No data to return.';
			return false;
		}
		return $this->tokenObj;
	}

	public function getData(){
		if(!isset($this->dataObj)){
			$this->exception = 'Error: No data to return.';
			return false;
		}
		return $this->dataObj;
	}
}
?>