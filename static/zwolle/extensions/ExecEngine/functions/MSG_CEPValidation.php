<?php
// This extension provides the functions needed by MSG_CEPValidation.adl

// Enable Messaging extension: MSG_Validation
// Config::set('url', 'msg_validation', ''); // msg_validation URL where response needs to be filled in.

function CreateCvrURLText()
{	$url = Config::get('url', 'msg_validation');
    Notifications::addLog('Using URL for filling in response: '.$url,'MESSAGING');
	return($url);
}

function CreateCvrMsgTitle($Nonce)
{ 	Notifications::addLog('Created a challenge message for CEPValidation using ['.$Nonce,'MESSAGING');
	return('Validation code: '.$Nonce);
}

function CreateCvrMsgText($Nonce)
{ 	return('Please enter the following number in the application: '.$Nonce);
}


?>