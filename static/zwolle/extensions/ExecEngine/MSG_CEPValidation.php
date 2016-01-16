<?php
// This extension provides the functions needed by MSG_CEPValidation.adl

// Enable Messaging extension: MSG_Validation
// Config::set('url', 'msg_validation', ''); // msg_validation URL where response needs to be filled in.

function CreateCvrNonce()
{   if (PHP_MAJOR_VERSION < 7) // 'random_int' only is available from PHP 7
	{ $nonce = rand(1,999999);
	} else 
	{ $nonce = random_int(1,999999);
	}
    Notifications::addLog('Generated nonce '.$nonce,'MESSAGING');
}

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