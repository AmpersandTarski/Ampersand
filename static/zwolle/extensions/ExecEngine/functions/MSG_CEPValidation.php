<?php
// This extension provides the functions needed by MSG_CEPValidation.adl

// Enable Messaging extension: MSG_Validation
// Config::set('url', 'msg_validation', ''); // msg_validation URL where response needs to be filled in.

function CreateCvrNonce()
{   Notifications::addLog('PHP_MAJOR_VERSION = '.PHP_MAJOR_VERSION,'MESSAGING');
    if (PHP_MAJOR_VERSION < 7) return(rand(1,999999));
    Notifications::addLog('using random_int for generating the nonce','MESSAGING');
    return(random_int(1,999999)); // 'random_int' only is available from PHP 7
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