<?php

namespace Ampersand\Extension\Messaging;

use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Log\Logger;
use MessageBird;

require_once (__DIR__ . '/lib/class.MessageBird.php');

// Define hooks
$hook = array( 'class' => '\Ampersand\Extension\Messaging\SMSNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'SMS.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => '\Ampersand\Extension\Messaging\SMSNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'SMS.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class SMSNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Logger::getLogger('MESSAGING')->debug('SMS[execEnginePushNotificationOnCommit'
		                     .']; $userKeys=['.$userKeys
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']');

		if($userKeys == '_NULL') $userKeys = array(null);
		else $userKeys = explode('_AND', $userKeys);
		
		self::pushNotificationOnCommit($userKeys, $message, $title, $url, $urltitle);
	}
	
	public static function pushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Logger::getLogger('MESSAGING')->debug('SMS[pushNotificationOnCommit'
		                     .']; $userKeys=['.$userKeys
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']');
		
		foreach($userKeys as $userKey){
			if(!is_null($userKey)) self::$notifications[] = array('userKey' => $userKey, 'message' => $message, 'title' => $title, 'url' => $url, 'urltitle' => $urltitle);
		}
		
		// Send same notification to users in 'alwaysNotifyUsers' config
		foreach((array)Config::get('alwaysNotifyUsers', 'msg_SMS') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				if ($notifyUser != '') self::$notifications[] = array('userKey' => $notifyUser, 'message' => $message, 'title' => $title, 'url' => $url, 'urltitle' => $urltitle); // Disregard a possibly empty setting from localSettings.php
			}
		}
	}
	
	public static function pushNotificationCache(){
		Logger::getLogger('MESSAGING')->debug('SMS[pushNotificationCache]');
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url'], $notification['urltitle']);
	}

	public static function clearNotificationCache(){
		Logger::getLogger('MESSAGING')->debug('SMS[clearNotificationCache]');
		self::$notifications = array();
	}

	private static function pushNotification($SMSAddr,$message, $title=null, $url=null, $urltitle=null){
			Logger::getLogger('MESSAGING')->debug('UNTESTED !!! SMS[pushNotification'
			                     .']; $SMSAddr=['.$SMSAddr
			                     .']; $message=['.$message
			                     .']; $title=['.$title
			                     .']; $url=['.$url
			                     .']; $urltitle=['.$urltitle
			                     .']');
	
	/* Config params for SendSMS function of ExecEngine (using MessageBird.com)
	 * Set the sender, could be a number (16 numbers) or letters (11 characters)
	 * 
	 */
	// Copy the following line to localSettings.php and provide settings
	// Config::set('sendSMSConfig', 'execEngine', array('username' => '', 'password' => '', 'sender' => '')); 
		$config = Config::get('sendSMSConfig', 'msg_SMS');
		$username = $config['username'];
		$password = $config['password'];
		$sender = $config['sender'];
	
		Logger::getLogger('MESSAGING')->debug('Username = '.$username);
		
		// Set the Messagebird username and password, and create an instance of the MessageBird class
		$sms = new MessageBird($username, $password);
		
		// Set the sender, could be a number (16 numbers) or letters (11 characters)
		$sms->setSender($sender);
		
		// Add the destination mobile number.
		// This method can be called several times to add have more then one recipient for the same message
		$sms->addDestination($SMSAddr); //e.g. $sms->addDestination('31600000000');
		
		// Set an reference, optional
		// $sms->setReference('123456789');
		
		// Set a schedule date-time, optional
		// $sms->setTimestamp('2014-01-01 10:02');
		
		// Replace non GSM-7 characters by appropriate valid GSM-7 characters
		// $sms->setReplacechars(false);
		
		// If you want a dlr notification of the message send to another url then that you have set on the web site, you can use this parameter. Don't forget to set a reference!
		// $sms->setDlrUrl('http://www.example.com/dlr_url.php');
		
		// If $test is TRUE, then the message is not actually sent or scheduled, and there will be no credits deducted.
	       Logger::getLogger('MESSAGING')->debug("SMS testing is set to TRUE (messages are not actually sent)");
		   $sms->setTest(true);
		
		// Send the message to the destination(s)
		$sms->sendSms($message);
	
		if ($sms->getResponseCode() =="01") {
		    Logger::getUserLogger()->notice("SMS message sent.");
	    } else
	    { Logger::getUserLogger()->error('SMS error: ' . $sms->getResponseMessage());
	    }
		Logger::getLogger('MESSAGING')->debug("SMS Response: " . $sms->getResponseMessage());
		Logger::getLogger('MESSAGING')->debug("SMS Balance: " . $sms->getCreditBalance());
	}
}
?>