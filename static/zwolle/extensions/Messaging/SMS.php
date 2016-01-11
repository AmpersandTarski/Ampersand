<?php

require_once (__DIR__ . '/lib/class.MessageBird.php');

// Define hooks
$hook = array( 'class' => 'SMSNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'SMS.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => 'SMSNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'SMS.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class SMSNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('SMS[execEnginePushNotificationOnCommit'
		                     .']; $userKeys=['.$userKeys
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']'
		                     ,'MESSAGING');

		if($userKeys == '_NULL') $userKeys = array(null);
		else $userKeys = explode('_AND', $userKeys);
		
		self::pushNotificationOnCommit($userKeys, $message, $title, $url, $urltitle);
	}
	
	public static function pushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('SMS[pushNotificationOnCommit'
		                     .']; $userKeys=['.$userKeys
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']'
		                     ,'MESSAGING');
		
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
		Notifications::addLog('SMS[pushNotificationCache]','MESSAGING');
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url'], $notification['urltitle']);
	}

	public static function clearNotificationCache(){
		Notifications::addLog('SMS[clearNotificationCache]','MESSAGING');
		self::$notifications = array();
	}

	private static function pushNotification($SMSAddr,$message, $title=null, $url=null, $urltitle=null){
			Notifications::addLog('UNTESTED !!! SMS[pushNotification'
			                     .']; $SMSAddr=['.$SMSAddr
			                     .']; $message=['.$message
			                     .']; $title=['.$title
			                     .']; $url=['.$url
			                     .']; $urltitle=['.$urltitle
			                     .']'
			                     ,'MESSAGING');
	
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
	
		Notifications::addLog('Username = '.$username, 'MESSAGING');
		
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
	       Notifications::addLog("SMS testing is set to TRUE (messages are not actually sent)", 'MESSAGING');
		   $sms->setTest(true);
		
		// Send the message to the destination(s)
		$sms->sendSms($message);
	
		if ($sms->getResponseCode() =="01") 
	    { Notifications::addSuccess('SMS message sent.');
	    } else
	    { Notifications::addError('SMS error: ' . $sms->getResponseMessage());
	    }
		Notifications::addLog("SMS Response: " . $sms->getResponseMessage(), 'MESSAGING');
		Notifications::addLog("SMS Balance: " . $sms->getCreditBalance(), 'MESSAGING');
	}
}
?>