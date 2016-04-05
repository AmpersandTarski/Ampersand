<?php

namespace Ampersand\Extension\Messaging;

use Exception;
use Ampersand\Hooks;
use Ampersand\Config;
use Ampersand\Log\Logger;
use Pushover;

require_once (__DIR__ . '/lib/php-pushover.php');

// Define hooks
$hook = array( 'class' => '\Ampersand\Extension\Messaging\PushoverNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'Pushover.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => '\Ampersand\Extension\Messaging\PushoverNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'Pushover.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class PushoverNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Logger::getLogger('MESSAGING')->debug('Pushover[execEnginePushNotificationOnCommit'
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
		Logger::getLogger('MESSAGING')->debug('Pushover[pushNotificationOnCommit'
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
		foreach((array)Config::get('alwaysNotifyUsers', 'msg_pushover') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				if ($notifyUser != '') self::$notifications[] = array('userKey' => $notifyUser, 'message' => $message, 'title' => $title, 'url' => $url, 'urltitle' => $urltitle); // Disregard a possibly empty setting from localSettings.php
			}
		}
	}
	
	public static function pushNotificationCache(){
		Logger::getLogger('MESSAGING')->debug('Pushover[pushNotificationCache]');
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url'], $notification['urltitle']);
	}

	public static function clearNotificationCache(){
		Logger::getLogger('MESSAGING')->debug('Pushover[clearNotificationCache]');
		self::$notifications = array();
	}

	private static function pushNotification($userKey, $message, $title=null, $url=null, $urltitle=null){
		Logger::getLogger('MESSAGING')->debug('Pushover[pushNotification'
							 .']; $userKey=['.$userKey
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']');
		$notification = new Pushover();
		
		$token = Config::get('applicationToken', 'msg_pushover');
		if(is_null($token)) throw new Exception("Pushover - Application token not specified", 500);
		if(is_null($userKey)) throw new Exception("Pushover - User key not specified", 500);
		
		$notification->setToken($token);
		$notification->setUser($userKey);
		$notification->setMessage($message);
		if(!is_null($title)) $notification->setTitle($title);
		$notification->setHtml(1);
		$notification->setUrl($url);
		$notification->setUrlTitle($urltitle);
		
		if(!$notification->send()) {
			Logger::getUserLogger()->error("Pushover - Error in sending a notification to '$userKey'");
		}else{
			Logger::getUserLogger()->notice("Pushover message sent.");
		}
		
	}
	
}
?>