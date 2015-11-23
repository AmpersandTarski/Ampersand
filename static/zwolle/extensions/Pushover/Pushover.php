<?php
require_once (__DIR__ . '/lib/php-pushover.php');

// Define hooks
$hook = array( 'class' => 'PushoverNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'Pushover.php'
			 , 'filepath' => 'extensions/Pushover'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => 'PushoverNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'Pushover.php'
			 , 'filepath' => 'extensions/Pushover'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class PushoverNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $title, $message, $url = null){
		
		if($userKeys == '_NULL') $userKeys = array(null);
		else $userKeys = explode('_AND', $userKeys);
		
		self::pushNotificationOnCommit($userKeys, $title, $message, $url);
	}
	
	public static function pushNotificationOnCommit($userKeys, $title, $message, $url = null){
		
		foreach($userKeys as $userKey){
			if(!is_null($userKey)) self::$notifications[] = array('userKey' => $userKey, 'title' => $title, 'message' => $message, 'url' => $url);
		}
		
		// Send same notification to users in 'alwaysNotifyUsers' config
		foreach((array)Config::get('alwaysNotifyUsers', 'pushover') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				self::$notifications[] = array('userKey' => $notifyUser, 'title' => $title, 'message' => $message, 'url' => $url);
			}
		}
	}
	
	private static function pushNotification($userKey, $title, $message, $url = null){
		$notification = new Pushover();
		
		$token = Config::get('applicationToken', 'pushover');
		if(is_null($token)) throw new Exception("Application token for Pushover not specified", 500);
		if(is_null($userKey)) throw new Exception("User key for Pushover notification not specified", 500);
		
		$notification->setToken($token);
		$notification->setUser($userKey);
		$notification->setTitle($title);
		$notification->setMessage($message);
		$notification->setHtml(1);
		$notification->setUrl($url);
		
		if(!$notification->send()) throw new Exception("Something went wrong sending a Pushover notification to '$userKey'", 500);	
		
	}
	
	public static function pushNotificationCache(){
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['title'], $notification['message'], $notification['url']);
	}

	public static function clearNotificationCache(){
		self::$notifications = array();
	}
}

?>