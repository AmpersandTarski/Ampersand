<?php
// See the README.md file for instructions on how to use this stuff.

require_once (__DIR__ . '/lib/pushalot_api.php');

// Define hooks
$hook = array( 'class' => 'PushalotNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'Pushalot.php'
			 , 'filepath' => 'extensions/Pushalot'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => 'PushalotNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'Pushalot.php'
			 , 'filepath' => 'extensions/Pushalot'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class PushalotNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title = null, $url = null){
		
		if($userKeys == '_NULL') $userKeys = array(null);
		else $userKeys = explode('_AND', $userKeys);
		
		self::pushNotificationOnCommit($userKeys, $message, $title, $url);
	}
	
	public static function pushNotificationOnCommit($userKeys, $message, $title = null, $url = null){
		
		foreach($userKeys as $userKey){
			if(!is_null($userKey)) self::$notifications[] = array('userKey' => $userKey, 'message' => $message, 'title' => $title, 'url' => $url);
		}
		
		// Send same notification to users in 'alwaysNotifyUsers' config
		foreach((array)Config::get('alwaysNotifyUsers', 'pushalot') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				self::$notifications[] = array('userKey' => $notifyUser, 'message' => $message, 'title' => $title, 'url' => $url);
			}
		}
	}
	
	private static function pushNotification($userKey, $message, $title = null, $url = null){
		if(is_null($userKey)) throw new Exception("User key for Pushalot notification not specified", 500);
		$notification = new Pushalot($userKey);
		//$pushalot->setProxy('http://localhost:12345','user:pass');
		$success = $notification->sendMessage(array(
			'Title'=>$title,
			'Body'=>$message,
		//	'LinkTitle'=>'Pushalot.com',
			'Link'=>$url,
			'IsImportant'=>true,
			'IsSilent'=>false,
			'Image'=>'http://wiki.tarski.nl/skins/common/images/AmpersandLogo.png',
			'Source'=>'Ampersand prototype'
		));
	        if(!$success) throw new Exception("Pushalot error '$notification->getError()' sending notification to '$userKey'", 500);	
	}
	
	public static function pushNotificationCache(){
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url']);
	}

	public static function clearNotificationCache(){
		self::$notifications = array();
	}
}

?>