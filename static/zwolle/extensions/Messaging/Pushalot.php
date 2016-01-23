<?php

require_once (__DIR__ . '/lib/pushalot_api.php');

// Define hooks
$hook = array( 'class' => 'PushalotNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'Pushalot.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => 'PushalotNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'Pushalot.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class PushalotNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('Pushalot[execEnginePushNotificationOnCommit'
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
		Notifications::addLog('Pushalot[pushNotificationOnCommit'
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
		foreach((array)Config::get('alwaysNotifyUsers', 'msg_pushalot') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				if ($notifyUser != '') self::$notifications[] = array('userKey' => $notifyUser, 'message' => $message, 'title' => $title, 'url' => $url, 'urltitle' => $urltitle); // Disregard a possibly empty setting from localSettings.php
			}
		}
	}
	
	public static function pushNotificationCache(){
		Notifications::addLog('Pushalot[pushNotificationCache]','MESSAGING');
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url'], $notification['urltitle']);
	}

	public static function clearNotificationCache(){
		Notifications::addLog('Pushalot[clearNotificationCache]','MESSAGING');
		self::$notifications = array();
	}

	private static function pushNotification($userKey, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('Pushalot - $userKey=['.$userKey
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']'
		                     ,'MESSAGING');
		if(is_null($userKey)) throw new Exception("Pushalot - User/API key not specified", 500);
		$notification = new Pushalot($userKey);
		//$pushalot->setProxy('http://localhost:12345','user:pass');
		$success = $notification->sendMessage(array(
			'Title'=>$title,
			'Body'=>$message,
//		  	'LinkTitle'=>$urltitle,
//			'Link'=>$url,
			'IsImportant'=>true,
			'IsSilent'=>false,
			'Image'=>'http://wiki.tarski.nl/skins/common/images/AmpersandLogo.png',
			'Source'=>'Ampersand prototype'
		));
	        if(!$success) {
			Notifications::addError("Pushalot error '$notification->getError()' sending notification to '$userKey'");
		}else{
			Notifications::addSuccess('Pushalot message sent.');
		}

	}

}

/* Example that was provided: 
include('pushalot_api.php');
$pushalot = new Pushalot('35b9832daffc4793aa477e44c0b0910f');
//$pushalot->setProxy('http://localhost:12345','user:pass');
$success = $pushalot->sendMessage(array(
	'Title'=>'Hello world!',
	'Body'=>"This is a test message!\n\nSent by Pushalot",
	'LinkTitle'=>'Pushalot.com',
	'Link'=>'http://www.pushalot.com',
	'IsImportant'=>true,
	'IsSilent'=>false,
	'Image'=>'https://pushalot.com/Content/Images/logo.png',
	'Source'=>'PHP script'
));
echo $success?'The message was submitted.':$pushalot->getError();
*/
	
?>