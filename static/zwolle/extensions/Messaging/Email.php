<?php

require_once (__DIR__ . '/lib/class.phpmailer.php');

// Define hooks
$hook = array( 'class' => 'EmailNotifications'
			 , 'function' => 'pushNotificationCache'
			 , 'filename' => 'Email.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array()
			 );
Hooks::addHook('postDatabaseCommitTransaction', $hook);

$hook = array( 'class' => 'EmailNotifications'
			 , 'function' => 'clearNotificationCache'
			 , 'filename' => 'Email.php'
			 , 'filepath' => 'extensions/Messaging'
			 , 'params' => array());
Hooks::addHook('postDatabaseRollbackTransaction', $hook);

class EmailNotifications {
	
	private static $notifications = array();
	
	public static function execEnginePushNotificationOnCommit($userKeys, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('Email[execEnginePushNotificationOnCommit'
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
		Notifications::addLog('Email[pushNotificationOnCommit'
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
		foreach((array)Config::get('alwaysNotifyUsers', 'msg_email') as $notifyUser){
			if(!in_array($notifyUser, $userKeys)){ // prevent duplicate notifications
				if ($notifyUser != '') self::$notifications[] = array('userKey' => $notifyUser, 'message' => $message, 'title' => $title, 'url' => $url, 'urltitle' => $urltitle); // Disregard a possibly empty setting from localSettings.php
			}
		}
	}
	
	public static function pushNotificationCache(){
		Notifications::addLog('Email[pushNotificationCache]','MESSAGING');
		foreach (self::$notifications as $notification) self::pushNotification($notification['userKey'], $notification['message'], $notification['title'], $notification['url'], $notification['urltitle']);
	}

	public static function clearNotificationCache(){
		Notifications::addLog('Email[clearNotificationCache]','MESSAGING');
		self::$notifications = array();
	}

	private static function pushNotification($emailAddr, $message, $title=null, $url=null, $urltitle=null){
		Notifications::addLog('Email[pushNotification'
		                     .']; $emailAddr=['.$emailAddr
		                     .']; $message=['.$message
		                     .']; $title=['.$title
		                     .']; $url=['.$url
		                     .']; $urltitle=['.$urltitle
		                     .']'
		                     ,'MESSAGING');
		// adapted from http://phpmailer.worxware.com/?pg=examplebgmail
		$config = Config::get('sendEmailConfig', 'msg_email');
		$from = $config['from'];
		$username = $config['username'];
		$password = $config['password'];
		Notifications::addLog('Email.php - Username = '.$username,'MESSAGING');

		$mail = new PHPMailer;
	
		$mail->IsSMTP();				// Set mailer to use SMTP
	     // $mail->SMTPDebug = 1;
		$mail->Host = 'smtp.gmail.com';	// Specify main and backup server
		$mail->SMTPSecure = 'ssl';		// Enable encryption, 'ssl' also accepted
		$mail->Port = 465;
		$mail->SMTPAuth = true;			// Enable SMTP authentication
		
		$mail->Username = $username;	// SMTP username (for GMAIL)
		$mail->Password = $password;	// SMTP password
		
		$mail->From = $from;
		$mail->FromName = 'Ampersand Prototype';
		
		$mail->AddAddress($emailAddr);  // Add a recipient, e.g. $to = 'rieks.joosten@tno.nl', 'Rieks Joosten'
		$mail->Subject = $title;

//      $message = $message . 'optional URL';
        if($url != '_NULL' && $url != '') {
           $mail->IsHTML(true);  // make sure we send in HTML
           if($urltitle != '_NULL' && $urltitle != '') {
              $message = '<p>'.$message.'</p><p><a href='.$url.'>'.$urltitle.'</a></p>';
           } else {
              $message = $message.'<a'.$urltitle.'</a>';
           }
		Notifications::addLog('Email message refactored to: ['.$message.']','MESSAGING');
        }
		$mail->Body    = $message;
		
		$mail->WordWrap = 50;			// Set word wrap to 50 characters
		
		if(!$mail->Send()){
			Notifications::addError('Mailer Error: ' . $mail->ErrorInfo);
		}else{
			Notifications::addSuccess('Email message sent.');
		}

	}

}

?>