<?php
/* NOTES:
1) Make sure that the file 'php_openssl.dll' is in directory C:\xampp\php\ext\ (or wherever else you have put it)
2) Make sure that the file 'php.ini' (in directory C:\XAMPP\php) contains the following line:
     extension=php_openssl.dll
   If you need to make changes to php.ini, then remember to reboot the Apache server.
3) Make sure the global variables (at the top of the function SendEmail) are available.
   Put them in the 'pluginsettings.php' file (same dir as 'dbsettings.php')
4) Make sure the class libraries 'class.phpmailer.php', 'class.pop3.php' and 'class.smtp.php' are available
*/

require_once (__DIR__ . '/SendEmail/lib/class.phpmailer.php');

/* Config params for SendEmail function of ExecEngine (now using Gmail)
 * 
 */
Config::set('sendEmailConfig', 'execEngine', array('from' => '', 'username' => '', 'password' => '')); // Copy this line to localSettings.php and provide settings

function SendEmail($to,$subject,$message){ 
	// adapted from http://phpmailer.worxware.com/?pg=examplebgmail
	$config = Config::get('sendEmailConfig', 'execEngine');
	$from = $config['from'];
	$username = $config['username'];
	$password = $config['password'];

	Notifications::addLog('Username = '.$username, 'ExecEngine');
	
	$mail = new PHPMailer;

	$mail->IsSMTP();				// Set mailer to use SMTP
	// $mail->SMTPDebug = 1;
	$mail->Host = 'smtp.gmail.com';	// Specify main and backup server
	$mail->SMTPSecure = 'tls';		// Enable encryption, 'ssl' also accepted
	$mail->Port = 587;
	$mail->SMTPAuth = true;			// Enable SMTP authentication
	
	$mail->Username = $username;	// SMTP username (for GMAIL)
	$mail->Password = $password;	// SMTP password
	
	$mail->From = $from;
	$mail->FromName = 'Ampersand Prototype';
	
	$mail->AddAddress($to);			// Add a recipient, e.g. $to = 'rieks.joosten@tno.nl', 'Rieks Joosten'
	$mail->Subject = $subject;
	$mail->Body    = $message;
	
	$mail->WordWrap = 50;			// Set word wrap to 50 characters
	
	if(!$mail->Send()){
		Notifications::addError('Mailer Error: ' . $mail->ErrorInfo);
	}else{
		Notifications::addSuccess('Email message sent.');
	}
}
?>