Add the following lines to your localsettings file to enable the Pushover notifications extension
https://pushover.net/

Steps:
1) You must register an application at Pushover (https://pushover.net/)
2) Enable the Pushover extension in your localsettings file (see below)
3) Register hooks or use the execEngine to sent Pushover notifications

// Enable Pushover notification extension
require_once(__DIR__ . '/extensions/Messaging/Pushover.php');
	Config::set('applicationToken', 'pushover', '<token here>'); // pushover application token
	Config::set('alwaysNotifyUsers', 'pushover', array('<userkey>', '<userkey>', '<etc>')); // array of pushover user keys that receive a copy of all notifications

// ExecEngine rule violation example:
VIOLATION(TXT "{EX}PushoverNotifications::execEnginePushNotificationOnCommit;<userKeys>;<message>;<title>;<url>")
Note:
- Multiple <userKeys> must be seperated with _AND. This is atomatically done when using an ampersand expression to fill the userKeys.
- The message is sent as html content. You can use a limited set of html tags (see https://pushover.net/api)
- The title and url are optional