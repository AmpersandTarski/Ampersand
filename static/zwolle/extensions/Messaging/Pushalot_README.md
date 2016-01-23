MSG_Pushalot README
===================

In order to start using the Pushalot notifications extension within a prototype, you must execute the following steps:
1) Enable the Pushalot extension in your localsettings file.
   This is done by adding the following lines to your 'localSettings.php' file:

	// Enable Pushalot notification extension
	require_once(__DIR__ . '/extensions/Messaging/Pushalot.php');

2) Include the Pushalot messaging module in your Ampersand script:

        INCLUDE "../Messaging/MSG-Pushalot.adl"

3) Send messages by a VIOLATION statement such as

	VIOLATION(TXT "{EX}PushalotNotifications::execEnginePushNotificationOnCommit;<authorizationtokens>;<message>;<title>;<url>")

   Notes:
   - Multiple <authorizationtokens> must be seperated with _AND. This is atomatically done when using an ampersand expression to fill the userKeys.
   - The message is sent as html content. You can use a limited set of html tags (see https://pushalot.com/api)
   - The title and url are optional

4) Users that want to receive messages through the Pushalot notification service must register an account at https://pushalot.com/ (free of charge). This will provide them with a so-called 'authorization token' (for the 'API' app). An authorization token is similar to a phone number or an email-address (it is a CEPEndpoint). Users can create additional authorization tokens at https://pushalot.com/manager/authorizations
