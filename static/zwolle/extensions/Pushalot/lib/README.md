## About
> Pushalot is the best application for sending free push notifications to Windows 8 and Windows Phone 7 and 8, using this very simple API for PHP.

## Pushalot PHP Class
> Pushalot PHP Class is a PHP wrapper class for using the Pushalot (https://pushalot.com) REST API. Have a look at their API docs (https://pushalot.com/api) for more information about all the parameters. Please note that you need to get the authorization token of your default "api" app (https://pushalot.com/manager/apps). You may also create a custom app there.

## Methods
* Pushalot([authorizationToken, proxy url, proxy user:pass])
> Constructor which takes up to 3 optional arguments.

* sendMessage(params)
> Sends the message and returns true if the message was successfully sent or false, if not. Use a key/value pair list to set the params here. See the Pushalot API (https://pushalot.com/api) docs for more information.

* getError()
> If sendMessage returned false, this function returns a detailed error.

* setProxy(url, user:pass)
> Set the proxy which cURL will use. Pass the proxy host and the login details like "user:password".
