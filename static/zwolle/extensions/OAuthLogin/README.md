# The OAuthLogin extension

## Purpose of the extension
The OAuthLogin extension allows you to easily add a user (account) registration to your Ampersand prototype, which allows for 
* assigning Roles to users, and add role-based access control (RBAC) to INTERFACES.
* add session variables, and use them for controlling access to data.

## How to INSTALL the extension
* Run 'gulp project' command after generating the prototype
* Enable the extension in your localSettings.php file. See below

## How to CONFIGURE the extension
Step 1: Choose identity providers you would like to support (e.g. Github, LinkedIn, Google, etc)
    - Register application at identity provider

Step 2: Add and configure supported identity providers in your `localSettings.php` file.
```php
require_once(__DIR__ . '/extensions/OAuthLogin/OAuthLogin.php');
	Config::set('redirectAfterLogin', 'OAuthLogin', 'https://[server]/#/');
	Config::set('redirectAfterLoginFailure', 'OAuthLogin', 'https://[server]/#/');
	Config::set('identityProviders', 'OAuthLogin', 
					    ['linkedin' => 
							    ['name' => 'LinkedIn'
                                ,'logoUrl' => 'extensions/OAuthLogin/ui/images/logo-linkedin.png'
                                ,'authBase' => 'https://www.linkedin.com/uas/oauth2/authorization'
								,'redirectUrl' => 'https://[server]/api/v1/oauthlogin/callback/linkedin'
								,'clientId' => '[string]'
								,'clientSecret' => '[string]'
								,'tokenUrl' => 'https://www.linkedin.com/uas/oauth2/accessToken'
								,'apiUrl' => 'https://api.linkedin.com/v1/people/~:(emailAddress)?format=json'
								,'scope' => 'r_emailaddress'
								,'state' => '[string]' // A unique string value of your choice that is hard to guess. Used to prevent CSRF
								,'emailField' => 'emailAddress'
                                ]
						,'google' => 
							    ['name' => 'Google'
                                ,'logoUrl' => 'extensions/OAuthLogin/ui/images/logo-google.png'
                                ,'authBase' => 'https://accounts.google.com/o/oauth2/auth'
								,'redirectUrl' => 'https://[server]/api/v1/oauthlogin/callback/google'
								,'clientId' => '[string]'
								,'clientSecret' => '[string]'
								,'tokenUrl' => 'https://accounts.google.com/o/oauth2/token'
								,'apiUrl' => 'https://www.googleapis.com/userinfo/v2/me'
								,'scope' => 'https://www.googleapis.com/auth/userinfo.email'
								,'state' => '[string]' // A unique string value of your choice that is hard to guess. Used to prevent CSRF
								,'emailField' => 'email'
                                ]
                        ,'github' =>
                                ['name' => 'GitHub'
                                ,'logoUrl' => 'extensions/OAuthLogin/ui/images/logo-github.png'
                                ,'authBase' => 'https://github.com/login/oauth/authorize'
                                ,'redirectUrl' => 'https://[server]/api/v1/oauthlogin/callback/github'
                                ,'clientId' => '[string]'
                                ,'clientSecret' => '[string]'
                                ,'tokenUrl' => 'https://github.com/login/oauth/access_token'
                                ,'apiUrl' => 'https://api.github.com/user/emails'
                                ,'scope' => 'user:email'
                                ,'state' => '[string]' // A unique string value of your choice that is hard to guess. Used to prevent CSRF
                                ]
                        ]
				);
```
Step 3: Add required concepts and relations to your Ampersand script. See SIAM OAuth module.

Step 4: Test the OAuth protocol on your local machine:
    - add example.com to redirect to localhost in host table (c:/windows/system32/drivers/etc/hosts)
    - run cmd: ipconfig /flushdns
    - restart browser and check if example.com redirect to your local machine
    - replace 'https://[server]' in localsettings above with example.com

## How to USE the extension (in ADL scripts)
See SIAM OAuth module. Todo, add explanation here.

## Notes
* The OAuthLogin extension uses [curl](http://php.net/manual/en/book.curl.php) to get/request data from the identity providers. To verify the peer curl needs a file with root certificates, which is provided in the cacerp.pem file in this folder. Goto https://curl.haxx.se/docs/caextract.html to update the cacert.pem once in a while.