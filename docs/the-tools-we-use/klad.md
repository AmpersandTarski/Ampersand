# Ampersand at the Open University of the Netherlands \(OUNL\)

In early 2016 the need arose to replace the RAP2 implementation of Ampersand by a RAP3 implementation, because RAP2 was insufficiently maintainable. This environment is used by students for completing the course Rule Based Design \(OBR, code ...\).  This implementation is hosted by ICTS, the IT-department of the university. We chose to implement RAP3 as a maintainable environment.

This chapter is an account of the installation process. It serves the following purposes:

1. It is an example for others who want to deploy Ampersand.  
   We get requests now and then by people who want to deploy Ampersand, so we figured it is nice to have a documented example for them.

2. It documents the installation we made for the Open University.  
   We want maintenance of RAP3 to be transferrable to other persons, so we need to document the choices made and the reasons for making them.

3. It contains all information needed to make a deployment script for automated deployment.  
   We want to automate deployment, so that RAP3 will always be up to date with the most recent stable release of Ampersand.

Each step in the installation process gets a separate section in this text. It is not necessary to do them in the given order.dir

## 1. Setting up a machine

I got a server from the Open University's IT-department.

The following settings apply:

|  |  |
| :--- | :--- |
| server name | lnx-hrl-148v |
| OS | SUSE Linux Enterprise Server 12 SP2, vs. 12.2 |
| Admin user name | lru |
| verification type | password \(Lloyd Rutledge knows the password\) |
| Inbound port: HTTP | TCP/80 |
| Inbound port: HTTPS | TCP/443 |
| Inbound port: SSH | TCP/22 \(only via a VPN-tunnel\) |
| Inbound port: SFTP | TCP/22 |
| Public IP-adres | 145.20.188.148 |
| PHP version \(RAP3 requires PHP version 5.6 or higher\) | 7.0.7 |

| `{APPDIR}`=  the directory into which RAP3 files will be deployed | /srv/www/htdocs |
| :--- | :--- |
| `{APPACC}`=  the account under which the RAP3 application will run \(the apache account, i.e. ${APACHE\_RUN\_USER} c.q. ${APACHE\_RUN\_GROUP} as defined in apache2.conf\) |  |
| `{APPHOST}` =  the URI of the machine that hosts the [RAP3 ](https://spreg.mydomain.org%29\)application \(e.g. 'mydomain.org', or 'rap3.mydomain.org'\) |  |
| `{APPPORT}` =  the port at which the Apache server will be listening | 80 |
| `{APPURI}` = the URI at which the RAP3 application will be accessible for browsers \(e.g. 'mydomain.org/[RAP3](https://spreg.mydomain.org%29\)', or '[RAP3](https://spreg.mydomain.org%29\).mydomain.org'\) |  |
| `{APPURL}` = the full name for calling the application \(e.g. [https://mydomain.org:8080/R](https://mydomain.org:8080/spreg)AP3', or [https://RAP3.mydomain.org\](https://spreg.mydomain.org%29\) |  |

I have checked that the server works by browsing to "[http://145.20.188.148/test.php](http://145.20.188.148/test.php)". I have been able to access this machine through SSH, using the Admin user name and password. I If have been able to access this machine through SSH \(using PUTTY\), but only after installing a VPN-tunnel to the server \(using Pulse Secure\).  In the sequel, I will refer to this machine as "the server".

## 2. Getting MariaDB \(MySQL\) and phpMyAdmin to work

To run RAP3 requires Apache and MySQL. On a preinstalled MySQL-server you will need the database administrator password to set it up for Ampersand. This step requires hardware, so you must have finished section 1 successfully.

On this server, there is no MySQL, so I had to install it. I followed the instructions on `https://en.opensuse.org/SDB:LAMP_setup#Setting_up_MariaDB`.

First I installed `mariadb`and `mariadb-tools`:

`sudo zypper in mariadb mariadb-tools`

Then I started MariaDB:

`lru@lnx-hrl-148v:~> sudo systemctl start mysql`

To make sure the server will start at every boot:

`lru@lnx-hrl-148v:~> sudo systemctl enable mysql`

To set up MariaDB, I ran:

`sudo mysql_secure_installation`

By carefully following the instructions, I set up the MariaDB and chose root password `RAP3root`.

However, here the procedure failed:

`/usr/bin/mysql_secure_installation: line 234: .my.cnf.14336: No space left on device`

After logging into phpMyAdmin, create a user called 'ampersand' with password 'ampersand' and host 'localhost', in compliance with the defaults used in the Ampersand compiler. RAP3 requires at least the following authorizations:

![](/assets/MySQL authorization.png)

As a consequence of these settings, users of RAP3 will not be able to reinstall RAP3, if we accidentally leave the "reinstall database" option alive.

## 3. Uploading and running RAP3

To run RAP3, the web-application must be installed on /srv/www/htdocs. This step requires sections 1 and 2 to be finished successfully. It also requires you to have a complete RAP3 web-application available for uploading to the server. If you don't have that web-application, you need to build it. Upon completion of step 9 you will have built that web-application by yourself.

To upload RAP3, I followed the instructions on [https://docs.bitnami.com/azure/faq/\#how-to-upload-files-to-the-server-with-sftp](https://docs.bitnami.com/azure/faq/#how-to-upload-files-to-the-server-with-sftp) to upload the RAP3 web-application from my laptop onto the server. I put it on /srv/www/htdocs, which is the location of web-applications on this particular configuration. \(On vanilla Linux this would be on /var/www, I guess\). You must change the authorization of the 'log' directory \(.../htdocs/RAP3/log/\) to 757 \(public write access\) or else the application won't work. This screenshot should show the situation after the transfer \(TODO: update the screenshot\):![](/assets/Filezilla transfer confirmation.png)

You can test whether this is successful by browsing to `145.20.188.148/RAP3/`

It should show \(TODO: update the screenshot\):

![](/assets/initial RAP3 screen.png)

If you need to restart the apache server for whatever reason, here is the command:

`sudo rcapache2 restart`

## 3. Installing Haskell

In order to build an Ampersand-compiler, we need a Haskell installation. We only need a working machine, so this step merely requires section 1 to be finished successfully.

You need Haskell stack for installing Haskell. I found stack already on this machine. It is installed as`/usr/local/bin/stack`.

I did not get Haskell running.

## 4. Filling the Git repository with Ampersand files and Ampersand models

To build an Ampersand-compiler, we need the Ampersand source files, which reside in a GitHub repository. We can download these source files on a fresh server, so this step merely requires section 1 to be finished successfully. The RAP3 source files reside in a GitHub repository as well, so we'll just clone both repositories into the server.

Git comes preconfigured in Bitnami's LAMP configuration. I have used Git on the command line to get the Ampersand source code and the Ampersand model repository cloned onto the server.

I have created `/home/ampersandadmin/git` for storing the local clones. Here is what I did:

`mkdir ~/git`

`cd ~/git`

`git clone https://github.com/AmpersandTarski/Ampersand`

`git clone https://github.com/AmpersandTarski/Ampersand-models`

This failed. The system says

`Cloning into 'Ampersand'...`

`error: copy-fd: write returned: No space left on device`

`fatal: cannot copy '/usr/share/git-core/templates/description' to '/home/lru/git/Ampersand/.git/description': No space left on device`

Upon success, the directory `/home/ampersandadmin/git/Ampersand` should contain the source code of the Ampersand compiler. The directory `/home/ampersandadmin/git/Ampersand-models` should contain the source code of the Ampersand models.

## 5. Creating an Ampersand-compiler

To generate RAP3 we need an Ampersand-compiler. The RAP3 user will also use that compiler. For both reasons, we need a working Ampersand compiler on the server. This step requires sections 3 and 4 to be finished successfully.

With the source code of the Ampersand-compiler on the system, I tried to create an executable by running `stack install`. Here is what I did:

`cd ~/git/Ampersand`

`stack setup`

`stack install`

Something is still wrong, because this doesn't work.

## 6. Installing LaTeX and GraphViz

When the RAP3-user generates documentation, RAP3 will call on pdflatex, neato and dot. I found pdflatex already working. So I only had to install GraphViz. This can be done on a fresh server, so this step only requires section 1 to be finished successfully.

For generating pictures, Ampersand needs the commands `dot` and `neato`. For that purpose I installed:

```
lru@lnx-hrl-148v:~> sudo zypper install graphviz
```

That too worked.

## 

## 9. Generating the RAP3 application

This step requires sections 4 and 5 to be finished successfully.

## 10. Last minute changes before going to production

1. In the source code of RAP3, in the file SIAM\_importer.adl, the interface RAP3\_LoginForDevelopment.ifc must be disabled and the interface RAP3\_LoginForProduction.ifc must be enabled.
2. Is there anything we must alter in localsettings.php before going live?



