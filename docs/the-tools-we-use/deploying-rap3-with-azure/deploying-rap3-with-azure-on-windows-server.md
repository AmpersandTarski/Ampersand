# Deploying on a windows server machine \(Azure cloud\)

In order to obtain a working machine with RAP3, we had difficulties getting everything to work on an Ubuntu machine. This is probably because of a lack of knowledge on our side about that infrastructure.

Because of these difficulties, we decided to install everything on a Windows server. This is a log of what we did.

## 1 Setting up the virtual machine

Resourcegroep\([wijzigen](https://portal.azure.com/)\)

[Ampersand](https://portal.azure.com/)

Computernaam

Renium

Besturingssysteem

Windows

Grootte

Standard DS2 versie 2 \(2 kernen, 7 GB geheugen\)

Openbaar IP-adres

[52.174.32.40](https://portal.azure.com/)

Virtueel netwerk/subnet

[Ampersand-vnet/default](https://portal.azure.com/)

## 2 Getting the required software

Once the VM has been launched, [connect](https://docs.microsoft.com/en-us/azure/virtual-machines/windows/connect-logon "How to connect to an azure VM") to it. The following software was installed, using default settings, unless stated otherwise:

1. Google Chrome
2. [Git](https://git-scm.com/download/) \(version 2.12.2.2 - 64-bit\)
3. [Xampp](https://www.apachefriends.org/download.html) \(version 7.0.15 / PHP 7.0.15\)
4. [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
5. [graphviz](http://www.graphviz.org/Download_windows.php)
6. [MiKTeX](https://miktex.org/download) \(all defaults, except choose to install missing packages on-the-fly: Yes\)
7. [Composer](https://getcomposer.org/doc/00-intro.md#installation-windows)

## 3 Configuring

Now all the software is in place, some of it needs configuration. This is what I did:

| What | Why | How I did it |
| :--- | :--- | :--- |
| We need a place to store all Git repo's | All repo's are nicely together | Create directory **c:\git** |
| Clone the ampersand repo | So we can build ampersand executable | git clone [https://github.com/AmpersandTarski/Ampersand.git](https://github.com/AmpersandTarski/Ampersand.git) |
| Clone the ampersand-models repo | So we have the models at hand | git clone [https://github.com/AmpersandTarski/Ampersand-models.git](https://github.com/AmpersandTarski/Ampersand-models.git) |
| Setup stack for ampersand repo | we need stack to be able to generate ampersand. It will put GHC in place and other stuff. | in c:\git\ampersand say 'stack setup' |
| Restart the shell | because MSYS2 says so! | restart the shell |
| Build ampersand.exe | That is what we need to run RAP3 | in c:\git\ampersand say 'stack install' |
| Let apache listen at port 8088 | Use a non-standard port, so it can be configured with tunneling | Go to the xampp control panel, choose config in the Apache row. httpd.conf wil open in notepad. Edit the listen port number. Also click the config button at the upper right corner of the xampp control panel. Then click Service and Port Settings. Edit the Apache Main Port. Also check the Autostart of modules: Apache and MySQL. Stop and start Apache \(from the xampp control panel\) |
| Build RAP3 application |  | goto RAP3 directory; say: ampersand.exe --meta-tables --add-semantic-metamodel --verbose  -pC:\xampp\htdocs\RAP3 RAP3.adl |
| Putting Graphviz into your path | So ampersand can use dot and neato executables to build graphics | The path to add is C:\Program Files \(x86\)\Graphviz2.38\bin |
| create a user for ampersand in mysql | So RAP3 can access mysql | Use phpmyadmin to do this.  |

Changes to Localsettings.php

* Under RAP3 settings, change the path where ampersand can be found. On Remium, this will be C:\Users\ampersandadmin\AppData\Roaming\local\bin\ampersand.exe



Unfortunately, we ran into a [bug with Graphviz](http://www.graphviz.org/mantisbt/view.php?id=2108). It seems, that graphviz doesn't run on windows server. For that reason, Remium doesn't work currently. 



