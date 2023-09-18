---
description: >-
  Once RAP4 is running, there are maintenance tasks you might want to do. The
  most frequently used tasks are described here for RAP-3 maintainers to refer
  to.
---

# Maintaining RAP4

The purpose of maintaining software is to guarantee continuous operation for all users. In this chapter we use the RAP4 server at OUNL as an example, trusting that you will figure out what to do for servers of your own.

## Tasks

Here is an overview of all tasks described on this page. Refer to the related section below for details on each specific task

1. Connect\
   Before doing any maintenance, you need credentials to gain access to your server. Then you can connect to the server. We assume you gain access to a command-line interface (CLI) of your server. In this text that CLI is `/bin/bash` on a linux machine.
2. Check the configuration\
   When you start a maintenance session, you may want to check on the system. RAP4 runs in containers on a docker-platform, so you can check whether the containers are running and you can check the configuration in which they should be running.
3. Upgrade to a new version of RAP
4. Upgrade to a new version of Ubuntu
5. Refresh the configuration\
   Refreshing the RAP4 configuration is something you need to do only when developers tell you to do that.&#x20;

## Connecting to the RAP4 server

The RAP-server has been configured to communicate via `ssh`. The Open Universiteit allows ssh-connections only through VPN. So I made sure my VPN-connection is active.

I entered the server using command `ssh rap.cs.ou.nl` and the right user/password combination, upon which I gained access to the CLI.

The RAP4-instance is installed from directory `~\RAP4`, which is the working directory from which maintenance is done.

This is what you can expect to see:

```
stefjoosten$ ssh sjo@rap.cs.ou.nl
sjo@rap.cs.ou.nl's password: 
Permission denied, please try again.
sjo@rap.cs.ou.nl's password: 
Welcome to Ubuntu 16.04.3 LTS (GNU/Linux 4.4.0-124-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

170 packages can be updated.
12 updates are security updates.

New release '18.04.2 LTS' available.
Run 'do-release-upgrade' to upgrade to it.


Last login: Wed May 29 12:03:17 2019 from 145.20.142.195
sjo@lnx-hrl-202v:~$cd RAP4
sjo@lnx-hrl-202v:~/RAP4$
```

## Checking which containers are running

The server should show at least two containers, a database container called `rap4_db` and a RAP-container called `rap4`. There may be a third container called `phpmyadmin`, which is there to gain access to the database (for maintainers only). To verify, give the command `docker ps`. This is what you may expect to see:

```
sjo@lnx-hrl-202v:~/RAP4$ docker ps
CONTAINER ID        IMAGE                     COMMAND                  CREATED             STATUS              PORTS                            NAMES
40e5e97c1b26        phpmyadmin/phpmyadmin     "/run.sh superviso..."   4 months ago        Up 36 seconds       9000/tcp, 0.0.0.0:8080->80/tcp   rap3_phptools_1
bff782b2da89        ampersandtarski/rap4-db   "docker-entrypoint..."   4 months ago        Up 2 weeks          3306/tcp                         rap3_db_1
773f7ad86527        dockerampersand_rap3      "docker-php-entryp..."   18 months ago       Up 2 weeks          0.0.0.0:80->80/tcp               dockerampersand_rap3_1
7525eb7ea95f        dockerampersand_db        "docker-entrypoint..."   18 months ago       Up 2 weeks          3306/tcp                         dockerampersand_db_1
sjo@lnx-hrl-202v:~/RAP4$ 
```

In this case it appears that a fourth container is running. This poses no problem, because other processes may run concurrently without interfering with RAP4.

The configuration file that specifies this configuration is the only file in the working directory:

```
sjo@lnx-hrl-202v:~/RAP4$ ls -al
total 20
drwxrwxr-x  3 sjo  sjo  4096 Oct 25  2018 .
drwxr-xr-x 18 sjo  sjo  4096 Jan  9  2018 ..
-rw-rw-r--  1 sjo  sjo   472 Oct 25  2018 docker-compose.yml
drwxr-xr-x  5 root root 4096 Dec 15  2017 volumes
sjo@lnx-hrl-202v:~/RAP4$ 
```

By inspecting the contents you can see whether the configuration matches what you see in `docker`. This is what you can expect in `docker-compose.yml`:

```
sjo@lnx-hrl-202v:~/RAP4$ cat docker-compose.yml
version: '3'

services:
  rap4:
    restart: always
    image: ampersandtarski/ampersand-rap:latest
    ports:
      - "80:80"
    links:
      - db
    volumes:
      - ./volumes/log:/var/www/html/RAP4/log
      - ./volumes/scripts:/var/www/html/RAP4/scripts
 
  db:
    restart: always
    image: ampersandtarski/rap4-db
    volumes:
      - ./volumes/mysql:/var/lib/mysql

  phptools:
    image: phpmyadmin/phpmyadmin
    ports:
      - "8080:80"
    links:
      - db
sjo@lnx-hrl-202v:~/RAP4$
```

The directory `volumes` contains the Ampersand data, which is kept outside the containers so the data persists if containers get killed or if you restart the software.

```
sjo@lnx-hrl-202v:~/RAP4$ ls -al volumes/
total 40
drwxr-xr-x   5 root     root      4096 Dec 15  2017 .
drwxrwxr-x   3 sjo      sjo       4096 Oct 25  2018 ..
drwxr-xr-x   2 www-data www-data  4096 Dec 28 11:16 log
drwxr-xr-x 229      999      999 20480 May 13 07:48 mysql
drwxr-xr-x  75 www-data www-data  4096 Oct 25  2018 scripts
sjo@lnx-hrl-202v:~/RAP4$
```

There are three data sets. The directory `log` contains logging information of the RAP4-server. The directory `mysql` contains the data from MariaDB. The directory `scripts` contains the student scripts and the files they generated when working in RAP4.

## Upgrade to a new version of RAP

If you need to upgrade RAP4 to the latest release run this command:

```
sjo@lnx-hrl-202v:~/RAP4$ docker-compose up -d
```

As you can see in the configuration (`docker-compose.yml`)&#x20;

## Upgrade to a new version of Ubuntu

When you connect to the server, you get hints about the state your Ubuntu server is in. E.g.

```
% ssh sjo@rap.cs.ou.nl
Welcome to Ubuntu 18.04.5 LTS (GNU/Linux 4.15.0-151-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

  System information as of Wed Aug 11 10:26:22 CEST 2021

  System load:                    0.08
  Usage of /:                     68.3% of 48.83GB
  Memory usage:                   17%
  Swap usage:                     0%
  Processes:                      264
  Users logged in:                0
  IP address for ens160:          145.20.188.96
  IP address for br-0b19c7bfdba3: 172.20.0.1
  IP address for br-0f95c427718c: 172.26.0.1
  IP address for docker0:         172.17.0.1

 * Canonical Livepatch is enabled.
   - All available patches applied.

84 packages can be updated.
1 update is a security update.


*** System restart required ***
Last login: Mon Aug  2 11:14:56 2021 from 145.20.142.176
sjo@lnx-hrl-202v:~$
```

In such cases you can update by giving two commands:

```
sjo@lnx-hrl-202v:~$ sudo apt update
sjo@lnx-hrl-202v:~$ sudo apt upgrade
```

Sometimes a package is kept back because there is a problem with dependencies. You will have to upgrade such packages by hand.

## Refreshing the code

In the rare event that the configuration of RAP4 has changed (to be announced by the developers), you must update the file `docker-compose.yml`by hand, using the command:

```
wget https://raw.githubusercontent.com/AmpersandTarski/RAP/master/docker-compose.yml
```



