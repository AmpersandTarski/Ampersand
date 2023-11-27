# Deploying OUNL RAP4

If you want to deploy RAP4 on your own server, you might try to copy how I deployed RAP4 at the OUNL. This chapter tells you how.

In early 2016 the need arose to replace the RAP2 implementation of Ampersand by a RAP3 implementation, because RAP2 was insufficiently maintainable. This environment is used by students for completing the course Rule Based Design (OBR, code IM0103). This implementation is hosted by ICTS, the IT-department of the university.

This chapter is an account of the installation process. It serves the following purposes:

1. It is an example for others who want to deploy Ampersand.\
   We get requests now and then by people who want to deploy Ampersand, so we figured it is nice to have a documented example for them.
2. It documents the installation we made for the Open University.\
   We want maintenance of RAP4 to be transferable to other persons, so we need to document the choices made and the reasons for making them.
3. It contains all information needed to make a deployment script for automated deployment.\
   We have automated the deployment with Docker, so that RAP4 will always be up to date with the most recent stable release of Ampersand.

Each step in the installation process gets a separate section in this text.

## 1. Setting up the virtual machine

I got a server from the Open University's IT-department.

The following settings apply:

|                                         |                                                                                                       |
| --------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| server name                             | lnx-hrl-202v                                                                                          |
| OS                                      | Ubuntu 18.04.5 LTS (GNU/Linux 4.15.0-153-generic x86\_64                                              |
| Admin user name                         | sjo                                                                                                   |
| verification type                       | password (Stef Joosten knows the password)                                                            |
| Size                                    | 2 core, 7GB                                                                                           |
| Inbound port: RAP4 (HTTP)               | TCP/80                                                                                                |
| Inbound port: HTTPS                     | TCP/443                                                                                               |
| Inbound port: SSH                       | TCP/22                                                                                                |
| Public IP-adres                         | 145.20.188.96                                                                                         |
| domain name for calling the application | [rap.cs.ou.nl](https://rap.cs.ou.nl)                                                                  |
| internet access needed                  | for generating prototypes because the prototype generator draws from the Ampersand-github repository. |

## Getting access to the server

At the OUNL, we need VPN to gain access with SSH to a server. This requires approval from the IT department. I got a raw Ubuntu machine, meaning that the port settings (specified above) and VPN have to be requested at the IT-servicedesk.

I can now access this machine through SSH (using PUTTY, which I downloaded from the Internet), but only after installing a VPN-tunnel to the server (using Pulse Secure). In the sequel, I will refer to this machine as "the server". This gave me access through a command line interface (CLI). Ubuntu gave me bash as its CLI.

## Installing Docker

Since this is a fresh machine, docker has to be installed. By just typing `docker`, the server advised to install Docker by means of the command `sudo apt install docker.io`. This turned out to be a bad advice, because it resulted in a too old version of docker. Instead, I followed the instructions on `https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/` for installing docker. The instructions for docker-compose are found on `https://docs.docker.com/compose/install/` .

Then I checked that everything went successfully by means of the `which`-command:

```
sjo@lnx-hrl-202v:~$ which docker
/usr/bin/docker
sjo@lnx-hrl-202v:~$ which docker-compose
/usr/bin/docker-compose
```

## Obtaining the files we need

We need only one file: `docker-compose.yml`\
To get it, I used the `wget` command, which gets stuff from the web:

```
sjo@lnx-hrl-202v:~$ mkdir RAP4
sjo@lnx-hrl-202v:~$ cd RAP4
sjo@lnx-hrl-202v:~/RAP4$ wget https://raw.githubusercontent.com/AmpersandTarski/RAP/master/docker-compose.yml
```

## Installing RAP4

To install RAP4:

```
sjo@lnx-hrl-202v:~/RAP4$ docker compose up -d
```

To check whether this worked, I went to my browser and navigated to `http://145.20.188.96/RAP4`.\
It took a while to get started, because it was building a fresh database.

I checked whether the containers are running by means of the `docker ps` command.

Completion of this step allowed access to RAP4 from an arbitrary computer on the internet:

![](<../assets/import.png>)

The database is accessible on port 8080:

![](<../assets/phpMyAdmin.png>)
