---
description: >-
  Deploying your Ampersand application has been automated, so you can deploy
  fast and deploy anywhere. Read this section for the quick "how-to's",
  examples, and the bigger picture behind it.
---

# Ampersand and Docker

Different aspects of the Ampersand user's environment can be containerized 1. Ampersand compiler 1. Modelling environment for Ampersand user \(incl. VSCode extensions\) 1. Prototype environment, including:

* Apache webserver with PHP
* Ampersand compiler
* PHP-composer
* NPM \(Node Package Manager\)
* Gulp
  1. Multi-stage prototype build, only:
* Apache webserver with PHP
* Generated code
  1. Prototype database
* MariaDB with default user/password

A docker-compose file is available to deploy prototype, database and supporting tools \(like phpMyAdmin\)

## Deploy your local environment to run Ampersand prototypes

Detailed technical instructions are at [https://github.com/AmpersandTarski/RAP](https://github.com/AmpersandTarski/RAP) under the directory listing. This page provides more general and summarized information as context.

The [project-template repository](https://github.com/AmpersandTarski/project-template) contains a docker-compose and Dockerfile to generate, build and run a prototype application.

Run: `docker-compose up -d` to deploy the following services:

* Apache webserver
  * Serves your prototypes
  * Available in the browser at `http://localhost:80`
* phpMyAdmin dashboard
  * Available in the browser at `http://localhost:8080`
* MariaDB database
  * Not directly exposed on host

## Install instructions for Windows

* Install Docker from [https://docs.docker.com/install/](https://docs.docker.com/install/) and start the desktop app
* Go to [https://github.com/AmpersandTarski/RAP](https://github.com/AmpersandTarski/RAP)
* Click on the green “Code” button and pull down to “Download ZIP”
* Download the zip and copy the directory on your laptop 
* With a command line interface:
  * docker-compose up –d
  * you may need to enter “docker network create proxy” and repeat “docker-compose up –d”. The second time is a lot faster
* Go to the Docker desktop app
  * Find rap-master or the name you gave this docker app
  * Click the play button \(the triangle\)
* Go to “localhost” on your browser and RAP should be running there as it does on the course server
  * If that doesn’t work then to back to docker and stop and restart rap4 but enter “2020” as the port and go on the browser to “localhost:2020”

RAP's GitHub site has more technical install instructions at [https://github.com/AmpersandTarski/RAP/blob/master/README.md](https://github.com/AmpersandTarski/RAP/blob/master/README.md)

