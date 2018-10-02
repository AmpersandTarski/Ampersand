#!/usr/bin/env sh
#
# Build docker images for:
#   1. a container with just the ampersand compiler;
#   2. a container with latex, graphviz and other modules for running RAP;
#   3. a container with MariaDB
# Run this build script from the repository's root:
#   ampersandadmin@hostname:~$  cd RAP 
#   ampersandadmin@hostname:~$  ./prototype/build.sh 

set -e

echo "Building ampersandtarski/ampersand-prototype:latest"
docker build --tag ampersandtarski/ampersand-prototype:latest docker

echo "Building ampersandtarski/ampersand-prototype:texlive"
docker build --tag ampersandtarski/ampersand-prototype:texlive docker/texlive

echo "Building ampersandtarski/ampersand-prototype-db"
docker build --tag ampersandtarski/ampersand-prototype-db:latest docker/prototype-db
