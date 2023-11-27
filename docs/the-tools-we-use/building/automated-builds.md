---
description: This page explains how Ampersand images are built automatically
---

# Automated builds

We followed [docker's instruction for automated builds](https://docs.docker.com/docker-hub/builds/), to automate the workflow from committing to the [Ampersand repository on Github](https://github.com/AmpersandTarski/Ampersand) to the corresponding docker image on [Docker hub](https://hub.docker.com/r/ampersandtarski/ampersand).

## Manual builds

If you must do things on your own, you might want to reproduce parts of the builds.

The Dockerfile for the Ampersand compiler resides in the root of the Ampersand repository. So from the command line, if you are at the root of the Ampersand repository, you can call `docker build .` to create the image. See [this page](building-an-ampersand-compiler-as-docker-image.md) for more details.

The package.yaml file for the Ampersand compiler resides in the root of the Ampersand repository. So from the command line, if you are at the root of the Ampersand repository, you can call `stack install` to create the image. See [this page](haskell.md) for more details.

