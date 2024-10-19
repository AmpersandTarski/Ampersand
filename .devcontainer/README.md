# On the use of the ampersand .devcontainer stuff

## Purpose

The purpose of the .devcontainer is to provide a common development environment for the Haskellers among the developers of Ampersand.

## What is in this directory

`DockerfileUpstream` is a Dockerfile to build an image. That image should be [available at dockerhub](https://hub.docker.com/repository/docker/ampersandtarski/ampersand-devcontainer/general).
`devcontainer.json` contains the information to properly launch a remote container for the developer.

## Usage

To use this devcontainer, simply open vscode in the Ampersand workspace. It should ask to reopen in a container. The first time might take quite a while, but it is worth the wait. All Haskell goodies will be at your fingertips.

## Maintenance of the upstream image

NB: This action is currently done by Han, no need for other people to do so. It can be seen as regular maintenance of the image

Sometimes there are updates of the Haskell toolchain we use. For instance whenever a new version of the Haskell Language Server is made available, the `DockerfileUpstream` should be updated accordingly. Then, the new image should be built and published at dockerhub. To do so, go to the ampersand root directory and run the following commands:

~~~docker
docker build -f .devcontainer/DockerfileUpstream -t ampersandtarski/ampersand-devcontainer:<tag> .
~~~

for example:
~~~docker
docker build -f .devcontainer/DockerfileUpstream -t ampersandtarski/ampersand-devcontainer:ghc9-6-6 .
~~~

Then, it is time to **test** it. Rebuild your environment locally, using the `Rebuild container` command of the `Dev containers` extention in vscode. 

As long as there are errors, don't push the image to docker hub. Instead, fix the problems.

Repeat the building until you (and the system) are happy with the result. Then it is time to push the new image to dockerhub. That is done by using the command:

~~~
docker push ampersandtarski/ampersand-devcontainer:<tag>
~~~
where `<tag>` must be replaced with an appropriate new tagname reflecting the version of ghc. Use the same tag as you used to build it:
~~~
docker push ampersandtarski/ampersand-devcontainer:ghc9-6-6
~~~

