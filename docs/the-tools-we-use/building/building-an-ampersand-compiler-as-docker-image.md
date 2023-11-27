---
description: >-
  If you need an Ampersand compiler in a Docker image, use the one on Docker
  hub. It sits there ready for you to use. However, if you want to know how it
  is baked, carry on reading.
---

# Baking a Docker image that contains the Ampersand Compiler

## The magic

To get a docker container I took the file [Dockerfile](https://github.com/AmpersandTarski/Ampersand/blob/feature/dockerize/docker/Dockerfile) and built an image called "myampersand", using the following command:

```text
docker build -t myampersand ~/Ampersand/
```

It took 45 minutes to build, but I got an image called `myampersand` in the docker repository on my laptop.

To see it work, I executed the newly created image on a little file in my working directory, `hello.adl` from the command line with the following command:

```text
docker run -it -v "$(pwd)":/scripts myampersand hello.adl
```

The same command in the Windows command line is:

```text
docker run -it -v "%cd%":/scripts myampersand hello.adl
```

## Do you want to reproduce this?

The Ampersand repository contains a file, [Dockerfile](https://github.com/AmpersandTarski/Ampersand/blob/development/Dockerfile), that contains a recipe for building an Ampersand compiler and put it in your Ampersand repository. You need the following ingredients to run it:

1. A machine to run docker, for building your docker image with. I ran it on my MacBook.
2. Docker, which you need [installed on your machine](https://docs.docker.com/install/).
3. Docker needs least 5G of memory to build Ampersand, which is more than the standard 2G. I used [this instruction](https://stackoverflow.com/questions/44533319/how-to-assign-more-memory-to-docker-container/44533437#44533437) to increase Docker's memory on my Macbook.

To run it, I cloned the Ampersand repository, into `~/Ampersand/`, and built the image with:

```text
docker build -t myampersand ~/Ampersand/
```

It runs on my Mac for over half an hour, so some patience is required. If you don't have that patience, consider using the image [ampersandtarski/ampersand](https://hub.docker.com/r/ampersandtarski/ampersand) from docker hub. It was built for your convenience.

The resulting docker image sits in the docker repository on you laptop \(placed there when docker was installed\).

## So which steps does Docker take?

If you want a slightly different image \(for reasons of your own\), you may want to repeat this process yourself. For that purpose, let us walk through the different steps described in Dockerfile.

Let us discuss the steps one-by-one. \(Please check the [Dockerfile](https://github.com/AmpersandTarski/Ampersand/blob/feature/dockerize/docker/Dockerfile), just in case it is inconsistent with this documentation.\) All of these steps happen automatically, as they are in the docker file.

The first statement states that the compiler is built on a well-defined Haskell image.

```text
FROM haskell:8.6.5 AS buildstage
```

This building stage is called buildstage because we want to use a [2-stage build](https://docs.docker.com/develop/develop-images/multistage-build/) to obtain a small Ampersand image without excess-software.

We decide to work in the build-container from a working directory called `/Ampersand`.

```text
WORKDIR /Ampersand/
```

Normally we want to generate Ampersand from the source code on GitHub. For this purpose we clone the Ampersand-repository into the \(working directory in the\) build environment.

```text
RUN git clone https://github.com/AmpersandTarski/Ampersand/ .
```

In this case I wanted to build from a specific feature, so I checked it out.

```text
RUN git checkout feature/Archimate3
```

Now everything is in place to compile Ampersand. Running `stack install` results in a full-fledged Ampersand compiler  in `/root/.local/bin`. Mind you, this takes a while...

```text
RUN stack install
```

If we were to stop here, we get an image larger than 4GB. We can do better than that by starting over with a clean machine. So we introduce a second `FROM`-line in the Dockerfile which starts with a clean slate. We use an empty ubuntu machine \(form some reason yet unknown, the smaller alpine image doesn't work\)

```text
FROM ubuntu:16.04
```

Now we must copy the ampersand executable to `/bin`, from where we can run it as though it were a normal ubuntu-command. It is the only software we will copy into this image. \(Haskell and the intermediate files are all absent\). This results in an image that is slightly over 220MB.

```text
COPY --from=buildstage /root/.local/bin/ampersand /bin/
```

When compiling, we  will work in a directory called scripts. When using this container, we will volume-map this directory to the actual working directory that contains the ampersand-files we want to compile.

```text
WORKDIR /scripts
VOLUME ["/scripts"]
```

The program to be called when running the container is of course `ampersand` \(residing in `/bin/`\). If called without arguments it will use `--verbose`.

```text
ENTRYPOINT ["/bin/ampersand"]
CMD ["--verbose"]
```

