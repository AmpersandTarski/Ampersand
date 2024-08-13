---
description: >-
  To use the Ampersand compiler from the command line without installing
  Ampersand, you can call on Docker. This page gives you the quick "how-to's"
  for this purpose.
---

# Compiler

To run the Ampersand compiler from any computer that runs docker, use a Docker image from our Ampersand repository on Docker hub, and make a shortcut tag to save typing:

```bash
docker pull ampersandtarski/ampersand
docker tag ampersandtarski/ampersand ampersand
```

Different tags are available, e.g.:

* :latest -> development branch\
  It contains the most recent developments that have passed the automated test sets.
* :stable -> master branche\
  This contains the latest stable release of ampersand. Use it for the lowest risk.
* :\[branch] -> specific branch\
  This allows you to pick a version yourself
* :v3.17 -> specific tags/releases\
  To use a specific previous version.

```bash
docker run -it -v "$(pwd)":/usr/local/project ampersand [OPTIONS] FILE
```

This alternative presumes you have `docker` installed on your computer and it can be found (i.e. the path is set correctly).

If you experience problems downloading from `docker.pkg.github.com`, you may first have to log in with an appropriate token. Generate a token [on github](https://github.com/settings/tokens) and be sure to switch on all repo-rights and the `read:packages`-right. With that token log in:

```
docker login -u <GITHUB USERNAME> -p <TOKEN> docker.pkg.github.com
```
