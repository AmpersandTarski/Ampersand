---
description: >-
  To build your own Ampersand compiler is something to avoid as a user. As a
  developer, however, you may have reasons to do this yourself. For instance to
  verify what happens in older versions.
---

# Building an Ampersand Compiler with Stack

The Ampersand compiler is a Haskell program built with [stack](https://haskellstack.org/). Stack is a build tool for Haskell projects such as Ampersand. We have automated the building process \(using stack\) for the following purposes:

1. to prevent mistakes such as dependency conflicts inside and between Haskell packages, for an uninterrupted  compilation process \(robust building\);
2. to generate ampersand compilers for different platforms \(platform independence\);
3. to provide a reproducible and reliable build process to developers with diverse development tools, operating systems, and working environments \(uniform building\); 
4. to allow for generating images for docker containers \(containerization\);
5. to accellerate the build process to increase the release frequency of Ampersand.

## Installation

[Haskell](https://www.haskell.org/) comes as part of [Stack](http://haskellstack.org), so there is no need to install Haskell separately.

The [instructions to install Stack](http://haskellstack.org) are pretty clear for the various platforms. Make sure you read the part about the STACK\_ROOT environment variable.

To compile Ampersand you need a file [package.yaml](https://github.com/AmpersandTarski/Ampersand/blob/development/package.yaml), which sits in the Ampersand repository. Fetch it and put it in you working directory. From the command-line, call command `stack install` and after a while \(go get coffee!\) your ampersand compiler exists! NB: If you want to build Rieks'  preprocessor as well, the magic spell is `stack install --flag ampersand:buildAll`

