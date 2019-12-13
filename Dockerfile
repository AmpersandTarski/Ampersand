FROM haskell:8.6.5 AS buildstage
# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
# The Haskell version number must be consistent with ./stack.yaml to ensure successful compilation.

# set up Haskell stack; downloads approx 177MB
# Don't worry about the correct version of ghc. It is specified in stack.yaml
# RUN stack setup

COPY AmpersandData app src testing ampersand.cabal package.yaml Setup.hs stack.yaml /code/

WORKDIR /code

# installs Ampersand executables in /root/.local/bin
RUN stack install

# show the results of the build stage
RUN ls -al /root/.local/bin

# Purpose: a light-weight container can copy ampersand executables from /root/.local/bin, ignoring the build-stuff such as source code and setup-work
FROM ubuntu

VOLUME ["/scripts"]

COPY --from=buildstage /root/.local/bin/ampersand /bin/

WORKDIR /scripts

ENTRYPOINT ["/bin/ampersand"]