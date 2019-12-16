FROM haskell:8.6.5 AS buildstage
# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
# The Haskell version number must be consistent with ./stack.yaml to ensure successful compilation.

# set up Haskell stack; downloads approx 177MB
# Don't worry about the correct version of ghc. It is specified in stack.yaml
# RUN stack setup

# See .dockerignore for files/folders that are not copied
COPY . /code

WORKDIR /code

# Build Ampersand compiler and install in /root/.local/bin
RUN stack install

# Show the results of the build stage
RUN ls -al /root/.local/bin

# Create a light-weight image that has the Ampersand compiler available
FROM ubuntu

COPY --from=buildstage /root/.local/bin/ampersand /bin/

ENTRYPOINT ["/bin/ampersand"]
