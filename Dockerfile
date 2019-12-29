# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
FROM haskell:8.6.5 AS buildstage

RUN mkdir /opt/ampersand
WORKDIR /opt/ampersand

# Start with a layer to build dependencies that can be cached
# Only updates to these files must trigger this layer
# https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
COPY stack.yaml package.yaml /opt/ampersand/
RUN stack build --dependencies-only

# Copy the rest of the application
# See .dockerignore for files/folders that are not copied
COPY . /opt/ampersand

# These ARGs are available as ENVs in next RUN and are needed for compiling the Ampersand compiler to have the right versioning info
ARG GIT_SHA
ARG GIT_Branch

# Build Ampersand compiler and install in /root/.local/bin
RUN stack install

# Show the results of the build stage
RUN ls -al /root/.local/bin

# Create a light-weight image that has the Ampersand compiler available
FROM ubuntu

COPY --from=buildstage /root/.local/bin/ampersand /bin/

ENTRYPOINT ["/bin/ampersand"]
