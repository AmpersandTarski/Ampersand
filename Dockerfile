# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
FROM haskell:8.6.5 AS buildstage

RUN mkdir /code
WORKDIR /code

# Start with a layer to build dependencies that can be cached
# Only updates to these files must trigger this layer
# https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
COPY stack.yaml package.yaml stack.yaml.lock /code/
RUN stack build --dependencies-only

# Copy the rest of the application
# See .dockerignore for files/folders that are not copied
COPY . /code

# Build Ampersand compiler and install in /root/.local/bin
RUN stack install

# Show the results of the build stage
RUN ls -al /root/.local/bin

# Create a light-weight image that has the Ampersand compiler available
FROM ubuntu

COPY --from=buildstage /root/.local/bin/ampersand /bin/

ENTRYPOINT ["/bin/ampersand"]
