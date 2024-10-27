# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
FROM haskell:9.6.6 AS buildstage

RUN mkdir /opt/ampersand
WORKDIR /opt/ampersand

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git pkg-config bzip2
    
# Start with a docker-layer that contains build dependencies, to maximize the reuse of these dependencies by docker's cache mechanism.
# Only updates to the files stack.yaml package.yaml will rebuild this layer; all other changes use the cache.
# Expect stack to give warnings in this step, which you can ignore.
# Idea taken from https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
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

# Display the resulting Ampersand version and SHA
RUN /root/.local/bin/ampersand --version

# Create a light-weight image that has the Ampersand compiler available
# to run ampersand from the command line.
# call with docker run -it  \       # run interactively on your CLI
#            --name devtest \       # name of the container (so you can remove it with `docker rm devtest`)
#            -v ${pwd}:/scripts  \       # mount the current working directory of your CLI on the container directory /scripts
#            <your subcommand>      # e.g. check, documentation, proto
FROM ubuntu

RUN apt-get update && apt-get install -y graphviz

VOLUME ["/scripts"]
WORKDIR /scripts

# Copy the Ampersand binary from the build stage to /bin.
# Note! Other images (i.e. prototype framework) use this image and depend on the binary to be in this location
COPY --from=buildstage /root/.local/bin/ampersand /bin/ampersand

ENTRYPOINT ["/bin/ampersand"]
CMD ["--verbose"]