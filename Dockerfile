# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.

# Normalize the version line of package.yaml before it enters the dependency layer.
# A release bumps `version:`, which would change the COPY checksum and needlessly
# invalidate the dependency layer below (231 packages, ~24 min), even though the
# dependency set did not change (issue #1664). The dependency layer must key on the
# dependency set, not on the package version. `COPY --from=manifest` keys on the
# content of the normalized file, so a version-only bump keeps the cache warm while
# a change to the dependency list, resolver, or lock still invalidates it.
FROM haskell:9.6.6 AS manifest
COPY package.yaml /manifest/package.yaml
RUN sed -i 's/^version:.*/version: 0.0.0/' /manifest/package.yaml

FROM haskell:9.6.6 AS buildstage

RUN mkdir /opt/ampersand
WORKDIR /opt/ampersand

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
    apt-transport-https \
    autoconf \
    automake \
    build-essential \
    curl \
    gcc \
    git \
    gnupg2 \
    graphviz \
    libbz2-dev \
    libexpat1-dev \
    libgmp-dev \
    libgmp10 \
    libnuma-dev \
    lsb-release \
    openssh-server \
    pkg-config \
    software-properties-common \
    wget \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
    
# Start with a docker-layer that contains build dependencies, to maximize the reuse of these dependencies by docker's cache mechanism.
# Only updates to stack.yaml, stack.yaml.lock or package.yaml (beyond its version line) rebuild this layer;
# all other changes use the cache. package.yaml comes from the manifest stage above, so a version-only bump
# (every release) keeps this layer cached.
# stack.yaml.lock is included on purpose: it pins the exact dependency set, so a lock-only change must invalidate
# this layer (see .dockerignore, which re-includes it despite the general *.lock exclusion).
# Expect stack to give warnings in this step, which you can ignore.
# Idea taken from https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b
COPY stack.yaml stack.yaml.lock /opt/ampersand/
COPY --from=manifest /manifest/package.yaml /opt/ampersand/
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
FROM ubuntu:24.04

RUN apt-get update && apt-get install -y --no-install-recommends graphviz && rm -rf /var/lib/apt/lists/*

VOLUME ["/scripts"]
WORKDIR /scripts

# Copy the Ampersand binary from the build stage to /bin.
# Note! Other images (i.e. prototype framework) use this image and depend on the binary to be in this location
COPY --from=buildstage /root/.local/bin/ampersand /bin/ampersand

ENTRYPOINT ["/bin/ampersand"]
CMD ["--verbose"]