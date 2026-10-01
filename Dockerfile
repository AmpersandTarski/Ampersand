# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.

# Normalize the version line of package.yaml before it enters the dependency layer.
# A release bumps `version:`, which would change the COPY checksum and needlessly
# invalidate the dependency layer below (231 packages, ~24 min), even though the
# dependency set did not change (issue #1664). The dependency layer must key on the
# dependency set, not on the package version. `COPY --from=manifest` keys on the
# content of the normalized file, so a version-only bump keeps the cache warm while
# a change to the dependency list, resolver, or lock still invalidates it.
FROM debian:bookworm-slim AS manifest
COPY package.yaml /manifest/package.yaml
RUN sed -i 's/^version:.*/version: 0.0.0/' /manifest/package.yaml

# The build stage runs on Debian 12 (bookworm), which receives security updates until 30 June 2028 (LTS).
# It used to run on haskell:9.6.6, which is based on Debian 11. After Debian 11's security support ended
# on 31 August 2026, its security source kept listing packages whose files were gone, so apt-get failed
# with 404 errors and no image could be built. No official Haskell image offers GHC 9.6 on a newer Debian,
# so Stack installs GHC itself (see the dependency layer below).
# Why Debian 12 and not 13: a binary only runs where the glibc is at least as new as the one it was built
# against. Debian 12 has glibc 2.36, so the binary runs in the runtime image below (Ubuntu 24.04, glibc 2.39)
# and in the prototype framework image (php:8.3-apache-bookworm, glibc 2.36). A build on Debian 13
# (glibc 2.41) would run in neither. Move this stage forward only together with, or after, those images.
FROM debian:bookworm-slim AS buildstage

RUN mkdir /opt/ampersand
WORKDIR /opt/ampersand

# The C toolchain and libraries that GHC and the Haskell dependencies link against.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    autoconf \
    automake \
    build-essential \
    ca-certificates \
    curl \
    git \
    libbz2-dev \
    libexpat1-dev \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    pkg-config \
    xz-utils \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Stack at a pinned version, for the architecture of this build (x86_64 or aarch64).
# pipefail makes a failed download fail the build, instead of handing tar an empty stream.
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG STACK_VERSION=3.1.1
RUN arch="$(uname -m)" && \
    curl -fsSL "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-${arch}.tar.gz" \
    | tar -xz --strip-components=1 -C /usr/local/bin "stack-${STACK_VERSION}-linux-${arch}/stack" && \
    stack --version

# Start with a docker-layer that contains build dependencies, to maximize the reuse of these dependencies by docker's cache mechanism.
# Only updates to stack.yaml, stack.yaml.lock or package.yaml (beyond its version line) rebuild this layer;
# all other changes use the cache. package.yaml comes from the manifest stage above, so a version-only bump
# (every release) keeps this layer cached.
# stack.yaml.lock is included on purpose: it pins the exact dependency set, so a lock-only change must invalidate
# this layer (see .dockerignore, which re-includes it despite the general *.lock exclusion).
# This layer also installs GHC, at the version that the resolver in stack.yaml prescribes (GHC 9.6.6 for lts-22.39).
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