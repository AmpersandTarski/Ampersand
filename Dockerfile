FROM haskell:8.6.5 AS buildstage
# The purpose of this docker file is to produce a latest Ampersand-compiler in the form of a docker image.
# Purpose: a light-weight container can copy ampersand executables from /root/.local/bin, ignoring the build-stuff such as source code and setup-work

# The Haskell version number must be consistent with ./stack.yaml to ensure successful compilation.

# build from the Ampersand source code directory
WORKDIR /Ampersand/

# clone the ampersand source files ('git clone' requires the directory to be empty)
RUN git clone https://github.com/AmpersandTarski/Ampersand/ .

# get Ampersand sources in the desired version
RUN git checkout feature/Archimate3

# Or alternatively, just copy your Ampersand working directory (i.e. your own clone of Ampersand) into the build
# COPY . .

# set up Haskell stack; downloads approx 177MB
# Don't worry about the correct version of ghc. It is specified in stack.yaml
# RUN stack setup

# installs Ampersand executables in /root/.local/bin
RUN stack install

# show the results of the build stage
RUN ls -al /root/.local/bin

FROM ubuntu

VOLUME ["/scripts"]

COPY --from=buildstage /root/.local/bin/ampersand /bin/

WORKDIR /scripts

ENTRYPOINT ["/bin/ampersand"]

CMD ["--verbose"]