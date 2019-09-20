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

FROM php:7-apache AS configstage

ENV AMP_DEPS \
             zip \
             libzip-dev \
             graphviz \
             texlive-font-utils \
             ghostscript

# install required php packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    $AMP_DEPS

RUN docker-php-ext-install mysqli \
 && a2enmod rewrite \
# enable ZipArchive for importing .xlsx files on runtime
 && docker-php-ext-install zip

# install composer (php's package manager)
RUN php  -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
 && php composer-setup.php --install-dir=/usr/local/bin --filename=composer \
 && php -r "unlink('composer-setup.php');" \
 && rm -rf /var/lib/apt/lists/*

ENV COMPOSER_HOME /usr/local/bin/

# assume that the docker container runs in the working directory where the .adl-file(s) resides (e.g. foo.adl).
# Use the image to run Ampersand from the command line as follows (command for bash):
#     docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand [OPTIONS] foo.adl
VOLUME ["/scripts"]

WORKDIR /scripts

# install ampersand by copying it from the build stage
COPY --from=buildstage /root/.local/bin/ampersand /bin/
COPY --from=buildstage /Ampersand/.stack-work/install/x86_64-linux/lts-14.1/8.6.5/share/x86_64-linux-ghc-8.6.5/pandoc-2.7.3/data/docx/ /Ampersand/.stack-work/install/x86_64-linux/lts-14.1/8.6.5/share/x86_64-linux-ghc-8.6.5/pandoc-2.7.3/data/docx/

ENTRYPOINT ["/bin/ampersand"]

CMD ["--verbose"]