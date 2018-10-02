# This Dockerfile can be used to generate prototype applications from Ampersand scripts.
# To generate functional specification documents use Dockerfile.fspec
FROM php:7-apache

ENV AMP_DEPS \
             curl \
             git \
             less \
             netbase \
             vim \
             wget \
             zlib1g-dev

# install required php packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    $AMP_DEPS \
 && docker-php-ext-install mysqli \
 && a2enmod rewrite \
 # enable ZipArchive for importing .xlsx files on runtime
 && docker-php-ext-install zip \
 # install composer (php's package manager)
 && php  -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
 && php composer-setup.php --install-dir=/usr/local/bin --filename=composer \
 && php -r "unlink('composer-setup.php');" \
 && rm -rf /var/lib/apt/lists/*

ENV COMPOSER_HOME /usr/local/bin/

# curl -s -L is to silently download the latest release HTML (after following redirect)
# egrep -o '...' uses regex to find the file you want
RUN curl -s -L $(curl -s -L https://api.github.com/repos/AmpersandTarski/Ampersand/releases/latest | egrep -o 'https://github.com/AmpersandTarski/Ampersand/releases/download/.+/ampersand' | uniq) > /usr/local/bin/ampersand \
 && chmod +x /usr/local/bin/ampersand

# TODO: make the log subdirectory writable

