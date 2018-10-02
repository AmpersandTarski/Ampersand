FROM ampersandtarski/ampersand-prototype:latest

ARG ADL_FILE
ENV ADL_FILE=${ADL_FILE:-Script.adl}

ENV AMPERSAND_DB_HOST=db

RUN mkdir /src

ADD . /src

# Generate prototype application from folder
RUN ampersand --proto=/var/www/html /src/$ADL_FILE --verbose \
 && mkdir -p /var/www/html/log \
 && chown -R www-data:www-data /var/www/html \
 ## replace database host config in localsettings so application can talk with docker database
 && sed -i "s|.*Config::set.*dbHost.*|Config::set('dbHost', 'mysqlDatabase', getenv('AMPERSAND_DB_HOST'));|" /var/www/html/localSettings.php
