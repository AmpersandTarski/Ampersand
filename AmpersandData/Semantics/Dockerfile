# This script is meant to build from the root directory of your RAP-repo.
# Ampersand compiler is also needed for RAP4 to compile student scripts

FROM ampersandtarski/prototype-framework:v1.8.5

# Install docker, so students can run their prototypes
RUN apt-get update \
 && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable"
# We only need the docker-cli because we connect to the docker deamon on the host machine using a volume bind to docker.sock
RUN apt-get update \
 && apt-get install -y docker-ce-cli

# We use graphviz to create drawings in diagnosis and functional specifications.
# and we need epstopdf to create pdf files for images
RUN apt-get update && apt-get install -y graphviz

# Copy Ampersand compiler
COPY --from=ampersandtarski/ampersand:local /bin/ampersand /usr/local/bin
RUN chmod +x /usr/local/bin/ampersand

COPY . /usr/local/project/

# Generate prototype application from folder
RUN ampersand proto /usr/local/project/Interfaces.adl \
  --proto-dir /var/www \
  --build-recipe Prototype \
  --verbose

# Copy customizations into generated application
COPY customizations /var/www/

RUN chown -R www-data:www-data /var/www/data /var/www/log /var/www/generics \
  && cd /var/www \
  && composer install --prefer-dist --no-dev --profile --optimize-autoloader \
  && npm update \
  && npm audit fix \
  && npm install \
  && gulp build-ampersand \
  && gulp build-project
