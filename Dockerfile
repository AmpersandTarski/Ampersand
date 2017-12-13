FROM ampersandtarski/haskell:latest

# clone git ampersand compiler repository
RUN mkdir ~/git \
 && cd ~/git \
 && git clone --depth=1 --branch master https://github.com/AmpersandTarski/Ampersand \
 && cd ~/git/Ampersand \
 && stack install --local-bin-path /usr/local/bin \
 && rm -rf ~/.stack ~/git/Ampersand/.stack-work
