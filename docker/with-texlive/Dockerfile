# !!!!!!!!!!!!!!
# This Dockerfile is not yet working or tested
# Contains old setup. Rethinking how to effectively and efficiently textlive can be used by Ampersand compiler
# !!!!!!!!!!!!!
FROM docker.pkg.github.com/ampersandtarski/ampersand/ampersand

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    curl \
    netbase \
    wget \
    zlib1g-dev \
    graphviz \
    ghostscript

# install texlive from network
ENV PATH /texlive/bin/x86_64-linux:$PATH
ADD texlive.profile /tmp
RUN cd ~ \
 && curl http://ctan.mirrors.hoobly.com/systems/texlive/tlnet/install-tl-unx.tar.gz | tar -vxz \
 && cd install-tl* \
 && ./install-tl -profile /tmp/texlive.profile -repository http://ftp.uni-erlangen.de/mirrors/CTAN/systems/texlive/tlnet/ \
 && cd .. \
 && rm -rf install-tl* \
 && rm /tmp/texlive.profile
