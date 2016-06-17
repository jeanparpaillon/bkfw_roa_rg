FROM debian:jessie
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

RUN apt update && \
    apt -y --force-yes install gawk wget git-core diffstat unzip texinfo gcc-multilib \
    build-essential chrpath socat autoconf automake libtool libglib2.0-dev \
    libarchive-dev cpio sudo
RUN groupadd -g 1000 jean && \
    useradd -m -s /bin/bash -u 1000 -g 1000 jean

USER jean
WORKDIR /home/jean

ENTRYPOINT ["/bin/bash"]
