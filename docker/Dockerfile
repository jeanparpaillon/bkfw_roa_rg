FROM debian:bullseye
LABEL author="Jean Parpaillon <jean.parpaillon@free.fr>"

ENV DEBIAN_FRONTEND noninteractive

#COPY esl-erlang_17.5.3-1~debian~jessie_amd64.deb .

RUN apt update && \
    apt -y --force-yes install gawk wget git-core diffstat unzip texinfo gcc-multilib \
    build-essential chrpath socat autoconf automake libtool libglib2.0-dev \
    libarchive-dev cpio sudo

#RUN dpkg -i ./esl-erlang_17.5.3-1~debian~jessie_amd64.deb || true
#RUN apt install -f -y

RUN groupadd -g 1000 jean && \
    useradd -m -s /bin/bash -u 1000 -g 1000 jean

USER jean
WORKDIR /home/jean

# ENTRYPOINT ["/bin/bash"]
