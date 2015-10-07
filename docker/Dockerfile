FROM debian:wheezy
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

ADD sources.list /etc/apt/sources.list
ADD esl-erlang_17.1-1~debian~wheezy_amd64.deb /root/esl-erlang_17.1-1~debian~wheezy_amd64.deb

RUN apt-get update && \
	apt-get -y install wget apt-utils build-essential devscripts ca-certificates zip snmp-mibs-downloader
RUN ( dpkg -i /root/esl-erlang_17.1-1~debian~wheezy_amd64.deb || true ) && \
	apt-get -f -y install
