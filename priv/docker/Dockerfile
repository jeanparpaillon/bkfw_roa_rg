FROM debian:wheezy
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

ADD sources.list /etc/apt/sources.list

RUN apt-get update && \
	apt-get -y install wget apt-utils build-essential devscripts ca-certificates zip snmp-mibs-downloader && \
	wget http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_3_general/esl-erlang_17.1-1~debian~wheezy_amd64.deb && \
	( dpkg -i esl-erlang_17.1-1~debian~wheezy_amd64.deb || true ) && \
	apt-get -f -y install
