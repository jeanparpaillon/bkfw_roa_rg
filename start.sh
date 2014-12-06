#!/bin/sh

BASEDIR=$(cd $(dirname $0) && pwd)
ebindir=${BASEDIR}/ebin
depsdir=${BASEDIR}/deps/*/ebin
reldir=${BASEDIR}/rel

USERCONF=/var/tmp/bkfw_user.conf

usage() {
    echo "Usage: $0 [-p XXXX] /path/to/com"
}

port=8000
while getopts ":p:" opt; do
    case $opt in
	p)
	    shift $((OPTIND -1))
	    port=$OPTARG
	    ;;
	:)
	    usage
	    exit 1
	    ;;
	*)
	    usage
	    exit 1
    esac
done

if [ $# -lt 1 ]; then
    usage
    exit 1
fi

com=$1

if [ ! -e /var/lib/bkfw/factory.config ]; then
    echo "[]." > /var/lib/bkfw/factory.config
fi

if [ ! -e /var/lib/bkfw/user.config ]; then
    echo "[]." > /var/lib/bkfw/user.config
fi

erl -sname agent \
    -pa ${ebindir} \
    -pa ${depsdir} \
    -bkfw http "[{port, $port}]" \
    -bkfw com "\"$com\"" \
    -config ${reldir}/files/sys \
    -s bkfw

exit 0
