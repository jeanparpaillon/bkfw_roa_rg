#!/bin/sh

BASEDIR=$(cd $(dirname $0) && pwd)
ebindir=${BASEDIR}/ebin
depsdir=${BASEDIR}/deps/*/ebin
reldir=${BASEDIR}/rel

USERCONF=/var/tmp/bkfw_user.conf

if [ ! -e /var/tmp/bkfw_user.confif ]; then
    echo "[]." > /var/tmp/bkfw_user.config
fi

erl -sname agent \
    -pa ${ebindir} \
    -pa ${depsdir} \
    -bkfw http "[{port, 8000}]" \
    -config ${reldir}/files/sys \
    -s bkfw

exit 0
