#!/bin/sh

BASEDIR=$(cd $(dirname $0) && pwd)
ebindir=${BASEDIR}/ebin
depsdir=${BASEDIR}/deps/*/ebin
reldir=${BASEDIR}/rel

erl -sname agent \
    -pa ${ebindir} \
    -pa ${depsdir} \
    -bkfw http "[{port, 8080}]" \
    -config ${reldir}/files/sys \
    -s bkfw

exit 0
