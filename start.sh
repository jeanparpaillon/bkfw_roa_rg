#!/bin/sh

BASEDIR=$(cd $(dirname $0) && pwd)
ebindir=${BASEDIR}/ebin
depsdir=${BASEDIR}/deps/*/ebin
privdir=${BASEDIR}/priv

erl -sname agent \
    -pa ${ebindir} \
    -pa ${depsdir} \
    -config ${privdir}/default \
    -s bkfw

exit 0
