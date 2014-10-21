#!/bin/sh

BASEDIR=$(cd $(dirname $0) && pwd)
ebindir=${BASEDIR}/ebin
privdir=${BASEDIR}/priv

erl -sname agent \
    -pa ${ebindir} \
    -config ${privdir}/default \
    -s bkfw

exit 0
