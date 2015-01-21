#!/bin/sh

if [ $# != 1 ]; then
    echo "err_arg"
    exit 2
fi

if [ `id -u` != 0 ]; then
    echo "ok"
    exit 0
fi

ifdown $1 > /dev/null 2>&1
ifup $1 > /dev/null 2>&1 &

echo "ok"

exit 0

