#!/bin/sh

if [ `id -u` != 0 ]; then
    echo "err_user"
    exit 1
fi

if [ $# != 1 ]; then
    echo "err_arg"
    exit 2
fi

ifconfig $1 down > /dev/null 2>&1
ifconfig $1 up > /dev/null 2>&1 \
    && echo "ok" \
    || echo "err_network"

exit 0

