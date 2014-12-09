#!/bin/sh

if [ `id -u` != 0 ]; then
    echo "err_user"
    exit 1
fi

if [ $# != 1 ]; then
    echo "err_arg"
    exit 2
fi

if [ ! -e $1 ]; then
    echo "err_file"
    exit 2
fi

dpkg -i --force-overwrite $1 > /dev/null \
    && echo "ok"
    || echo "err_dpkg"

exit 0

