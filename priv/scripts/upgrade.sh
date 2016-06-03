#!/bin/sh

if [ $# != 1 ]; then
    echo "err_arg"
    exit 2
fi

if [ ! -e $1 ]; then
    echo "err_file"
    exit 2
fi

if [ `id -u` != 0 ]; then
    echo "ok"
    exit 0
fi

opkg install --force-overwrite --force-reinstall $1 > /dev/null \
    && echo "ok" \
    || echo "err_dpkg"

rm -f /var/lib/bkfw/upload/*

exit 0

