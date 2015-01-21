#!/bin/sh


if [ `id -u` != 0 ]; then
    echo "ok"
    exit 0
fi

#/opt/bkfw/bin/bkfw resstart
reboot > /dev/null &

echo "ok"

exit 0
