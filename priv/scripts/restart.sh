#!/bin/sh


if [ `id -u` != 0 ]; then
    echo "ok"
    exit 0
fi

/opt/bkfw/bin/bkfw stop > /dev/null || true
/opt/bkfw/bin/bkfw start > /dev/null

echo "ok"

exit 0
