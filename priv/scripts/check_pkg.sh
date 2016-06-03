#!/bin/sh

if [ $# != 1 ]; then
    echo "err_arg"
    exit 2
fi

EXPECT=bkfw
PKG=$(LANG=C opkg info $1 2> /dev/null | awk '/^Package:/ { print $2 }')
if [ "${PKG}" = "${EXPECT}" ]; then
    echo "ok"
else
    echo "err_badname"
fi

exit 0
