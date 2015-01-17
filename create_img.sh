#!/bin/sh

usage() {
    echo "Usage: $0 /path/to/dev"
}

if [ `id -u` != 0 ]; then
    sudo $0 $@
    exit 0
fi

if [ $# -ne 1 ]; then
    usage
    exit 1
fi

if [ ! -b "$1" ]; then
    echo "E: not a block device"
fi

DEV=$1
LAST=$(LANG=C fdisk $DEV -l | egrep '^/dev/mmcblk0' | awk '{ print $3}' | sort -n -r | head -1)
UNIT=$(LANG=C fdisk $DEV -l | egrep '^Units = ' | awk '{ print $9 }')

if [ -t 1 ]; then
    echo "I don't want to dump a partition to the terminal, sorry !"
    exit 1
fi

COUNT=$(( LAST * UNIT / 1024 / 1024 / 4 ))
echo "Dumping $COUNT blocks of 4MB from $DEV" >&2
dd bs=4M count=$COUNT if=$DEV | pv -pe -s $(( LAST * UNIT ))

exit 0
