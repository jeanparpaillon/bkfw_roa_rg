#!/bin/sh

/usr/sbin/snmptrapd -f -m ALL -M +mibs -Lo localhost:4001
