#!/bin/sh

socat -d -d pty,raw,echo=0,b9600 pty,raw,echo=0,b9600

exit 0
