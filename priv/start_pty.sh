#!/bin/sh

PTY_OPTS=raw,echo=0,b9600,icrnl=0,inlcr=0,ocrnl=0,onlcr=0

socat -d -d pty,$PTY_OPTS pty,$PTY_OPTS

exit 0
