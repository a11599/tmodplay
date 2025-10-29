#!/usr/bin/env bash
set -e
. ./env

# Build binary

if [ "$1" == "db" ]; then
    _TEST=1
    _ARGS=${@:2}
else
    _ARGS=$@
fi
"${WATCOM_BIN_DIR}/wmake" ${_ARGS}

# Start DOSBox

if [ "$_TEST" == "1" ]; then
    "${DOSBOX_BIN}" test.bat -exit -c mount . -conf dosbox.conf
fi
