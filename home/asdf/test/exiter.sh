#!/bin/sh

if [ $# -gt 3 ]; then
    exit 1
fi

if [ $# -ge 2 ]; then
    echo $2
fi
if [ $# -ge 3 ]; then
    echo $3 >&2
fi

exit ${1:-0}

