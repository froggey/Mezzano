#!/bin/sh

if [ $# -gt 2 ]; then
    exit 1
fi

sleep ${2:-0}
exit ${1:-0}
