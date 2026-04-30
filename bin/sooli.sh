#!/bin/sh
#
# Shell wrapper for SooL interpreter
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2026
# University of Chicago
#

DIR="$(cd "$(dirname "$0")" && pwd)"

HEAP_SUFFIX="$(sml @SMLsuffix)"

HEAP="$DIR/sooli.$HEAP_SUFFIX"

if test ! -r "$HEAP" ; then
  echo "Heap image $HEAP not found; run make to build"
  exit 1
fi

exec sml @SMLcmdname="$0" @SMLload="$HEAP" "$@"
