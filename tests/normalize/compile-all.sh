#!/bin/sh
#
# compile all of the test programs
#

SOOLC=../../bin/soolc.sh

for f in test*sool ; do
  echo "***** $f"
  $SOOLC $f
done

for f in err-test*sool ; do
  echo "***** $f"
  $SOOLC $f
done

