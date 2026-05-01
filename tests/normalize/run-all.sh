#!/bin/sh
#
# run all of the test programs using sooli
#

SOOLI=../../bin/sooli.sh

for f in test*sool ; do
  base=$(basename $f .sool)
  echo "***** $f"
  $SOOLI --print $base.out --step-limit 1000 $f
done

for f in err-test*sool ; do
  base=$(basename $f .sool)
  echo "***** $f"
  $SOOLI --print $base.out --step-limit 1000 $f
done

