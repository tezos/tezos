#! /bin/sh

set -e

if [ $1 != "zeronet" ] && [ $1 != "alphanet" ] ; then
    echo Ignored
    exit 0
fi

if git log | grep "net: change economic constants" >/dev/null 2>&1 ; then
    echo OK
    exit 0
fi

cat <<EOF

The branch $1 should include the constant patch.
Please run './scripts/apply-patch.sh $1'.

EOF

exit 1
