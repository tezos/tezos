#!/bin/bash
# run test at most NUM times, exit at first failure

if [ $# -ne 2 ] || [ "$1" -le 0 ] || [ ! -f $2 ]; then
    echo "Repeat a pytest test until first failure, at most N times ."
    echo
    echo "Usage: $0 NUM TEST"
    echo "  where NUM should be a positive integer and TEST a python script"
    echo "  executable with pytest."
    exit 1
fi

NUM=$1
TEST=$2

mkdir -p tmp
for i in $(seq 1 $NUM)
do
    rm -f tmp/*
    echo Execution $i/$NUM
    if ! pytest $TEST --log-dir=tmp --tb=short -v; then
        exit 1
    fi
done
exit 0

