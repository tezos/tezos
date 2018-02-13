#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1

sleep 2
$client inject protocol "$test_dir/demo"
$client list protocols

echo
echo End of test
echo

show_logs="no"
