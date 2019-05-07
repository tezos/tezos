#! /usr/bin/env bash

set -e

export USE_TLS=true

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1

show_logs="no"

sleep 2

# Dummy command to test connection with node
$client bootstrapped

echo
echo End of test
echo
