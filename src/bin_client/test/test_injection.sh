#! /usr/bin/env bash

## from genesis to proto_test_injection

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

expected_connections=2
max_peer_id=3
for i in $(seq 1 $max_peer_id); do
    echo
    echo "## Starting node $i."
    echo
    start_node $i
    echo
done

## waiting for the node to establish connections

for client in "${client_instances[@]}"; do
    echo
    echo "### $client bootstrapped"
    echo
    $client -w none config update
    $client bootstrapped
    echo
done

sleep 2

protocol_version=`$compiler -hash-only "$test_dir/proto_test_injection"`

$admin_client inject protocol "$test_dir/proto_test_injection"
$admin_client list protocols

$client activate protocol $protocol_version \
        with fitness 1 \
        and key activator \
        and parameters $parameters_file

retry 2 15 assert_protocol "$protocol_version"

echo
echo End of test
echo

show_logs="no"
