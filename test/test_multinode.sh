#!/usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/lib/test_lib.inc.sh

expected_connections=3
max_peer_id=8
for i in $(seq 1 $max_peer_id); do
    echo
    echo "## Starting node $i."
    echo
    start_node $i
    echo
done

## waiting for the node to establich connections
sleep 2
for client in "${client_instances[@]}"; do
    echo
    echo "### $client network stat"
    echo
    $client network stat
    echo
done

activate_alpha
sleep 5

assert_propagation_level() {
    level=$1
    printf "\n\nAsserting all nodes have reached level %s\n" "$level"
    for client in "${client_instances[@]}"; do
        $client rpc call /blocks/head/proto/context/level \
            | assert_in_output "\"level\": $level"
    done
}

printf "\n\nAsserting protocol propagation\n"

for client in "${client_instances[@]}"; do
    $client rpc call /blocks/head/protocol \
        | assert_in_output "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
done

printf "\n\n"

$client1 mine for bootstrap1
sleep 5
assert_propagation_level 2

$client2 mine for bootstrap2
sleep 5
assert_propagation_level 3

$client3 mine for bootstrap3
sleep 5
assert_propagation_level 4

$client4 mine for bootstrap4
sleep 5
assert_propagation_level 5

endorse_hash=$($client3 endorse for bootstrap3 | extract_operation_hash)
transfer_hash=$($client4 transfer 500 from bootstrap1 to bootstrap3 | extract_operation_hash)
sleep 5

$client4 mine for bootstrap4
sleep 5

assert_contains_operation() {
    hash="$1"
    printf "Asserting operations list contains '$hash'\n"
    for client in "${client_instances[@]}"; do
        $client rpc call /blocks/head/operations with {} \
            | assert_in_output $hash
    done
}

assert_contains_operation $endorse_hash
assert_contains_operation $transfer_hash

echo
echo End of test
echo

show_logs="no"
