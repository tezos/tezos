#!/usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

expected_connections=4
max_peer_id=8
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
    echo "### $client p2p stat"
    echo
    $client bootstrapped
    $admin_client network stat
    echo
done

activate_alpha

assert_propagation_level() {
    level=$1
    printf "\n\nAsserting all nodes have reached level %s\n" "$level"
    for client in "${client_instances[@]}"; do
        ( $client rpc call /blocks/head/proto/context/level \
              | assert_in_output "\"level\": $level" ) \
            || exit 2
    done
}


assert_protocol() {
    proto=$1
    printf "\n\nAsserting protocol propagation\n"
    for client in "${client_instances[@]}"; do
        ( $client rpc call /blocks/head/protocol | assert_in_output "$proto" ) \
              || exit 2
    done
}


printf "\n\n"

retry() {
    local timeout=$1
    local attempts=$2
    shift 2
    sleep $timeout
    while ! ( "$@" ) ; do
        echo
        echo "Will retry after $timeout seconds..."
        echo
        sleep $timeout
        attempts=$(($attempts-1))
        if [ "$attempts" -eq 0 ] ; then
            echo
            echo "Failed after too many retries" 1>&2
            exit 1
        fi
    done
}

retry 2 15 assert_protocol "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

$client1 bake for bootstrap1 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 2

$client2 bake for bootstrap2 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 3

$client3 bake for bootstrap3 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 4

$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 5

endorse_hash=$($client3 endorse for bootstrap3 | extract_operation_hash)
transfer_hash=$($client4 transfer 500 from bootstrap1 to bootstrap3 | extract_operation_hash)

# wait for the propagation of operations
sleep 2

assert_contains_operation() {
    hash="$1"
    printf "Asserting operations list contains '$hash'\n"
    for client in "${client_instances[@]}"; do
        ( $client rpc call /blocks/head/operations with {} \
              | assert_in_output $hash ) \
            || exit 2
    done
}

$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
retry 2 15 assert_contains_operation $endorse_hash
retry 2 15 assert_contains_operation $transfer_hash

echo
echo End of test
echo

show_logs="no"
