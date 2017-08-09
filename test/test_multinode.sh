#!/usr/bin/env bash

source test_utils.sh

export LWT_ASYNC_METHOD="none"

node1_rpcs=3000
node1_addr=[::1]:3001
node2_rpcs=3002
node2_addr=[::1]:3003
node3_rpcs=3004
node3_addr=[::1]:3005
node4_rpcs=3006
node4_addr=[::1]:3007

CLIENT_1="$(make_client) -addr [::1] -port $node1_rpcs"
CLIENT_2="$(make_client) -addr [::1] -port $node2_rpcs"
CLIENT_3="$(make_client) -addr [::1] -port $node3_rpcs"
CLIENT_4="$(make_client) -addr [::1] -port $node4_rpcs"



assert_propagation_level() {
    level=$1
    printf "\n\nAsserting all nodes have reached level %s\n" "$level"
    ${CLIENT_1} rpc call /blocks/head/proto/context/level \
        | assert_in_output "\"level\": $level"
    ${CLIENT_2} rpc call /blocks/head/proto/context/level \
        | assert_in_output "\"level\": $level"
    ${CLIENT_3} rpc call /blocks/head/proto/context/level \
        | assert_in_output "\"level\": $level"
    ${CLIENT_4} rpc call /blocks/head/proto/context/level \
        | assert_in_output "\"level\": $level"
}

start_sandboxed_node --rpc-addr=[::1]:$node1_rpcs --net-addr=$node1_addr --peer=$node2_addr --no-bootstrap-peers
start_sandboxed_node --rpc-addr=[::1]:$node2_rpcs --net-addr=$node2_addr --peer=$node1_addr --no-bootstrap-peers
start_sandboxed_node --rpc-addr=[::1]:$node3_rpcs --net-addr=$node3_addr --peer=$node1_addr --no-bootstrap-peers
start_sandboxed_node --rpc-addr=[::1]:$node4_rpcs --net-addr=$node4_addr --peer=$node1_addr --no-bootstrap-peers

sleep 3

printf "\n\n"

activate_alpha [::1] $node1_rpcs

sleep 3

printf "\n\nAsserting protocol propagation\n"

${CLIENT_1} rpc call /blocks/head/protocol \
    | assert_in_output "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
${CLIENT_2} rpc call /blocks/head/protocol \
    | assert_in_output "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
${CLIENT_3} rpc call /blocks/head/protocol \
    | assert_in_output "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
${CLIENT_4} rpc call /blocks/head/protocol \
    | assert_in_output "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

printf "\n\n"

add_bootstrap_identities "${CLIENT_1}"
add_bootstrap_identities "${CLIENT_2}"
add_bootstrap_identities "${CLIENT_3}"
add_bootstrap_identities "${CLIENT_4}"

printf "\n\n"

${CLIENT_1} mine for bootstrap1

sleep 3

assert_propagation_level 2
${CLIENT_2} mine for bootstrap2

sleep 3

assert_propagation_level 3
${CLIENT_3} mine for bootstrap3

sleep 3

assert_propagation_level 4
${CLIENT_4} mine for bootstrap4

sleep 3

assert_propagation_level 5

endorse_hash=$(${CLIENT_3} endorse for bootstrap3 | extract_operation_hash)

transfer_hash=$(${CLIENT_4} transfer 500 from bootstrap1 to bootstrap3 | extract_operation_hash)

sleep 3

${CLIENT_4} mine for bootstrap4

sleep 3

assert_contains_operation() {
    hash="$1"
    printf "Asserting operations list contains '$hash'\n"
    ${CLIENT_1} rpc call /blocks/head/operations with {} \
        | assert_in_output $hash
    ${CLIENT_2} rpc call /blocks/head/operations with {} \
        | assert_in_output $hash
    ${CLIENT_3} rpc call /blocks/head/operations with {} \
        | assert_in_output $hash
    ${CLIENT_4} rpc call /blocks/head/operations with {} \
        | assert_in_output $hash
}

assert_contains_operation $endorse_hash
assert_contains_operation $transfer_hash

# printf "\nEnd of test"
