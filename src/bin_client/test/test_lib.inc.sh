#! /usr/bin/env bash

test_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && echo "$(pwd -P)")"
src_dir="$(dirname "$test_dir")"
cd "$test_dir"

sandbox_file="$test_dir/sandbox.json"
parameters_file="$test_dir/protocol_parameters.json"

export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y

tezos_sandboxed_node="${1:-$test_dir/../../bin_node/tezos-sandboxed-node.sh}"
local_node="${2:-$test_dir/../../../_build/default/src/bin_node/main.exe}"
tezos_init_sandboxed_client="${3:-$test_dir/../../bin_client/tezos-init-sandboxed-client.sh}"
local_client="${4:-$test_dir/../../../_build/default/src/bin_client/main_client.exe}"
local_admin_client="${5:-$test_dir/../../../_build/default/src/bin_client/main_admin.exe}"
local_compiler="${6:-$test_dir/../../../_build/default/src/lib_protocol_compiler/main_native.exe}"

contract_op_dir="contracts/opcodes"
contract_macros_dir="contracts/macros"
contract_scenarios_dir="contracts/mini_scenarios"
contract_attic_dir="contracts/attic"

source $tezos_sandboxed_node
source $tezos_init_sandboxed_client

### Log files handling

display_file() {
    echo
    echo "#################"
    echo "### $ cat $1"
    sed -e 's/^/### /' $1
    echo "#################"
    echo
}

log_files=()

register_log() {
    log_files+=("$1")
}

show_logs="${show_logs:-yes}"

display_logs() {
    if [ "$show_logs" = "yes" ]; then
        for file in "${log_files[@]}"; do
            display_file $file;
        done
        good_run=OK
    fi
}

### Node/Client instances control

client_instances=()
admin_client_instances=()

start_node() {
    local id=${1:-1}
    shift
    start_sandboxed_node $id "$@" > LOG.$id 2>&1
    register_log LOG.$id
    init_sandboxed_client $id
    wait_for_the_node_to_be_ready
    add_sandboxed_bootstrap_identities
    client_instances+=("$client")
    admin_client_instances+=("$admin_client")
    export "client$id=$client"
}

cleanup() {
    set -e
    display_logs
    cleanup_nodes
    cleanup_clients
}
trap cleanup EXIT INT

### Various helpers

run_contract_file () {
    local contract="$1"
    local storage="$2"
    local input="$3"
    local amount_flag=""
    if [ ! -z "$4" ]; then
        amount_flag="-amount $4"
    fi
    $client run script "$contract" on storage "$storage" and input "$input" $amount_flag
}

assert_storage () {
    local contract=$1;
    local input=$2;
    local storage=$3;
    local expected=$4;
    local amount=$5;
    echo "Testing [$contract]"
    local storage=$(run_contract_file "$contract" "$input" "$storage" "$amount"  | awk '/storage/{getline; print}' |
                        sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' ||
                        { printf '\nTest failed with error at line %s\n' "$(caller)" 1>&2;
                          exit 1; });
    if [ "$expected" != "$storage" ]; then
        echo "Test at " `caller` failed 1>&2 ;
        printf "Expected %s but got %s" "$expected" "$storage" 1>&2 ;
        exit 1;
    fi
}

assert_balance () {
    local KEY="$1"
    local EXPECTED_BALANCE="$2"
    local RESULT=$($client get balance for ${KEY})
    echo "[Asserting balance for '$KEY']"
    if [ "${RESULT}" != "${EXPECTED_BALANCE}" ]; then
        printf "Balance assertion failed for ${KEY} on line '%s'. Expected %s but got %s.\n" \
               "$(caller)" "${EXPECTED_BALANCE}" "${RESULT}"
        exit 2
    fi
}

contract_name_of_file () {
    basename "$1" ".tz"
}

init_contract_from_file () {
    local FILE="$1"
    local NAME=$(contract_name_of_file "${FILE}")
    $client remember script "${NAME}" "file:${FILE}"
}

bake () {
    $client bake for bootstrap1 --max-priority 512 --minimal-timestamp  --minimal-fees 0 --minimal-nanotez-per-byte 0 --minimal-nanotez-per-gas-unit 0
}

bake_after () {
    "$@"
    bake
}

init_with_transfer () {
    local FILE="$1"
    local NAME=$(contract_name_of_file "${FILE}")
    local KEY="$2"
    local INITIAL_STORAGE="$3"
    local TRANSFER_AMT="$4"
    local TRANSFER_SRC=${5-bootstrap1}
    echo "Originating [$NAME]"
    $client originate contract ${NAME} \
            for ${KEY} transferring "${TRANSFER_AMT}" \
            from ${TRANSFER_SRC} running "${FILE}" -init "${INITIAL_STORAGE}" --burn-cap 10
    bake
}

# Takes a grep regexp and fails with an error message if command does not include
# the regexp
assert_in_output () {
    local MATCHING="$1"
    local INPUT="$2"
    if ! grep -q "${MATCHING}" ${INPUT}; then
        printf "\nFailure on line %s. Expected to find %s in output." \
               "$(caller)"  "${MATCHING}"
        exit 1
    else
        echo "[Assertion succeeded]"
    fi
}

get_contract_addr () {
    local CONTRACT_NAME="$1"
    $client show known contract "${CONTRACT_NAME}"
}

contract_storage () {
    local CONTRACT_NAME="$1"    # Can be either an alias or hash
    $client get script storage for ${CONTRACT_NAME}
}

assert_storage_contains () {
    local CONTRACT_NAME="$1"
    local EXPECTED_STORAGE="$2"
    contract_storage ${CONTRACT_NAME} | assert_in_output "${EXPECTED_STORAGE}"
}

assert() {
    local expected="$1"
    local result="$(cat)"
    if [ "${result}" != "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Expected: \"${expected}\""
        exit 2
    fi
}

assert_success() {
    printf "[Asserting success]\n"
    if "$@" 2> /dev/null; then
        return 0
    else
        printf "Expected command line to success, but failed:\n"
        echo "$@"
        exit 1
    fi
}

assert_fails() {
    printf "[Asserting failure]\n"
    if "$@" 2> /dev/null; then
        printf "Expected command line to fail, but succeeded:\n"
        echo "$@"
        exit 1
    else
        return 0
    fi
}

assert_contract_fails() {
    local contract=$1;
    local input=$2;
    local storage=$3;
    local amount=$4;
    printf "Testing failure for [$contract]\n"
    if run_contract_file "$contract" "$input" "$storage" "$amount" 2> /dev/null ; then
        printf "Expected contract execution to fail, but succeeded:\n"
        exit 1
    fi
}

extract_operation_hash() {
    grep "Operation hash is" | grep -o "'.*'" | tr -d "'"
}

assert_propagation_level() {
    level=$1
    printf "\n\nAsserting all nodes have reached level %s\n" "$level"
    for client in "${client_instances[@]}"; do
        ( $client rpc get /chains/main/blocks/head/header/shell \
              | assert_in_output "\"level\": $level" ) \
            || exit 2
    done
}

assert_protocol() {
    proto=$1
    printf "\n\nAsserting protocol propagation\n"
    for client in "${client_instances[@]}"; do
        ( $client -p ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im \
                  rpc get /chains/main/blocks/head/metadata | assert_in_output "\"next_protocol\": \"$proto\"" ) \
              || exit 2
    done
}

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
