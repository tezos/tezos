#! /usr/bin/env bash

test_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && echo "$(pwd -P)")"
src_dir="$(dirname "$test_dir")"
cd "$test_dir"

sandbox_file="$test_dir/sandbox.json"

tezos_sandboxed_node="${1:-$test_dir/../../bin_node/tezos-sandboxed-node.sh}"
local_node="${2:-$test_dir/../../../_build/default/src/bin_node/main.exe}"
tezos_init_sandboxed_client="${3:-$test_dir/../../bin_client/tezos-init-sandboxed-client.sh}"
local_client="${4:-$test_dir/../../../_build/default/src/bin_client/main.exe}"

contract_dir="contracts"

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

show_logs=yes

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

start_node() {
    local id=${1:-1}
    start_sandboxed_node $id > LOG.$id 2>&1
    register_log LOG.$id
    init_sandboxed_client $id
    wait_for_the_node_to_be_ready
    add_sandboxed_bootstrap_identities
    client_instances+=("$client")
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
    $client run program "$contract" on storage "$storage" and input "$input" $amount_flag
}

assert_output () {
    local contract=$1;
    local input=$2;
    local storage=$3;
    local expected=$4;
    local amount=$5;
    echo "Testing [$contract]"
    local output=$(run_contract_file "$contract" "$input" "$storage" "$amount" | sed '1,/output/d' |
                       sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' ||
                       { printf '\nTest failed with error at line %s\n' "$(caller)" 1>&2;
                         exit 1; });
    if [ "$expected" != "$output" ]; then
        echo "Test at " `caller` failed 1>&2 ;
        printf "Expected %s but got %s" "$expected" "$output" 1>&2 ;
        exit 1;
    fi
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
    $client remember program "${NAME}" "file:${FILE}"
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
            from ${TRANSFER_SRC} running "${FILE}" -init "${INITIAL_STORAGE}"
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
    $client get storage for ${CONTRACT_NAME}
}

assert_storage_contains () {
    local CONTRACT_NAME="$1"
    local EXPECTED_STORAGE="$2"
    contract_storage ${CONTRACT_NAME} | assert_in_output ${EXPECTED_STORAGE}
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
