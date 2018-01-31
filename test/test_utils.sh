#!/usr/bin/env bash

# If the TZPATH environment variable is not set,
# assume that we're in the test directory
if [ -z "$TZPATH" ]; then
    export TZPATH="../"
fi

# Global arrays for cleanup
if [ -z "${CLEANUP_DIRS}" ]; then
    export CLEANUP_DIRS=()
fi

if [ -z "${CLEANUP_PROCESSES}" ]; then
    export CLEANUP_PROCESSES=()
fi

cleanup() {
    for ps in "${CLEANUP_PROCESSES[@]}"; do
        kill -9 $ps
    done
    CLEANUP_PROCESSES=();
    sleep 2

    for node_dir in "${CLEANUP_DIRS[@]}"; do
        printf "\nNode's log:\n" 1>&2
        [ ! -f $node_dir/LOG ] || cat $node_dir/LOG 1>&2
        rm -rf $node_dir
    done
    CLEANUP_DIRS=()

    for client_dir in ${CLIENT_DIRS[@]}; do
        rm -rf $client_dir;
    done
    CLIENT_DIRS=();
}
trap cleanup EXIT

register_dir() {
    CLEANUP_DIRS+=("$1")
}

make_client () {
    client_dir="$(mktemp -d -t tezos_client.XXXXXXXXXX)"
    echo "${TZPATH}/tezos-client -base-dir ${client_dir}"
}
TZCLIENT_DIR="$(mktemp -d -t tezos_client.XXXXXXXXXX)"
register_dir "${TZCLIENT_DIR}"
export TZCLIENT="${TZPATH}/tezos-client -base-dir ${TZCLIENT_DIR}"
export TZNODE="${TZPATH}/tezos-node"

CUSTOM_PARAM="--sandbox=${TZPATH}/test/sandbox.json"

start_sandboxed_node() {
    if [ "$#" == "0" ]; then
        default_args=--rpc-addr=[::]:8732
    else
        default_args=""
    fi

    data_dir="$(mktemp -d -t tezos_node.XXXXXXXXXX)"
    register_dir "$data_dir"
    ${TZNODE} identity generate 0 --data-dir "${data_dir}" 2>&1|  sed 's/^/## /' 1>&2
    ${TZNODE} config init --data-dir="${data_dir}" --connections=2 --expected-pow=0.0 2>&1| sed 's/^/## /' 1>&2
    ${TZNODE} run --data-dir "${data_dir}" ${CUSTOM_PARAM} "$@" $default_args > "$data_dir"/LOG 2>&1 &
    node_pid="$!"
    CLEANUP_PROCESSES+=($node_pid)
    export CLEANUP_PROCESSES
    echo alias tezos-client=\"${TZCLIENT} \"\;
    echo alias "tezos-sandbox-stop=\"kill -9 ${node_pid}; sleep 1; rm -rf ${data_dir} ${TZCLIENT_DIR};\""
    echo "## Created node, pid: ${node_pid}, log: $data_dir/LOG" 1>&2
}


activate_alpha() {
    addr=${1:-[::]}
    port=${2:-8732}
    ${TZCLIENT} -port $port -addr $addr \
                -block genesis \
                activate \
                protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
                with fitness 1 \
                and key edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6 \
                > /dev/stderr
}



run_contract_file () {
    local contract=$1;
    local storage=$2;
    local input=$3;
    ${TZCLIENT} run program "$contract" on storage "$storage" and input "$input";
}

assert_output () {
    local contract=$1;
    local input=$2;
    local storage=$3;
    local expected=$4;
    echo "Testing [$contract]"
    local output=$(run_contract_file "$contract" "$input" "$storage" | sed '1,/output/d' |
                       sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' ||
                       { printf '\nTest failed with error at line %s\n' "$(caller)" > /dev/stderr;
                         exit 1; });
    if [ "$expected" != "$output" ]; then
        echo "Test at " `caller` failed > /dev/stderr;
        printf "Expected %s but got %s" "$expected" "$output" > /dev/stderr;
        exit 1;
    fi
}

assert_balance () {
    local KEY="$1"
    local EXPECTED_BALANCE="$2"
    local RESULT=$(${TZCLIENT} get balance for ${KEY})
    if [ "${RESULT}" != "${EXPECTED_BALANCE}" ]; then
        printf "Balance assertion failed for ${KEY} on line '%s'. Expected %s but got %s.\n" \
               "$(caller)" "${EXPECTED_BALANCE}" "${RESULT}"
        exit 2
    fi
}

contract_name_of_file () {
    basename ${FILE} ".tz"
}

init_contract_from_file () {
    local FILE="$1"
    local NAME=$(contract_name_of_file ${FILE})
    ${TZCLIENT} remember program "${NAME}" "file:${FILE}"
}

init_with_transfer () {
    local FILE="$1"
    local NAME=$(contract_name_of_file ${FILE})
    local KEY="$2"
    local INITIAL_STORAGE="$3"
    local TRANSFER_AMT="$4"
    local TRANSFER_SRC=${5-bootstrap1}
    echo "Originating [$NAME]"
    ${TZCLIENT} originate contract ${NAME} \
                for ${KEY} transferring "${TRANSFER_AMT}" \
                from ${TRANSFER_SRC} running "${FILE}" -init "${INITIAL_STORAGE}"
}

# Takes a grep regexp and fails with an error message if command does not include
# the regexp
assert_in_output () {
    local MATCHING="$1"
    local INPUT=${2-/dev/stdin}
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
    ${TZCLIENT} show known contract "${CONTRACT_NAME}"
}

contract_storage () {
    local CONTRACT_NAME="$1"    # Can be either an alias or hash
    ${TZCLIENT} get storage for ${CONTRACT_NAME}
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

BOOTSTRAP1_SECRET=edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh
BOOTSTRAP2_SECRET=edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo
BOOTSTRAP3_SECRET=edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ
BOOTSTRAP4_SECRET=edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3
BOOTSTRAP5_SECRET=edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm

KEY1=foo
KEY2=bar

add_bootstrap_identities() {
    client=${1:-${TZCLIENT}}
    ${client} import unencrypted secret key bootstrap1 ${BOOTSTRAP1_SECRET}
    ${client} import unencrypted secret key bootstrap2 ${BOOTSTRAP2_SECRET}
    ${client} import unencrypted secret key bootstrap3 ${BOOTSTRAP3_SECRET}
    ${client} import unencrypted secret key bootstrap4 ${BOOTSTRAP4_SECRET}
    ${client} import unencrypted secret key bootstrap5 ${BOOTSTRAP5_SECRET}

    sleep 2

    ${client} gen keys ${KEY1}
    ${client} gen keys ${KEY2}
}

extract_operation_hash() {
    grep "Operation hash is" | grep -o "'.*'" | tr -d "'"
}

display_aliases() {
    echo <<EOF

alias tezos-client="${TZCLIENT} "\;
alias tezos-sandbox-stop="kill -9 ${node_pid}; sleep 1; rm -rf ${CLEANUP_DIRS[@]}; unalias tezos-client tezos-sandbox-stop"

EOF
}

