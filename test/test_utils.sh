#!/usr/bin/env bash

# If the TZPATH environment variable is not set,
# assume that we're in the test directory
if [ -z "$TZPATH" ]; then
    export TZPATH="../"
fi

# Global arrays for cleanup
if [ -z "${CLEANUP_DIS}" ]; then
    export CLEANUP_DIS=()
fi

if [ -z "${CLEANUP_PROCESSES}" ]; then
    export CLEANUP_PROCESSES=()
fi

cleanup() {
    for ps in "${CLEANUP_PROCESSES[@]}"; do
        kill -9 $ps
    done
    CLEANUP_PROCESSES=()
    sleep 2

    for node_dir in "${CLEANUP_DIS[@]}"; do
        printf "\nNode's log:\n" > /dev/stderr
        cat $node_dir/LOG > /dev/stderr
        rm -rf $node_dir
    done
    CLEANUP_DIS=()

    for client_dir in ${CLIENT_DIRS[@]}; do
        rm -rf $client_dir
    done
    CLIENT_DIRS=()
}
trap cleanup EXIT

register_dir() {
    CLEANUP_DIS+=("$1")
}

make_client () {
    client_dir="$(mktemp -d -t tezos_client.XXXXXXXXXX)"
    echo "${TZPATH}/tezos-client -base-dir ${client_dir}"
}
TZCLIENT=$(make_client)
TZNODE="${TZPATH}/tezos-node"

CUSTOM_PARAM="--sandbox=${TZPATH}/test/sandbox.json"

start_sandboxed_node() {
    if [ "$#" == "0" ]; then
        default_args=--rpc-addr=[::]:8732
    else
        default_args=""
    fi

    data_dir="$(mktemp -d -t tezos_node.XXXXXXXXXX)"
    register_dir "$data_dir"
    ${TZNODE} identity generate 0 --data-dir "${data_dir}"
    ${TZNODE} config init --data-dir=${data_dir} --connections=2 --expected-pow=0.0
    ${TZNODE} run --data-dir "${data_dir}" ${CUSTOM_PARAM} "$@" $default_args > "$data_dir"/LOG 2>&1 &
    node_pid="$!"
    CLEANUP_PROCESSES+=($node_pid)
    echo "Created node, pid: ${node_pid}, log: $data_dir/LOG" > /dev/stderr
}


activate_alpha() {
    addr=${1:-[::]}
    port=${2:-8732}
    ${TZCLIENT} -port $port -addr $addr \
                -block genesis \
                activate \
                protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
                with fitness 1 \
                and key edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z
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
        echo "Test at" `caller` failed > /dev/stderr;
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

BOOTSTRAP1_IDENTITY=tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
BOOTSTRAP1_PUBLIC=edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav
BOOTSTRAP1_SECRET=edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi
BOOTSTRAP2_IDENTITY=tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN
BOOTSTRAP2_PUBLIC=edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9
BOOTSTRAP2_SECRET=edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDbym9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc
BOOTSTRAP3_IDENTITY=tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU
BOOTSTRAP3_PUBLIC=edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV
BOOTSTRAP3_SECRET=edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWBypUSbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC
BOOTSTRAP4_IDENTITY=tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv
BOOTSTRAP4_PUBLIC=edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU
BOOTSTRAP4_SECRET=edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyPJdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL
BOOTSTRAP5_IDENTITY=tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv
BOOTSTRAP5_PUBLIC=edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n
BOOTSTRAP5_SECRET=edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcCyM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ

KEY1=foo
KEY2=bar

add_bootstrap_identities() {
    client=${1:-${TZCLIENT}}
    ${client} add identity bootstrap1 ${BOOTSTRAP1_IDENTITY}
    ${client} add public key bootstrap1 ${BOOTSTRAP1_PUBLIC}
    ${client} add secret key bootstrap1 ${BOOTSTRAP1_SECRET}

    ${client} add identity bootstrap2 ${BOOTSTRAP2_IDENTITY}
    ${client} add public key bootstrap2 ${BOOTSTRAP2_PUBLIC}
    ${client} add secret key bootstrap2 ${BOOTSTRAP2_SECRET}

    ${client} add identity bootstrap3 ${BOOTSTRAP3_IDENTITY}
    ${client} add public key bootstrap3 ${BOOTSTRAP3_PUBLIC}
    ${client} add secret key bootstrap3 ${BOOTSTRAP3_SECRET}

    ${client} add identity bootstrap4 ${BOOTSTRAP4_IDENTITY}
    ${client} add public key bootstrap4 ${BOOTSTRAP4_PUBLIC}
    ${client} add secret key bootstrap4 ${BOOTSTRAP4_SECRET}

    ${client} add identity bootstrap5 ${BOOTSTRAP5_IDENTITY}
    ${client} add public key bootstrap5 ${BOOTSTRAP5_PUBLIC}
    ${client} add secret key bootstrap5 ${BOOTSTRAP5_SECRET}

    sleep 2

    ${client} gen keys ${KEY1}
    ${client} gen keys ${KEY2}
}

extract_operation_hash() {
    grep "Operation hash is" | grep -o "'.*'" | tr -d "'"
}
