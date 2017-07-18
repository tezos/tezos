#!/bin/bash
# Run this as a command in scripts to test if a contract produces the correct output
# Write `source test-michelson.sh`

DATA_DIR="$(mktemp -d -t tezos_node.XXXXXXXXXX)"
CLIENT_DIR="$(mktemp -d -t tezos_client.XXXXXXXXXX)"

TZCLIENT="../tezos-client -base-dir ${CLIENT_DIR}"
TZNODE=../tezos-node

cleanup() {
    [ -z "${TZNODE_PID}" ] || kill -9 ${TZNODE_PID} || true
    printf "\nNode's log:\n" > /dev/stderr
    cat $DATA_DIR/LOG > /dev/stderr
    rm -fr ${DATA_DIR} ${CLIENT_DIR}
}
trap cleanup EXIT QUIT INT SIGINT SIGKILL

CUSTOM_PARAM="--sandbox sandbox.json"
${TZNODE} run --data-dir "${DATA_DIR}" ${CUSTOM_PARAM} --rpc-addr "[::]:8732" > "$DATA_DIR"/LOG 2>&1 &
TZNODE_PID="$!"

echo "Created node, pid: ${TZNODE_PID}, log: $DATA_DIR/LOG" > /dev/stderr

sleep 3

${TZCLIENT} -block genesis \
          activate \
          protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
          with fitness 1 \
          and key edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z


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
        printf "Failure on line %s. Expected to find %s in output." \
               "$(caller)"  "${MATCHING}"
        exit 1
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
BOOTSTRAP3_IDENTITY=tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU
BOOTSTRAP4_IDENTITY=tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv
BOOTSTRAP5_IDENTITY=tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv

KEY1=foo
KEY2=bar

${TZCLIENT} add identity bootstrap1 ${BOOTSTRAP1_IDENTITY}
${TZCLIENT} add public key bootstrap1 ${BOOTSTRAP1_PUBLIC}
${TZCLIENT} add secret key bootstrap1 ${BOOTSTRAP1_SECRET}
${TZCLIENT} add identity bootstrap2 ${BOOTSTRAP2_IDENTITY}
${TZCLIENT} add identity bootstrap3 ${BOOTSTRAP3_IDENTITY}
${TZCLIENT} add identity bootstrap4 ${BOOTSTRAP4_IDENTITY}
${TZCLIENT} add identity bootstrap5 ${BOOTSTRAP5_IDENTITY}

sleep 2

${TZCLIENT} gen keys ${KEY1}
${TZCLIENT} gen keys ${KEY2}

# For ease of use outside of the script
alias client="${TZCLIENT}"
