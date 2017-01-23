#! /bin/sh

set -e

DIR=$(dirname "$0")
cd "${DIR}"

DATA_DIR="$(mktemp -td tezos_node.XXXXXXXXXX)"
CLIENT_DIR="$(mktemp -td tezos_client.XXXXXXXXXX)"

cleanup() {
    rm -fr ${DATA_DIR} ${CLIENT_DIR}
    [ -z "${NODE_PID}" ] || kill -9 ${NODE_PID}
}
trap cleanup EXIT QUIT INT

NODE=../tezos-node
CLIENT="../tezos-client -base-dir ${CLIENT_DIR}"

CUSTOM_PARAM="--sandbox ./sandbox.json"
${NODE} --base-dir "${DATA_DIR}" ${CUSTOM_PARAM} --rpc-addr :::8732 > LOG 2>&1 &
NODE_PID="$!"

sleep 3

${CLIENT} list versions
${CLIENT} bootstrap

KEY1=foo
KEY2=bar

${CLIENT} gen keys ${KEY1}
${CLIENT} gen keys ${KEY2}

${CLIENT} list known identities

${CLIENT} transfer 1000 from bootstrap1 to ${KEY1}
${CLIENT} transfer 2000 from bootstrap1 to ${KEY2}

assert() {
    local expected="$1"
    local result="$(cat)"
    if [ "${result}" != "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Expected: \"${expected}\""
        exit 2
    fi
}

${CLIENT} get balance ${KEY1} | assert "1,000.00 ꜩ"
${CLIENT} get balance ${KEY2} | assert "2,000.00 ꜩ"

${CLIENT} transfer 1000 from ${KEY2} to ${KEY1}

${CLIENT} get balance ${KEY1} | assert "2,000.00 ꜩ"
${CLIENT} get balance ${KEY2} | assert "999.95 ꜩ"

# Should fail
# ${CLIENT} transfer 999.95 from ${KEY2} to ${KEY1}

${CLIENT} mine for bootstrap1

${CLIENT} remember program noop file:scripts/noop.tez
${CLIENT} typecheck program noop
${CLIENT} originate contract noop \
          for ${KEY1} transfering 1000 from bootstrap1 \
          running noop
${CLIENT} transfer 10 from bootstrap1 to noop -arg "Unit"

${CLIENT} originate contract hardlimit \
          for ${KEY1} transfering 1000 from bootstrap1 \
          running file:scripts/hardlimit.tez -init "3"
${CLIENT} transfer 10 from bootstrap1 to hardlimit -arg "Unit"
${CLIENT} transfer 10 from bootstrap1 to hardlimit -arg "Unit"
# ${CLIENT} transfer 10 from bootstrap1 to hardlimit -arg "unit" # should fail

echo
echo End of test
echo
