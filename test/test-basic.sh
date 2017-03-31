#! /bin/sh

set -e

DIR=$(dirname "$0")
cd "${DIR}"

DATA_DIR="$(mktemp -td tezos_node.XXXXXXXXXX)"
CLIENT_DIR="$(mktemp -td tezos_client.XXXXXXXXXX)"

cleanup() {
    [ -z "${NODE_PID}" ] || kill -9 ${NODE_PID} || true
    echo
    echo "Node's log:"
    echo
    cat $DATA_DIR/LOG
    rm -fr ${DATA_DIR} ${CLIENT_DIR}
}
trap cleanup EXIT QUIT INT

NODE=../tezos-node
CLIENT="../tezos-client -base-dir ${CLIENT_DIR}"

CUSTOM_PARAM="--sandbox ./sandbox.json"
${NODE} run --data-dir "${DATA_DIR}" ${CUSTOM_PARAM} --rpc-addr "[::]:8732" > "$DATA_DIR"/LOG 2>&1 &
NODE_PID="$!"

echo "Created node, pid: ${NODE_PID}, log: $DATA_DIR/LOG"

sleep 3

${CLIENT} list versions

${CLIENT} add identity bootstrap1 tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
${CLIENT} add public key bootstrap1 edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav
${CLIENT} add secret key bootstrap1 edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi
${CLIENT} add identity bootstrap2 tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN
${CLIENT} add identity bootstrap3 tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU
${CLIENT} add identity bootstrap4 tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv
${CLIENT} add identity bootstrap5 tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv

${CLIENT} activate \
          protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
          with fitness 1 \
          and key edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z

sleep 2

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
