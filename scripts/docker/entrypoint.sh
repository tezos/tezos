#!/bin/sh

set -e

bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

: ${BIN_DIR:="/usr/local/bin"}
: ${DATA_DIR:="/var/run/tezos"}

: ${NODE_HOST:="node"}
: ${NODE_RPC_PORT:="8732"}

: ${PROTOCOL:="unspecified-PROTOCOL-variable"}

node="$BIN_DIR/tezos-node"
client="$BIN_DIR/tezos-client"
admin_client="$BIN_DIR/tezos-admin-client"
baker="$BIN_DIR/tezos-baker-$PROTOCOL"
endorser="$BIN_DIR/tezos-endorser-$PROTOCOL"
accuser="$BIN_DIR/tezos-accuser-$PROTOCOL"
signer="$BIN_DIR/tezos-signer"

client_dir="$DATA_DIR/client"
node_dir="$DATA_DIR/node"
node_data_dir="$node_dir/data"

. "$bin_dir/entrypoint.inc.sh"

command=${1:-tezos-node}
shift 1

case $command in
    tezos-node)
        launch_node "$@"
    ;;
    tezos-baker)
        launch_baker "$@"
        ;;
    tezos-baker-test)
        launch_baker_test "$@"
        ;;
    tezos-endorser)
        launch_endorser "$@"
        ;;
    tezos-endorser-test)
        launch_endorser_test "$@"
        ;;
    tezos-accuser)
        launch_accuser "$@"
        ;;
    tezos-accuser-test)
        launch_accuser_test "$@"
        ;;
    tezos-client)
        configure_client
        exec "$client" "$@"
        ;;
    tezos-admin-client)
        configure_client
        exec "$admin_client" "$@"
        ;;
    tezos-signer)
        exec "$signer" "$@"
        ;;
    *)
        cat <<EOF
Available commands:
- tezos-node [args]
- tezos-client [args]
- tezos-baker [keys]
- tezos-baker-test [keys]
- tezos-endorser [keys]
- tezos-endorser-test [keys]
- tezos-signer [args]
EOF
        ;;
esac
