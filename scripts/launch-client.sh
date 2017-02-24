#! /bin/bash

set -e

script_dir="$(dirname "$(readlink -f "$0")")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

function usage() {
    echo "Small script client to a local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: $0 <id> [ bake [id] | endorse [id] | exec ... ]"
    echo "  where <id> should be an integer between 1 and 9."
}

if [ $# -lt 2 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
    usage
    exit 1
fi

id="$1"
cmd="$2"
shift 2

rpc=$((18730 + id))
base_dir="/tmp/tezos-client-$rpc"

client="./tezos-client -base-dir $base_dir -addr 127.0.0.1 -port $rpc"

. "$script_dir/client_lib.inc.sh"

function cmd_bake() {
    wait_for_the_node_to_be_ready
    if [ $# -eq 0 ]; then
        create_identity
        create_account
    fi
    start_baker "$@"
}

function cmd_endorse() {
    wait_for_the_node_to_be_ready
    if [ $# -eq 0 ]; then
        create_account
        endorsement
    fi
    start_endorser "$@"
}

function cmd_exec() {
    $client "$@"
}

function cmd_clear() {
    rm -fr "$base_dir"
}

if [ ! -d "$base_dir" ] && [ "$cmd" != "clear" ]; then
    $client bootstrap
fi

case $cmd in
    bake)
        cmd_bake "$@"
        ;;
    endorse)
        cmd_endorse "$@"
        ;;
    exec)
        cmd_exec "$@"
        ;;
    clear)
        cmd_clear
        ;;
    *)
        usage
        exit 1
        ;;
esac
