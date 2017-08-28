#! /usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

source $script_dir/node_lib.inc.sh

if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
    echo "Small script to launch local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: $0 <id>"
    echo "  where <id> should be an integer between 1 and 9."
    exit 1
fi

cleanup () {
    set +e
    echo Cleaning up...
    cleanup_nodes
}
trap cleanup EXIT INT

start_sandboxed_node "$@"
wait $node_pids
