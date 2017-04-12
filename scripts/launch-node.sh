#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
    echo "Small script to launch local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: $0 <id>"
    echo "  where <id> should be an integer between 1 and 9."
    exit 1
fi

id="$1"
shift 1

port=$((19730 + id))
rpc=$((18730 + id))
expected_pow=10
data_dir="$(mktemp -td tezos-node-XXXXX)"
peers="--no-bootstrap-peers $(seq -f '--peer localhost:1973%.f' 1 9) --closed"
node="$src_dir/tezos-node"

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$data_dir"
}
trap cleanup EXIT INT

$node config init \
      --data-dir "$data_dir" \
      --net-addr ":$port" \
      --rpc-addr "[::]:$rpc" \
      --expected-pow "$expected_pow" \
      --connections 2 $peers
$node identity generate "$expected_pow" --data-dir "$data_dir"
$node run --data-dir "$data_dir" "$@"
