#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

usage() {
    echo "Small script to initialize a client to a local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: eval \`$0 <id>\`"
    echo "  where <id> should be an integer between 1 and 9."
}

if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
    usage
    exit 1
fi

id="$1"
shift 1

rpc=$((18730 + id))
client_dir="$(mktemp -td tezos-client-XXXXX)"
client="./tezos-client -base-dir $client_dir -addr 127.0.0.1 -port $rpc"

. "$script_dir/client_lib.inc.sh"

add_sandboxed_bootstrap_identities | sed -e 's/^/## /' 1>&2

cat <<EOF
if type tezos-client-reset >/dev/null 2>&1 ; then tezos-client-reset; fi ;
alias tezos-client="$client" ;
alias tezos-activate-alpha="$client -block genesis activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK with fitness 1 and key dictator" ;
alias tezos-client-reset="rm -rf \"$client_dir\"; unalias tezos-client tezos-activate-alpha tezos-client-reset" ;
trap tezos-client-reset EXIT ;
EOF

(cat | sed -e 's/^/## /') 1>&2 <<EOF

The client is now properly initialized. In the rest of this shell
session, you might now run \`tezos-client\` to communicate with a
tezos node launched with \`launch-sandboxed-node $1\`. For instance:

  tezos-client rpc call blocks/head/protocol

Note: if the current protocol version, as reported by the previous
command, is "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im", you
may have to activate in your "sandboxed network" the same economic
protocol than used by the alphanet by running:

  tezos-activate-alpha

Warning: all the client data will be removed when you close this shell
or if you run this command a second time.

EOF
