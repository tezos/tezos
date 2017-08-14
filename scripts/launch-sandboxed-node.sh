#!/usr/bin/env bash


script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
export TZPATH="$(dirname $script_dir)"

. $TZPATH/test/test_utils.sh

start_sandboxed_node
sleep 3

activate_alpha |& sed 's/^/## /' > /dev/stderr

trap - EXIT

display_aliases

echo | sed 's/^/## /' 1>&2 <<EOF

  Successfully launched a sandboxed node.

  Run 'tezos-client' to communicate with the sandboxed node.
  Run 'tezos-sandbox-stop' to stop the node and remove the sandbox data.

EOF
