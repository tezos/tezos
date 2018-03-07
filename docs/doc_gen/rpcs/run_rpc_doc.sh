#!/bin/bash

set -e
set -o pipefail

#**************************************************************************#
#*                                                                        *#
#*    Copyright (c) 2014 - 2018.                                          *#
#*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *#
#*                                                                        *#
#*    All rights reserved. No warranty, explicit or implicit, provided.   *#
#*                                                                        *#
#**************************************************************************#

docgen_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && echo "$(pwd -P)")"

rpc_doc="../_build/default/docs/doc_gen/rpcs/rpc_doc.exe"

tezos_sandboxed_node="${1:-$docgen_dir/../../../src/bin_node/tezos-sandboxed-node.sh}"
tezos_init_sandboxed_client="${3:-$docgen_dir/../../../src/bin_client/tezos-init-sandboxed-client.sh}"

local_node="${2:-$docgen_dir/../../../_build/default/src/bin_node/main.exe}"
local_client="${2:-$docgen_dir/../../../_build/default/src/bin_client/main_client.exe}"

sandbox_file="$docgen_dir/sandbox.json"
usage="$docgen_dir/usage.rst"

source $tezos_sandboxed_node
source $tezos_init_sandboxed_client

start_node() {
    local id=${1:-1}
    start_sandboxed_node $id
    init_sandboxed_client $id
    wait_for_the_node_to_be_ready
    add_sandboxed_bootstrap_identities
    client_instances+=("$client")
    export "client$id=$client"
}

cleanup() {
    set -e
    cleanup_nodes
    cleanup_clients
}
trap cleanup EXIT INT

# Default to 7 to avoid potentially running nodes
# TODO : get first available port
start_node 7 >&2

activate_alpha >&2

sleep 2

$rpc_doc $rpc < $usage | sed 's|/blocks/head/|/blocks/<block_id>/|g'
