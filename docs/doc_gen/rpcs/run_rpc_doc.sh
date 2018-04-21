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
usage="$docgen_dir/usage.rst"

$rpc_doc < $usage | sed -e 's|/chains/main/blocks/head/|.../<block_id>/|g'
