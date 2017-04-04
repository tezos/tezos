(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let genesis =
  Block_hash.of_b58check
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let get_block_hash config = function
  | `Hash hash -> return hash
  | `Genesis | `Head _ | `Test_head _ as block ->
      Client_node_rpcs.Blocks.hash config block
  | `Prevalidation -> Client_node_rpcs.Blocks.hash config (`Head 0)
  | `Test_prevalidation -> Client_node_rpcs.Blocks.hash config (`Test_head 0)

let get_block_info config block =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | b -> b in
  Client_node_rpcs.Blocks.info config block
