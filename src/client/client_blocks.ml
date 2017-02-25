(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let get_block_hash cctxt = function
  | `Hash hash -> Lwt.return hash
  | `Genesis | `Head _ | `Test_head _ as block ->
      Client_node_rpcs.Blocks.hash cctxt block
  | `Prevalidation -> Client_node_rpcs.Blocks.hash cctxt (`Head 0)
  | `Test_prevalidation -> Client_node_rpcs.Blocks.hash cctxt (`Test_head 0)


let get_block_info cctxt block =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | b -> b in
  Client_node_rpcs.Blocks.info cctxt block
