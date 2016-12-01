(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Web Interface - version dependent services *)

let contextual_static_files : string OCamlRes.Res.root Protocol_hash_table.t =
  Protocol_hash_table.create 7

let register_static_files version root =
  Protocol_hash_table.add contextual_static_files version root

let find_contextual_static_files version =
  Protocol_hash_table.find contextual_static_files version

let contextual_services : Node_rpc_services.Blocks.block RPC.directory Protocol_hash_table.t =
  Protocol_hash_table.create 7

let register_services version root =
  Protocol_hash_table.add contextual_services version root

let find_contextual_services version =
  Protocol_hash_table.find  contextual_services version
