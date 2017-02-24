(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Web Interface - version dependent services *)

let contextual_static_files : string OCamlRes.Res.root Protocol_hash.Table.t =
  Protocol_hash.Table.create 7

let register_static_files version root =
  Protocol_hash.Table.add contextual_static_files version root

let find_contextual_static_files version =
  Protocol_hash.Table.find contextual_static_files version

let contextual_services : Node_rpc_services.Blocks.block RPC.directory Protocol_hash.Table.t =
  Protocol_hash.Table.create 7

let register_services version root =
  Protocol_hash.Table.add contextual_services version root

let find_contextual_services version =
  Protocol_hash.Table.find  contextual_services version
