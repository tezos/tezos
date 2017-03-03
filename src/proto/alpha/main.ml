(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Protocol Signature Instance *)

type operation = Tezos_context.operation

let parse_operation = Tezos_context.Operation.parse

let max_operation_data_length =
  Tezos_context.Operation.max_operation_data_length

type block =
  { header : Tezos_context.Block.header ;
    pred_timestamp : Time.t }

let parse_block raw_header pred_timestamp =
  Tezos_context.Block.parse_header raw_header >>? fun header ->
  Ok { header ; pred_timestamp }

let max_number_of_operations =
  Tezos_context.Constants.max_number_of_operations

let max_block_length =
  Tezos_context.Block.max_header_length

let rpc_services = Services_registration.rpc_services

let apply ctxt block ops =
  Apply.apply ctxt true block.header block.pred_timestamp ops

let preapply = Apply.preapply

let configure_sandbox = Tezos_context.configure_sandbox
