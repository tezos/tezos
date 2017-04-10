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

let max_number_of_operations =
  Tezos_context.Constants.max_number_of_operations

let max_block_length =
  Tezos_context.Block.max_header_length

let rpc_services = Services_registration.rpc_services

type validation_mode =
  | Application of Tezos_context.Block.header * Tezos_context.public_key_hash
  | Construction of { pred_block : Block_hash.t ; timestamp : Time.t }

type validation_state =
  { mode : validation_mode ;
    ctxt : Tezos_context.t ;
    op_count : int }

let current_context { ctxt } =
  Tezos_context.finalize ctxt

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    raw_block =
  Lwt.return (Tezos_context.Block.parse_header raw_block) >>=? fun _ ->
  (* TODO: decide what other properties should be checked *)
  return ()

let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:pred_timestamp
    raw_block =
  Lwt.return (Tezos_context.Block.parse_header raw_block) >>=? fun header ->
  Tezos_context.init ctxt >>=? fun ctxt ->
  Apply.begin_application ctxt header pred_timestamp >>=? fun (ctxt, miner) ->
  let mode = Application (header, miner) in
  return { mode ; ctxt ; op_count = 0 }

let begin_construction
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor:pred_block
    ~timestamp =
  let mode = Construction { pred_block ; timestamp } in
  Tezos_context.init ctxt >>=? fun ctxt ->
  Apply.begin_construction ctxt >>=? fun ctxt ->
  return { mode ; ctxt ; op_count = 0 }

let apply_operation ({ mode ; ctxt ; op_count } as data) operation =
  let pred_block, block_prio, miner_contract =
    match mode with
    | Construction { pred_block } ->
        pred_block, 0l, None
    | Application (block, delegate) ->
        block.shell.predecessor,
        block.proto.mining_slot.priority,
        Some (Tezos_context.Contract.default_contract delegate) in
  Apply.apply_operation
    ctxt miner_contract pred_block block_prio operation
  >>=? fun (ctxt, _contracts, _ignored_script_error) ->
  let op_count = op_count + 1 in
  return { data with ctxt ; op_count }

let finalize_block { mode ; ctxt ; op_count } = match mode with
  | Construction _ ->
      Tezos_context.finalize ctxt >>=? fun ctxt ->
      return ctxt
  | Application (block, miner) ->
      Apply.finalize_application
        ctxt block miner op_count >>=? fun (commit_message, ctxt) ->
      Tezos_context.finalize ~commit_message ctxt >>=? fun ctxt ->
      return ctxt

let compare_operations op1 op2 =
  Apply.compare_operations op1 op2

let configure_sandbox = Tezos_context.configure_sandbox
