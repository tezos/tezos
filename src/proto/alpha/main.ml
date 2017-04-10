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
  return (Tezos_context.finalize ctxt).context

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
    ~predecessor_fitness:pred_fitness
    raw_block =
  Lwt.return (Tezos_context.Block.parse_header raw_block) >>=? fun header ->
  let level = header.shell.level in
  let fitness = pred_fitness in
  let timestamp = header.shell.timestamp in
  Tezos_context.init ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  Apply.begin_application ctxt header pred_timestamp >>=? fun (ctxt, miner) ->
  let mode = Application (header, miner) in
  return { mode ; ctxt ; op_count = 0 }

let begin_construction
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor_level:pred_level
    ~predecessor_fitness:pred_fitness
    ~predecessor:pred_block
    ~timestamp =
  let mode = Construction { pred_block ; timestamp } in
  let level = Int32.succ pred_level in
  let fitness = pred_fitness in
  Tezos_context.init ~timestamp ~level ~fitness ctxt >>=? fun ctxt ->
  let ctxt = Apply.begin_construction ctxt in
  return { mode ; ctxt ; op_count = 0 }

let apply_operation ({ mode ; ctxt ; op_count } as data) operation =
  let pred_block, block_prio, miner_contract =
    match mode with
    | Construction { pred_block } ->
        pred_block, 0, None
    | Application (block, delegate) ->
        block.shell.predecessor,
        block.proto.priority,
        Some (Tezos_context.Contract.default_contract delegate) in
  Apply.apply_operation
    ctxt miner_contract pred_block block_prio operation
  >>=? fun (ctxt, _contracts, _ignored_script_error) ->
  let op_count = op_count + 1 in
  return { data with ctxt ; op_count }

let finalize_block { mode ; ctxt ; op_count } = match mode with
  | Construction _ ->
      let ctxt = Tezos_context.finalize ctxt in
      return ctxt
  | Application (block, miner) ->
      Apply.finalize_application ctxt block miner >>=? fun ctxt ->
      let { level } : Tezos_context.Level.t =
        Tezos_context. Level.current ctxt in
      let priority = block.proto.priority in
      let level = Tezos_context.Raw_level.to_int32 level in
      let fitness = Tezos_context.Fitness.current ctxt in
      let commit_message =
        Format.asprintf
          "lvl %ld, fit %Ld, prio %d, %d ops"
          level fitness priority op_count in
      let ctxt = Tezos_context.finalize ~commit_message ctxt in
      return ctxt

let compare_operations op1 op2 =
  Apply.compare_operations op1 op2

let configure_sandbox = Tezos_context.configure_sandbox
