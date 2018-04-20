(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Protocol Signature Instance *)

type block_header_data = Alpha_context.Block_header.protocol_data
type block_header = Alpha_context.Block_header.t = {
  shell: Block_header.shell_header ;
  protocol_data: block_header_data ;
}

let block_header_data_encoding = Alpha_context.Block_header.protocol_data_encoding

type operation_data = Alpha_context.Operation.protocol_data
type operation = Alpha_context.Operation.t = {
  shell: Operation.shell_header ;
  protocol_data: operation_data ;
}

let operation_data_encoding = Alpha_context.Operation.protocol_data_encoding

let acceptable_passes = Alpha_context.Operation.acceptable_passes

let max_block_length =
  Alpha_context.Block_header.max_header_length

let validation_passes =
  Updater.[ { max_size = 32 * 1024 ; max_op = Some 32 } ; (* 32kB FIXME *)
            { max_size = 32 * 1024 ; max_op = None } ; (* 32kB FIXME *)
            { max_size = 32 * 1024 ; (* 32kB FIXME *)
              max_op = Some Alpha_context.Constants.max_revelations_per_block } ;
            { max_size = 512 * 1024 ; max_op = None } ] (* 512kB *)

let rpc_services = Services_registration.get_rpc_services ()

type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t ;
      baker : Alpha_context.public_key_hash ;
    }
  | Partial_construction of {
      predecessor : Block_hash.t ;
    }
  | Full_construction of {
      predecessor : Block_hash.t ;
      protocol_data : Alpha_context.Block_header.contents ;
      baker : Alpha_context.public_key_hash ;
    }

type validation_state =
  { mode : validation_mode ;
    ctxt : Alpha_context.t ;
    op_count : int ;
    deposit : Alpha_context.Tez.t ;
  }

let current_context { ctxt ; _ } =
  return (Alpha_context.finalize ctxt).context

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    _block_header =
  (* TODO: decide what properties should be checked *)
  return ()

let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:pred_timestamp
    ~predecessor_fitness:pred_fitness
    (block_header : Alpha_context.Block_header.t) =
  let level = block_header.shell.level in
  let fitness = pred_fitness in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.prepare ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  Apply.begin_application
    ctxt block_header pred_timestamp >>=? fun (ctxt, baker, deposit) ->
  let mode = Application { block_header ; baker = Signature.Public_key.hash baker } in
  return { mode ; ctxt ; op_count = 0 ; deposit }

let begin_construction
    ~predecessor_context:ctxt
    ~predecessor_timestamp:pred_timestamp
    ~predecessor_level:pred_level
    ~predecessor_fitness:pred_fitness
    ~predecessor
    ~timestamp
    ?(protocol_data : block_header_data option)
    () =
  let level = Int32.succ pred_level in
  let fitness = pred_fitness in
  Alpha_context.prepare ~timestamp ~level ~fitness ctxt >>=? fun ctxt ->
  begin
    match protocol_data with
    | None ->
        Apply.begin_partial_construction ctxt >>=? fun ctxt ->
        let mode = Partial_construction { predecessor } in
        return (mode, ctxt, Alpha_context.Tez.zero)
    | Some proto_header ->
        Apply.begin_full_construction
          ctxt pred_timestamp
          proto_header.contents >>=? fun (ctxt, protocol_data, baker, deposit) ->
        let mode =
          let baker = Signature.Public_key.hash baker in
          Full_construction { predecessor ; baker ; protocol_data } in
        return (mode, ctxt, deposit)
  end >>=? fun (mode, ctxt, deposit) ->
  return { mode ; ctxt ; op_count = 0 ; deposit }

let apply_operation ({ mode ; ctxt ; op_count ; _ } as data) operation =
  let predecessor =
    match mode with
    | Partial_construction { predecessor }
    | Application
        { block_header = { shell = { predecessor ; _ } ; _ } ; _ }
    | Full_construction { predecessor ; _ } ->
        predecessor in
  Apply.apply_operation ctxt Optimized predecessor
    (Alpha_context.Operation.hash operation) operation >>=? fun (ctxt, _) ->
  let op_count = op_count + 1 in
  return { data with ctxt ; op_count }

let finalize_block { mode ; ctxt ; op_count ; deposit = _ } =
  match mode with
  | Partial_construction _ ->
      let ctxt = Alpha_context.finalize ctxt in
      return ctxt
  | Application
      { baker ;  block_header = { protocol_data = { contents = protocol_data ; _ } ; _ } }
  | Full_construction { protocol_data ; baker ; _ } ->
      Apply.finalize_application ctxt protocol_data baker >>=? fun ctxt ->
      let { level ; _ } : Alpha_context.Level.t =
        Alpha_context. Level.current ctxt in
      let priority = protocol_data.priority in
      let level = Alpha_context.Raw_level.to_int32 level in
      let fitness = Alpha_context.Fitness.current ctxt in
      let commit_message =
        Format.asprintf
          "lvl %ld, fit %Ld, prio %d, %d ops"
          level fitness priority op_count in
      let ctxt = Alpha_context.finalize ~commit_message ctxt in
      return ctxt

let compare_operations op1 op2 =
  Apply.compare_operations op1 op2

let init ctxt block_header =
  let level = block_header.Block_header.level in
  let fitness = block_header.fitness in
  let timestamp = block_header.timestamp in
  Alpha_context.prepare_first_block
    ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  return (Alpha_context.finalize ctxt)
