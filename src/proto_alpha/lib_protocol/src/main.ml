(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Protocol Signature Instance *)

type operation = Alpha_context.operation

let parse_operation _hash op = Alpha_context.Operation.parse op
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
      protocol_data : Alpha_context.Block_header.protocol_data ;
      baker : Alpha_context.public_key_hash ;
    }

type validation_state =
  { mode : validation_mode ;
    ctxt : Alpha_context.t ;
    op_count : int ;
    deposit : Alpha_context.Tez.t ;
    fees : Alpha_context.Tez.t ;
    rewards : Alpha_context.Tez.t ;
  }

let current_context { ctxt ; _ } =
  return (Alpha_context.finalize ctxt).context

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    raw_block =
  Lwt.return (Alpha_context.Block_header.parse raw_block) >>=? fun _ ->
  (* TODO: decide what other properties should be checked *)
  return ()

let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:pred_timestamp
    ~predecessor_fitness:pred_fitness
    raw_block =
  Lwt.return (Alpha_context.Block_header.parse raw_block) >>=? fun block_header ->
  let level = block_header.shell.level in
  let fitness = pred_fitness in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.init ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  Apply.begin_application
    ctxt block_header pred_timestamp >>=? fun (ctxt, baker, deposit) ->
  let mode = Application { block_header ; baker = Ed25519.Public_key.hash baker } in
  return { mode ; ctxt ; op_count = 0 ; deposit ;
           fees = Alpha_context.Tez.zero ;
           rewards = Alpha_context.Tez.zero }

let begin_construction
    ~predecessor_context:ctxt
    ~predecessor_timestamp:pred_timestamp
    ~predecessor_level:pred_level
    ~predecessor_fitness:pred_fitness
    ~predecessor
    ~timestamp
    ?protocol_data
    () =
  let level = Int32.succ pred_level in
  let fitness = pred_fitness in
  Alpha_context.init ~timestamp ~level ~fitness ctxt >>=? fun ctxt ->
  begin
    match protocol_data with
    | None ->
        Apply.begin_partial_construction ctxt >>=? fun ctxt ->
        let mode = Partial_construction { predecessor } in
        return (mode, ctxt, Alpha_context.Tez.zero)
    | Some proto_header ->
        Apply.begin_full_construction
          ctxt pred_timestamp
          proto_header >>=? fun (ctxt, protocol_data, baker, deposit) ->
        let mode =
          let baker = Ed25519.Public_key.hash baker in
          Full_construction { predecessor ; baker ; protocol_data } in
        return (mode, ctxt, deposit)
  end >>=? fun (mode, ctxt, deposit) ->
  return { mode ; ctxt ; op_count = 0 ; deposit ;
           fees = Alpha_context.Tez.zero ;
           rewards = Alpha_context.Tez.zero  }

let apply_operation ({ mode ; ctxt ; op_count ; _ } as data) operation =
  let pred_block, block_prio, baker =
    match mode with
    | Partial_construction { predecessor } ->
        predecessor, 0, None
    | Application
        { baker ;  block_header = { shell = { predecessor ; _ } ;
                                    protocol_data ; _ } }
    | Full_construction { predecessor ; protocol_data ; baker } ->
        predecessor,
        protocol_data.priority,
        Some baker in
  Apply.apply_operation ctxt baker pred_block block_prio
    (Alpha_context.Operation.hash operation) operation
  >>=? fun (ctxt, _contracts, _ignored_script_error, fees, rewards) ->
  let op_count = op_count + 1 in
  Lwt.return Alpha_context.Tez.(fees >>? (+?) data.fees) >>=? fun fees ->
  Lwt.return Alpha_context.Tez.(rewards >>? (+?) data.rewards) >>=? fun rewards ->
  return { data with ctxt ; op_count ; fees ; rewards }

let finalize_block { mode ; ctxt ; op_count ; deposit ; fees ; rewards } =
  match mode with
  | Partial_construction _ ->
      let ctxt = Alpha_context.finalize ctxt in
      return ctxt
  | Application
      { baker ;  block_header = { protocol_data ; _ } }
  | Full_construction { protocol_data ; baker ; _ } ->
      Apply.finalize_application
        ctxt protocol_data baker deposit fees rewards >>=? fun ctxt ->
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

let configure_sandbox = Alpha_context.configure_sandbox
