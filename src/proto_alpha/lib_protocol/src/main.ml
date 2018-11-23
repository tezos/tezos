(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Tezos Protocol Implementation - Protocol Signature Instance *)

type block_header_data = Alpha_context.Block_header.protocol_data
type block_header = Alpha_context.Block_header.t = {
  shell: Block_header.shell_header ;
  protocol_data: block_header_data ;
}

let block_header_data_encoding = Alpha_context.Block_header.protocol_data_encoding

type block_header_metadata = Apply_results.block_metadata
let block_header_metadata_encoding = Apply_results.block_metadata_encoding

type operation_data = Alpha_context.packed_protocol_data =
  | Operation_data : 'kind Alpha_context.Operation.protocol_data -> operation_data
let operation_data_encoding = Alpha_context.Operation.protocol_data_encoding

type operation_receipt = Apply_results.packed_operation_metadata =
  | Operation_metadata : 'kind Apply_results.operation_metadata -> operation_receipt
  | No_operation_metadata: operation_receipt
let operation_receipt_encoding =
  Apply_results.operation_metadata_encoding

let operation_data_and_receipt_encoding =
  Apply_results.operation_data_and_metadata_encoding

type operation = Alpha_context.packed_operation = {
  shell: Operation.shell_header ;
  protocol_data: operation_data ;
}


let acceptable_passes = Alpha_context.Operation.acceptable_passes

let max_block_length =
  Alpha_context.Block_header.max_header_length

let max_operation_data_length =
  Alpha_context.Constants.max_operation_data_length

let validation_passes =
  let max_anonymous_operations =
    Alpha_context.Constants.max_revelations_per_block +
    (* allow 100 wallet activations or denunciations per block *) 100 in
  Updater.[ { max_size = 32 * 1024 ; max_op = Some 32 } ; (* 32 endorsements *)
            { max_size = 32 * 1024 ; max_op = None } ; (* 32k of voting operations *)
            { max_size = max_anonymous_operations * 1024 ;
              max_op = Some max_anonymous_operations } ;
            { max_size = 512 * 1024 ; max_op = None } ] (* 512kB *)

let rpc_services =
  Alpha_services.register () ;
  Services_registration.get_rpc_services ()

type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t ;
      baker : Alpha_context.public_key_hash ;
    }
  | Partial_application of {
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
    chain_id : Chain_id.t ;
    ctxt : Alpha_context.t ;
    op_count : int ;
  }

let current_context { ctxt ; _ } =
  return (Alpha_context.finalize ctxt).context

let begin_partial_application
    ~chain_id
    ~ancestor_context:ctxt
    ~predecessor_timestamp
    ~predecessor_fitness
    (block_header : Alpha_context.Block_header.t) =
  let level = block_header.shell.level in
  let fitness = predecessor_fitness in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.prepare ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  Apply.begin_application
    ctxt chain_id block_header predecessor_timestamp >>=? fun (ctxt, baker) ->
  let mode =
    Partial_application
      { block_header ; baker = Signature.Public_key.hash baker } in
  return { mode ; chain_id ; ctxt ; op_count = 0 }

let begin_application
    ~chain_id
    ~predecessor_context:ctxt
    ~predecessor_timestamp
    ~predecessor_fitness
    (block_header : Alpha_context.Block_header.t) =
  let level = block_header.shell.level in
  let fitness = predecessor_fitness in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.prepare ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  Apply.begin_application
    ctxt chain_id block_header predecessor_timestamp >>=? fun (ctxt, baker) ->
  let mode = Application { block_header ; baker = Signature.Public_key.hash baker } in
  return { mode ; chain_id ; ctxt ; op_count = 0 }

let begin_construction
    ~chain_id
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
        return (mode, ctxt)
    | Some proto_header ->
        Apply.begin_full_construction
          ctxt pred_timestamp
          proto_header.contents >>=? fun (ctxt, protocol_data, baker) ->
        let mode =
          let baker = Signature.Public_key.hash baker in
          Full_construction { predecessor ; baker ; protocol_data } in
        return (mode, ctxt)
  end >>=? fun (mode, ctxt) ->
  return { mode ; chain_id ; ctxt ; op_count = 0 }

let apply_operation
    ({ mode ; chain_id ; ctxt ; op_count ; _ } as data)
    (operation : Alpha_context.packed_operation) =
  match mode with
  | Partial_application _ when
      not (List.exists
             (Compare.Int.equal 0)
             (Alpha_context.Operation.acceptable_passes operation)) ->
      (* Multipass validation only considers operations in pass 0. *)
      let op_count = op_count + 1 in
      return ({ data with ctxt ; op_count }, No_operation_metadata)
  | _ ->
      let { shell ; protocol_data = Operation_data protocol_data } = operation in
      let operation : _ Alpha_context.operation = { shell ; protocol_data } in
      let predecessor, baker =
        match mode with
        | Partial_application
            { block_header = { shell = { predecessor ; _ } ; _ } ; baker }
        | Application
            { block_header = { shell = { predecessor ; _ } ; _ } ; baker }
        | Full_construction { predecessor ; baker ; _ }
          -> predecessor, baker
        | Partial_construction { predecessor }
          -> predecessor, Signature.Public_key_hash.zero
      in
      let partial =
        match mode with
        | Partial_construction _ -> true
        | Application _
        | Full_construction _
        | Partial_application _ -> false in
      Apply.apply_operation ~partial ctxt chain_id Optimized predecessor baker
        (Alpha_context.Operation.hash operation)
        operation >>=? fun (ctxt, result) ->
      let op_count = op_count + 1 in
      return ({ data with ctxt ; op_count }, Operation_metadata result)

let finalize_block { mode ; ctxt ; op_count } =
  match mode with
  | Partial_construction _ ->
      let level = Alpha_context.Level.current ctxt in
      Alpha_context.Vote.get_current_period_kind ctxt >>=? fun voting_period_kind ->
      let baker = Signature.Public_key_hash.zero in
      Signature.Public_key_hash.Map.fold
        (fun delegate deposit ctxt ->
           ctxt >>=? fun ctxt ->
           Alpha_context.Delegate.freeze_deposit ctxt delegate deposit)
        (Alpha_context.get_deposits ctxt)
        (return ctxt) >>=? fun ctxt ->
      let ctxt = Alpha_context.finalize ctxt in
      return (ctxt, Apply_results.{ baker ;
                                    level ;
                                    voting_period_kind ;
                                    nonce_hash = None ;
                                    consumed_gas = Z.zero ;
                                    deactivated = [];
                                    balance_updates = []})
  | Partial_application { baker ; _ } ->
      let level = Alpha_context. Level.current ctxt in
      Alpha_context.Vote.get_current_period_kind ctxt >>=? fun voting_period_kind ->
      let ctxt = Alpha_context.finalize ctxt in
      return (ctxt, Apply_results.{ baker ;
                                    level ;
                                    voting_period_kind ;
                                    nonce_hash = None ;
                                    consumed_gas = Z.zero ;
                                    deactivated = [];
                                    balance_updates = []})
  | Application
      { baker ;  block_header = { protocol_data = { contents = protocol_data ; _ } ; _ } }
  | Full_construction { protocol_data ; baker ; _ } ->
      Apply.finalize_application ctxt protocol_data baker >>=? fun (ctxt, receipt) ->
      let level = Alpha_context.Level.current ctxt in
      let priority = protocol_data.priority in
      let raw_level = Alpha_context.Raw_level.to_int32 level.level in
      let fitness = Alpha_context.Fitness.current ctxt in
      let commit_message =
        Format.asprintf
          "lvl %ld, fit %Ld, prio %d, %d ops"
          raw_level fitness priority op_count in
      let ctxt = Alpha_context.finalize ~commit_message ctxt in
      return (ctxt, receipt)

let compare_operations op1 op2 =
  let open Alpha_context in
  let Operation_data op1 = op1.protocol_data in
  let Operation_data op2 = op2.protocol_data in
  match op1.contents, op2.contents with
  | Single (Endorsement _), Single (Endorsement _) -> 0
  | _, Single (Endorsement _) -> 1
  | Single (Endorsement _), _ -> -1

  | Single (Seed_nonce_revelation _), Single (Seed_nonce_revelation _) -> 0
  | _, Single (Seed_nonce_revelation _) -> 1
  | Single (Seed_nonce_revelation _), _ -> -1

  | Single (Double_endorsement_evidence _), Single (Double_endorsement_evidence _) -> 0
  | _, Single (Double_endorsement_evidence _) -> 1
  | Single (Double_endorsement_evidence _), _ -> -1

  | Single (Double_baking_evidence _), Single (Double_baking_evidence _) -> 0
  | _, Single (Double_baking_evidence _) -> 1
  | Single (Double_baking_evidence _), _ -> -1

  | Single (Activate_account _), Single (Activate_account _) -> 0
  | _, Single (Activate_account _) -> 1
  | Single (Activate_account _), _ -> -1

  | Single (Proposals _), Single (Proposals _) -> 0
  | _, Single (Proposals _) -> 1
  | Single (Proposals _), _ -> -1

  | Single (Ballot _), Single (Ballot _) -> 0
  | _, Single (Ballot _) -> 1
  | Single (Ballot _), _ -> -1

  (* Manager operations with smaller counter are pre-validated first. *)
  | Single (Manager_operation op1), Single (Manager_operation op2) ->
      Z.compare op1.counter op2.counter
  | Cons (Manager_operation op1, _), Single (Manager_operation op2) ->
      Z.compare op1.counter op2.counter
  | Single (Manager_operation op1), Cons (Manager_operation op2, _) ->
      Z.compare op1.counter op2.counter
  | Cons (Manager_operation op1, _), Cons (Manager_operation op2, _) ->
      Z.compare op1.counter op2.counter

let init ctxt block_header =
  let level = block_header.Block_header.level in
  let fitness = block_header.fitness in
  let timestamp = block_header.timestamp in
  let typecheck (ctxt:Alpha_context.context) (script:Alpha_context.Script.t) =
    Script_ir_translator.parse_script ctxt script >>=? fun (_ex_script, ctxt) ->
    return ctxt
  in
  Alpha_context.prepare_first_block
    ~typecheck
    ~level ~timestamp ~fitness ctxt >>=? fun ctxt ->
  return (Alpha_context.finalize ctxt)
