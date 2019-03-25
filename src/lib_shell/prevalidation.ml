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

open Validation_errors

module type T = sig

  module Proto: Registered_protocol.T

  type t

  type operation = private {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }
  val compare: operation -> operation -> int

  val parse: Operation.t -> operation tzresult

  (** Creates a new prevalidation context w.r.t. the protocol associate to the
      predecessor block . When ?protocol_data is passed to this function, it will
      be used to create the new block *)
  val create :
    ?protocol_data: MBytes.t ->
    predecessor: State.Block.t ->
    timestamp: Time.Protocol.t ->
    unit -> t tzresult Lwt.t

  type result =
    | Applied of t * Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Duplicate
    | Outdated

  val apply_operation: t -> operation -> result Lwt.t

  type status = {
    applied_operations : (operation * Proto.operation_receipt) list ;
    block_result : Tezos_protocol_environment_shell.validation_result ;
    block_metadata : Proto.block_header_metadata ;
  }

  val status: t -> status tzresult Lwt.t

  val pp_result: Format.formatter -> result -> unit
end

module Make(Proto : Registered_protocol.T) : T with module Proto = Proto = struct

  module Proto = Proto

  type operation = {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }


  type t =
    { state : Proto.validation_state ;
      applied : (operation * Proto.operation_receipt) list ;
      live_blocks : Block_hash.Set.t ;
      live_operations : Operation_hash.Set.t ;
    }

  type result =
    | Applied of t * Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Duplicate
    | Outdated

  let parse (raw : Operation.t) =
    let hash = Operation.hash raw in
    let size = Data_encoding.Binary.length Operation.encoding raw in
    if size > Proto.max_operation_data_length then
      error
        (Oversized_operation
           { size ; max = Proto.max_operation_data_length })
    else
      match Data_encoding.Binary.of_bytes
              Proto.operation_data_encoding
              raw.Operation.proto with
      | None -> error Parse_error
      | Some protocol_data ->
          ok { hash ; raw ; protocol_data }

  let compare op1 op2 =
    Proto.compare_operations
      { shell = op1.raw.shell ; protocol_data = op1.protocol_data }
      { shell = op2.raw.shell ; protocol_data = op2.protocol_data }

  let create ?protocol_data ~predecessor ~timestamp () =
    (* The prevalidation module receives input from the system byt handles
       protocol values. It translates timestamps here. *)
    let { Block_header.shell =
            { fitness = predecessor_fitness ;
              timestamp = predecessor_timestamp ;
              level = predecessor_level ; _ } ; _ } =
      State.Block.header predecessor in
    State.Block.context predecessor >>= fun predecessor_context ->
    let predecessor_header = State.Block.header predecessor in
    let predecessor_hash = State.Block.hash predecessor in
    Chain_traversal.live_blocks
      predecessor
      (State.Block.max_operations_ttl predecessor)
    >>= fun (live_blocks, live_operations) ->
    Block_validation.update_testchain_status
      predecessor_context predecessor_header
      timestamp >>=? fun predecessor_context ->
    begin
      match protocol_data with
      | None -> return_none
      | Some protocol_data ->
          match
            Data_encoding.Binary.of_bytes
              Proto.block_header_data_encoding
              protocol_data
          with
          | None -> failwith "Invalid block header"
          | Some protocol_data -> return_some protocol_data
    end >>=? fun protocol_data ->
    Proto.begin_construction
      ~chain_id: (State.Block.chain_id predecessor)
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_fitness
      ~predecessor_level
      ~predecessor: predecessor_hash
      ~timestamp
      ?protocol_data
      ()
    >>=? fun state ->
    (* FIXME arbitrary value, to be customisable *)
    return {
      state ;
      applied = [] ;
      live_blocks ;
      live_operations ;
    }

  let apply_operation pv op =
    if Operation_hash.Set.mem op.hash pv.live_operations then
      Lwt.return Outdated
    else
      Proto.apply_operation pv.state
        { shell = op.raw.shell ; protocol_data = op.protocol_data } >|= function
      | Ok (state, receipt) ->
          let pv =
            { state ;
              applied = (op, receipt) :: pv.applied ;
              live_blocks = pv.live_blocks ;
              live_operations = Operation_hash.Set.add op.hash pv.live_operations ;
            } in
          Applied (pv, receipt)
      | Error errors ->
          match classify_errors errors with
          | `Branch -> Branch_refused errors
          | `Permanent -> Refused errors
          | `Temporary -> Branch_delayed errors

  type status = {
    applied_operations : (operation * Proto.operation_receipt) list ;
    block_result : Tezos_protocol_environment_shell.validation_result ;
    block_metadata : Proto.block_header_metadata ;
  }

  let status pv =
    Proto.finalize_block pv.state >>=? fun (block_result, block_metadata) ->
    return {
      block_metadata ;
      block_result ;
      applied_operations = pv.applied ;
    }

  let pp_result ppf =
    let open Format in
    function
    | Applied _ -> pp_print_string ppf "applied"
    | Branch_delayed err -> fprintf ppf "branch delayed (%a)" pp_print_error err
    | Branch_refused err -> fprintf ppf "branch refused (%a)" pp_print_error err
    | Refused err -> fprintf ppf "refused (%a)" pp_print_error err
    | Duplicate -> pp_print_string ppf "duplicate"
    | Outdated -> pp_print_string ppf "outdated"

end

let preapply ~predecessor ~timestamp ~protocol_data operations =
  State.Block.context predecessor >>= fun predecessor_context ->
  Context.get_protocol predecessor_context >>= fun protocol ->
  begin
    match Registered_protocol.get protocol with
    | None ->
        (* FIXME. *)
        (* This should not happen: it should be handled in the validator. *)
        failwith "Prevalidation: missing protocol '%a' for the current block."
          Protocol_hash.pp_short protocol
    | Some protocol ->
        return protocol
  end >>=? fun (module Proto) ->
  let module Prevalidation = Make(Proto) in
  let apply_operation_with_preapply_result preapp t op =
    let open Preapply_result in
    Prevalidation.apply_operation t op >>= function
    | Applied (t, _) ->
        let applied = (op.hash, op.raw) :: preapp.applied in
        Lwt.return ({ preapp with applied }, t)
    | Branch_delayed errors ->
        let branch_delayed =
          Operation_hash.Map.add
            op.hash
            (op.raw, errors)
            preapp.branch_delayed in
        Lwt.return ({ preapp with branch_delayed }, t)
    | Branch_refused errors ->
        let branch_refused =
          Operation_hash.Map.add
            op.hash
            (op.raw, errors)
            preapp.branch_refused in
        Lwt.return ({ preapp with branch_refused }, t)
    | Refused errors ->
        let refused =
          Operation_hash.Map.add
            op.hash
            (op.raw, errors)
            preapp.refused in
        Lwt.return ({ preapp with refused }, t)
    | Duplicate | Outdated -> Lwt.return (preapp, t) in
  Prevalidation.create
    ~protocol_data ~predecessor ~timestamp () >>=? fun validation_state ->
  Lwt_list.fold_left_s
    (fun (acc_validation_result, acc_validation_state) operations ->
       Lwt_list.fold_left_s
         (fun (acc_validation_result, acc_validation_state) op ->
            match Prevalidation.parse op with
            | Error _ ->
                (* FIXME *)
                Lwt.return (acc_validation_result, acc_validation_state)
            | Ok op ->
                apply_operation_with_preapply_result
                  acc_validation_result acc_validation_state op)
         (Preapply_result.empty, acc_validation_state)
         operations
       >>= fun (new_validation_result, new_validation_state) ->
       (* Applied operations are reverted ; revert to the initial ordering *)
       let new_validation_result =
         { new_validation_result with applied = List.rev new_validation_result.applied } in
       Lwt.return (acc_validation_result @ [new_validation_result], new_validation_state)
    ) ([], validation_state) operations
  >>= fun (validation_result_list, validation_state) ->
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map (fun r ->
           Operation_list_hash.compute
             (List.map fst r.Preapply_result.applied)
         ) validation_result_list)
  in
  Prevalidation.status validation_state >>=? fun { block_result ; _ } ->
  let pred_shell_header = State.Block.shell_header predecessor in
  let level = Int32.succ pred_shell_header.level in
  Block_validation.may_patch_protocol
    ~level block_result >>=? fun { fitness ; context ; message ; _ } ->
  State.Block.protocol_hash predecessor >>= fun pred_protocol ->
  Context.get_protocol context >>= fun protocol ->
  let proto_level =
    if Protocol_hash.equal protocol pred_protocol then
      pred_shell_header.proto_level
    else
      ((pred_shell_header.proto_level + 1) mod 256) in
  let shell_header : Block_header.shell_header = {
    level ;
    proto_level ;
    predecessor = State.Block.hash predecessor ;
    timestamp ;
    validation_passes = List.length validation_result_list ;
    operations_hash ;
    fitness ;
    context = Context_hash.zero ; (* place holder *)
  } in
  begin
    if Protocol_hash.equal protocol pred_protocol then
      return (context, message)
    else
      match Registered_protocol.get protocol with
      | None ->
          fail (Block_validator_errors.Unavailable_protocol
                  { block = State.Block.hash predecessor ; protocol })
      | Some (module NewProto) ->
          NewProto.init context shell_header >>=? fun { context ; message ; _ } ->
          return (context, message)
  end >>=? fun (context, message) ->
  Context.hash ?message ~time:timestamp context >>= fun context ->
  return ({ shell_header with context }, validation_result_list)
