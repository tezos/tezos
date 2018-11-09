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

type application_result = {
  validation_result: Tezos_protocol_environment_shell.validation_result ;
  block_data: Secp256k1.watermark ;
  ops_metadata: Secp256k1.watermark list list ;
  context_hash: Context_hash.t ;
}

type error +=
  | Failed_to_checkout_context of Context_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"Validator_process.failed_to_checkout_context"
    ~title: "Fail during checkout context"
    ~description: "The context checkout failed using a given hash"
    ~pp:(fun ppf (hash:Context_hash.t) ->
        Format.fprintf ppf
          "@[Failed to checkout the context with hash %a@]"
          Context_hash.pp_short hash)
    Data_encoding.(obj1 (req "hash" Context_hash.encoding))
    (function
      | Failed_to_checkout_context h -> Some h
      | _ -> None)
    (fun h -> Failed_to_checkout_context h)


(** The standard block validation method *)
module SeqValidator = struct

  include Logging.Make (struct let name = "sequential validator process" end)

  type validation_context = {
    context_index : Context.index ;
  }

  type t = validation_context

  let init context_root =
    lwt_log_notice "Intialized" >>= fun _ ->
    Context.init context_root >>= fun context_index ->
    Lwt.return { context_index }

  let close _ =
    lwt_log_notice "Shutting down ..." >>= fun _ ->
    Lwt.return ()

  let get_context index hash =
    Context.checkout index hash >>= function
    | None -> fail (Failed_to_checkout_context hash)
    | Some ctx -> return ctx

  open Block_validator_errors

  let invalid_block block error = Invalid_block { block ; error }

  let apply_block
      validator_process
      (header : Block_header.t)
      operations
      chain_state =
    State.Block.read
      chain_state header.shell.predecessor >>=? fun pred ->
    State.Block.context pred >>= fun pred_context ->
    Context.get_protocol pred_context >>= fun pred_protocol_hash ->
    let hash = Block_header.hash header in
    let chain_id = State.Chain.id chain_state in
    begin
      match Registered_protocol.get pred_protocol_hash with
      | None ->
          fail (Unavailable_protocol { block = hash ;
                                       protocol = pred_protocol_hash })
      | Some p -> return p
    end >>=? fun (module Proto) ->
    let pred_header = State.Block.header pred in
    get_context
      validator_process.context_index
      pred_header.shell.context >>=? fun pred_context ->
    let pred_hash = State.Block.hash pred in
    Context.reset_test_chain
      pred_context pred_hash header.shell.timestamp >>= fun context ->
    let max_operations_ttl = State.Block.max_operations_ttl pred in
    let operation_hashes = List.map (List.map Operation.hash) operations in
    begin
      match
        Data_encoding.Binary.of_bytes
          Proto.block_header_data_encoding
          header.protocol_data with
      | None ->
          fail (invalid_block hash Cannot_parse_block_header)
      | Some protocol_data ->
          return ({ shell = header.shell ; protocol_data } : Proto.block_header)
    end >>=? fun header ->
    mapi2_s (fun pass -> map2_s begin fun op_hash op ->
        match
          Data_encoding.Binary.of_bytes
            Proto.operation_data_encoding
            op.Operation.proto with
        | None ->
            fail (invalid_block hash (Cannot_parse_operation op_hash))
        | Some protocol_data ->
            let op = { Proto.shell = op.shell ; protocol_data } in
            let allowed_pass = Proto.acceptable_passes op in
            fail_unless (List.mem pass allowed_pass)
              (invalid_block hash
                 (Unallowed_pass { operation = op_hash ;
                                   pass ; allowed_pass } )) >>=? fun () ->
            return op
      end)
      operation_hashes
      operations >>=? fun parsed_operations ->
    (* TODO wrap 'proto_error' into 'block_error' *)
    Proto.begin_application
      ~chain_id: chain_id
      ~predecessor_context:context
      ~predecessor_timestamp:pred_header.shell.timestamp
      ~predecessor_fitness:pred_header.shell.fitness
      header >>=? fun state ->
    fold_left_s
      (fun (state, acc) ops ->
         fold_left_s
           (fun (state, acc) op ->
              Proto.apply_operation state op >>=? fun (state, op_metadata) ->
              return (state, op_metadata :: acc))
           (state, []) ops >>=? fun (state, ops_metadata) ->
         return (state, List.rev ops_metadata :: acc))
      (state, []) parsed_operations >>=? fun (state, ops_metadata) ->
    let ops_metadata = List.rev ops_metadata in
    Proto.finalize_block state >>=? fun (validation_result, block_data) ->
    Context.get_protocol validation_result.context >>= fun new_protocol ->
    let expected_proto_level =
      if Protocol_hash.equal new_protocol Proto.hash then
        pred_header.shell.proto_level
      else
        (pred_header.shell.proto_level + 1) mod 256 in
    fail_when (header.shell.proto_level <> expected_proto_level)
      (invalid_block hash @@  Invalid_proto_level {
          found = header.shell.proto_level ;
          expected = expected_proto_level ;
        }) >>=? fun () ->
    fail_when
      Fitness.(validation_result.fitness <> header.shell.fitness)
      (invalid_block hash @@ Invalid_fitness {
          expected = header.shell.fitness ;
          found = validation_result.fitness ;
        }) >>=? fun () ->
    begin
      if Protocol_hash.equal new_protocol Proto.hash then
        return validation_result
      else
        match Registered_protocol.get new_protocol with
        | None ->
            fail (Unavailable_protocol { block = hash ;
                                         protocol = new_protocol })
        | Some (module NewProto) ->
            NewProto.init validation_result.context header.shell
    end >>=? fun validation_result ->
    let max_operations_ttl =
      max 0
        (min
           ((max_operations_ttl)+1)
           validation_result.max_operations_ttl) in
    let validation_result =
      { validation_result with max_operations_ttl } in
    let block_data =
      Data_encoding.Binary.to_bytes_exn
        Proto.block_header_metadata_encoding block_data in
    let ops_metadata =
      List.map
        (List.map
           (Data_encoding.Binary.to_bytes_exn
              Proto.operation_receipt_encoding))
        ops_metadata in
    Context.commit
      ~time:header.shell.timestamp
      ?message:validation_result.message
      validation_result.context >>= fun context_hash ->
    return ({ validation_result ; block_data ;
              ops_metadata ; context_hash })

end

type kind =
  | Internal

type t =
  | Sequential of SeqValidator.t

let init ~context_root = function
  | Internal ->
      SeqValidator.init context_root >>= fun v ->
      Lwt.return (Sequential v)

let close = function
  | Sequential vp -> SeqValidator.close vp

let apply_block = function
  | Sequential vp -> SeqValidator.apply_block vp
