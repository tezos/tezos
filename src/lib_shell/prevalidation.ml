(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Preapply_result
open Validation_errors

let rec apply_operations apply_operation state r max_ops ~sort ops =
  Lwt_list.fold_left_s
    (fun (state, max_ops, r) (hash, op, parsed_op) ->
       apply_operation state max_ops op parsed_op >>= function
       | Ok (state, _metadata) ->
           let applied = (hash, op) :: r.applied in
           Lwt.return (state, max_ops - 1, { r with applied })
       | Error errors ->
           match classify_errors errors with
           | `Branch ->
               let branch_refused =
                 Operation_hash.Map.add hash (op, errors) r.branch_refused in
               Lwt.return (state, max_ops, { r with branch_refused })
           | `Permanent ->
               let refused =
                 Operation_hash.Map.add hash (op, errors) r.refused in
               Lwt.return (state, max_ops, { r with refused })
           | `Temporary ->
               let branch_delayed =
                 Operation_hash.Map.add hash (op, errors) r.branch_delayed in
               Lwt.return (state, max_ops, { r with branch_delayed }))
    (state, max_ops, r)
    ops >>= fun (state, max_ops, r) ->
  match r.applied with
  | _ :: _ when sort ->
      let rechecked_operations =
        List.filter
          (fun (hash, _, _) -> Operation_hash.Map.mem hash r.branch_delayed)
          ops in
      let remaining = List.length rechecked_operations in
      if remaining = 0 || remaining = List.length ops then
        Lwt.return (state, max_ops, r)
      else
        apply_operations apply_operation state r max_ops ~sort rechecked_operations
  | _ ->
      Lwt.return (state, max_ops, r)

type prevalidation_state =
    State : { proto : 'a proto ; state : 'a ;
              max_number_of_operations : int }
    -> prevalidation_state

and 'a proto =
  (module Registered_protocol.T with type P.validation_state = 'a)

let start_prevalidation
    ?protocol_data
    ~predecessor ~timestamp () =
  let { Block_header.shell =
          { fitness = predecessor_fitness ;
            timestamp = predecessor_timestamp ;
            level = predecessor_level } } =
    State.Block.header predecessor in
  State.Block.context predecessor >>= fun predecessor_context ->
  Context.get_protocol predecessor_context >>= fun protocol ->
  let predecessor = State.Block.hash predecessor in
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
  Context.reset_test_chain
    predecessor_context predecessor
    timestamp >>= fun predecessor_context ->
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
    ~predecessor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    ~predecessor_level
    ~predecessor
    ~timestamp
    ?protocol_data
    ()
  >>=? fun state ->
  (* FIXME arbitrary value, to be customisable *)
  let max_number_of_operations = 1000 in
  return (State { proto = (module Proto) ; state ;
                  max_number_of_operations })


let prevalidate
    (State { proto = (module Proto) ; state ;
             max_number_of_operations })
    ~sort (ops : (Operation_hash.t * Operation.t) list)=
  let ops =
    List.map
      (fun (h, op) ->
         let parsed_op =
           match Data_encoding.Binary.of_bytes
                   Proto.operation_data_encoding
                   op.Operation.proto with
           | None -> error Parse_error
           | Some protocol_data ->
               Ok ({ shell = op.shell ; protocol_data } : Proto.operation) in
         (h, op, parsed_op))
      ops in
  let invalid_ops =
    List.filter_map
      (fun (h, op, parsed_op) -> match parsed_op with
         | Ok _ -> None
         | Error err -> Some (h, op, err)) ops
  and parsed_ops =
    List.filter_map
      (fun (h, op, parsed_op) -> match parsed_op with
         | Ok parsed_op -> Some (h, op, parsed_op)
         | Error _ -> None) ops in
  let sorted_ops =
    if sort then
      let compare (_, _, op1) (_, _, op2) = Proto.compare_operations op1 op2 in
      List.sort compare parsed_ops
    else parsed_ops in
  let apply_operation state max_ops op (parse_op) =
    let size = Data_encoding.Binary.length Operation.encoding op in
    if max_ops <= 0 then
      fail Too_many_operations
    else if size > Proto.max_operation_data_length then
      fail (Oversized_operation { size ; max = Proto.max_operation_data_length })
    else
      Proto.apply_operation state parse_op >>=? fun (state, receipt) ->
      return (state, receipt) in
  apply_operations
    apply_operation
    state Preapply_result.empty max_number_of_operations
    ~sort sorted_ops >>= fun (state, max_number_of_operations, r) ->
  let r =
    { r with
      applied = List.rev r.applied ;
      branch_refused =
        List.fold_left
          (fun map (h, op, err) -> Operation_hash.Map.add h (op, err) map)
          r.branch_refused invalid_ops } in
  Lwt.return (State { proto = (module Proto) ; state ;
                      max_number_of_operations },
              r)

let end_prevalidation (State { proto = (module Proto) ; state }) =
  Proto.finalize_block state >>=? fun (result, _metadata) ->
  return result

let preapply ~predecessor ~timestamp ~protocol_data ~sort_operations:sort ops =
  start_prevalidation
    ~protocol_data ~predecessor ~timestamp () >>=? fun validation_state ->
  let ops = List.map (List.map (fun x -> Operation.hash x, x)) ops in
  Lwt_list.fold_left_s
    (fun (validation_state, rs) ops ->
       prevalidate
         validation_state ~sort ops >>= fun (validation_state, r) ->
       Lwt.return (validation_state, rs @ [r]))
    (validation_state, []) ops >>= fun (validation_state, rs) ->
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map
         (fun r ->
            Operation_list_hash.compute
              (List.map fst r.Preapply_result.applied))
         rs) in
  end_prevalidation validation_state >>=? fun validation_result ->
  let pred_shell_header = State.Block.shell_header predecessor in
  let level = Int32.succ pred_shell_header.level in
  Block_validator.may_patch_protocol
    ~level validation_result >>=? fun { fitness ; context ; message } ->
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
    validation_passes = List.length rs ;
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
  Context.commit ?message ~time:timestamp context >>= fun context ->
  return ({ shell_header with context }, rs)
