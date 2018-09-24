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
    State : { proto : ('state, 'operation_data) proto ; state : 'state ;
              max_number_of_operations : int ;
              new_operation_input : ([ `Applied | `Refused | `Branch_refused | `Branch_delayed ] *
                                     Operation.shell_header * 'operation_data) Lwt_watcher.input ;
            }
    -> prevalidation_state

and ('state, 'operation_data) proto =
  (module Registered_protocol.T
    with type P.validation_state = 'state
     and type P.operation_data = 'operation_data )

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
  let predecessor_hash = State.Block.hash predecessor in
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
    predecessor_context predecessor_hash
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
  let max_number_of_operations = 2000 in
  let new_operation_input = Lwt_watcher.create_input () in
  return (State { proto = (module Proto) ; state ;
                  max_number_of_operations ;
                  new_operation_input ;
                })

let prevalidate
    (State { proto = (module Proto) ; state ;
             max_number_of_operations ; new_operation_input })
    ~sort (ops : (Operation_hash.t * Operation.t) list) =
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
                      max_number_of_operations ; new_operation_input },
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
  Context.hash ?message ~time:timestamp context >>= fun context ->
  return ({ shell_header with context }, rs)

let notify_operation (State { proto = (module Proto) ; new_operation_input ; }) result =
  let { applied ; refused ; branch_refused ; branch_delayed } = result in
  (* Notify new opperations *)
  let map_op kind { Operation.shell ; proto } =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding
        proto in
    kind, shell, protocol_data in
  let fold_op kind _k (op, _error) acc = map_op kind op :: acc in
  let applied = List.map (map_op `Applied) (List.map snd applied) in
  let refused = Operation_hash.Map.fold (fold_op `Refused) refused [] in
  let branch_refused = Operation_hash.Map.fold (fold_op `Branch_refused) branch_refused [] in
  let branch_delayed = Operation_hash.Map.fold (fold_op `Branch_delayed) branch_delayed [] in
  let ops = List.concat [ applied ; refused ; branch_refused ; branch_delayed ] in
  List.iter (Lwt_watcher.notify new_operation_input) ops

let shutdown_operation_input (State { new_operation_input }) =
  Lwt_watcher.shutdown_input new_operation_input

let build_rpc_directory protocol =
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
  let module Proto_services = Block_services.Make(Proto)(Proto) in

  let dir : (prevalidation_state * Error_monad.error Preapply_result.t) RPC_directory.t ref =
    ref RPC_directory.empty in

  let gen_register s f =
    dir := RPC_directory.gen_register !dir s f in

  gen_register
    (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
    begin fun ((State { new_operation_input ; proto = (module Next_proto) }), current_mempool) params () ->
      let operation_stream, stopper =
        Lwt_watcher.create_stream new_operation_input in
      (* Convert ops *)
      let map_op op =
        let protocol_data =
          Data_encoding.Binary.of_bytes_exn
            Proto.operation_data_encoding
            op.Operation.proto in
        Proto.{ shell = op.shell ; protocol_data } in
      let fold_op _k (op, _error) acc = map_op op :: acc in
      (* First call : retrieve the current set of op from the mempool *)
      let { applied ; refused ; branch_refused ; branch_delayed } = current_mempool in
      let applied = if params#applied then List.map map_op (List.map snd applied) else [] in
      let refused = if params#refused then
          Operation_hash.Map.fold fold_op refused [] else [] in
      let branch_refused = if params#branch_refused then
          Operation_hash.Map.fold fold_op branch_refused [] else [] in
      let branch_delayed = if params#branch_delayed then
          Operation_hash.Map.fold fold_op branch_delayed [] else [] in
      let current_mempool = List.concat [ applied ; refused ; branch_refused ; branch_delayed ] in
      let current_mempool = ref (Some current_mempool) in
      let filter_result = function
        | `Applied -> params#applied
        | `Refused -> params#branch_refused
        | `Branch_refused -> params#refused
        | `Branch_delayed -> params#branch_delayed
      in
      let next () =
        match !current_mempool with
        | Some mempool -> begin
            current_mempool := None ;
            Lwt.return_some mempool
          end
        | None -> begin
            Lwt_stream.get operation_stream >>= function
            | Some (kind, shell, protocol_data) when filter_result kind ->
                let bytes = Data_encoding.Binary.to_bytes_exn
                    Next_proto.operation_data_encoding
                    protocol_data in
                let protocol_data = Data_encoding.Binary.of_bytes_exn
                    Proto.operation_data_encoding
                    bytes in
                Lwt.return_some [ { Proto.shell ; protocol_data } ]
            | _ -> Lwt.return_none
          end
      in
      let shutdown () = Lwt_watcher.shutdown stopper in
      RPC_answer.return_stream { next ; shutdown }
    end ;

  return !dir
