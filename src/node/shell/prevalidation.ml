(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type 'error preapply_result = {
  applied: (Operation_hash.t * Operation.t) list;
  refused: (Operation.t * 'error list) Operation_hash.Map.t;
  branch_refused: (Operation.t * 'error list) Operation_hash.Map.t;
  branch_delayed: (Operation.t * 'error list) Operation_hash.Map.t;
}

let empty_result = {
  applied = [] ;
  refused = Operation_hash.Map.empty ;
  branch_refused = Operation_hash.Map.empty ;
  branch_delayed = Operation_hash.Map.empty ;
}

let map_result f r = {
  applied = r.applied;
  refused = Operation_hash.Map.map f r.refused ;
  branch_refused = Operation_hash.Map.map f r.branch_refused ;
  branch_delayed = Operation_hash.Map.map f r.branch_delayed ;
}

let preapply_result_encoding error_encoding =
  let open Data_encoding in
  let operation_encoding =
    merge_objs
      (obj1 (req "hash" Operation_hash.encoding))
      (dynamic_size Operation.encoding) in
  let refused_encoding =
    merge_objs
      (obj1 (req "hash" Operation_hash.encoding))
      (merge_objs
         (dynamic_size Operation.encoding)
         (obj1 (req "error" error_encoding))) in
  let build_list map = Operation_hash.Map.bindings map in
  let build_map list =
    List.fold_right
      (fun (k, e) m -> Operation_hash.Map.add k e m)
      list Operation_hash.Map.empty in
  conv
    (fun { applied ; refused ; branch_refused ; branch_delayed } ->
       (applied, build_list refused,
        build_list branch_refused, build_list branch_delayed))
    (fun (applied, refused, branch_refused, branch_delayed) ->
       let refused = build_map refused in
       let branch_refused = build_map branch_refused in
       let branch_delayed = build_map branch_delayed in
       { applied ; refused ; branch_refused ; branch_delayed })
    (obj4
       (req "applied" (list operation_encoding))
       (req "refused" (list refused_encoding))
       (req "branch_refused" (list refused_encoding))
       (req "branch_delayed" (list refused_encoding)))

let preapply_result_operations t =
  let ops =
    List.fold_left
      (fun acc (h, op) -> Operation_hash.Map.add h op acc)
      Operation_hash.Map.empty t.applied in
  let ops =
    Operation_hash.Map.fold
      (fun h (op, _err) acc -> Operation_hash.Map.add h op acc)
      t.branch_delayed ops in
  let ops =
    Operation_hash.Map.fold
      (fun h (op, _err) acc -> Operation_hash.Map.add h op acc)
      t.branch_refused ops in
  ops

let empty_result =
  { applied = [] ;
    refused = Operation_hash.Map.empty ;
    branch_refused = Operation_hash.Map.empty ;
    branch_delayed = Operation_hash.Map.empty }

let rec apply_operations apply_operation state r ~sort ops =
  Lwt_list.fold_left_s
    (fun (state, r) (hash, op, parsed_op) ->
       apply_operation state parsed_op >>= function
       | Ok state ->
           let applied = (hash, op) :: r.applied in
           Lwt.return (state, { r with applied } )
       | Error errors ->
           match classify_errors errors with
           | `Branch ->
               let branch_refused =
                 Operation_hash.Map.add hash (op, errors) r.branch_refused in
               Lwt.return (state, { r with branch_refused })
           | `Permanent ->
               let refused =
                 Operation_hash.Map.add hash (op, errors) r.refused in
               Lwt.return (state, { r with refused })
           | `Temporary ->
               let branch_delayed =
                 Operation_hash.Map.add hash (op, errors) r.branch_delayed in
               Lwt.return (state, { r with branch_delayed }))
    (state, r)
    ops >>= fun (state, r) ->
  match r.applied with
  | _ :: _ when sort ->
      let rechecked_operations =
        List.filter
          (fun (hash, _, _) -> Operation_hash.Map.mem hash r.branch_delayed)
          ops in
      let remaining = List.length rechecked_operations in
      if remaining = 0 || remaining = List.length ops then
        Lwt.return (state, r)
      else
        apply_operations apply_operation state r ~sort rechecked_operations
  | _ ->
      Lwt.return (state, r)

type prevalidation_state =
    State : { proto : 'a proto ; state : 'a }
    -> prevalidation_state

and 'a proto =
  (module State.Registred_protocol.T with type validation_state = 'a)

let start_prevalidation ?proto_header ~predecessor ~timestamp () =
  let { Block_header.shell =
          { fitness = predecessor_fitness ;
            timestamp = predecessor_timestamp ;
            level = predecessor_level } } =
    State.Block.header predecessor in
  State.Block.context predecessor >>= fun predecessor_context ->
  Context.get_protocol predecessor_context >>= fun protocol ->
  let predecessor = State.Block.hash predecessor in
  begin
    match State.Registred_protocol.get protocol with
    | None ->
        (* FIXME. *)
        (* This should not happen: it should be handled in the validator. *)
        failwith "Prevalidation: missing protocol '%a' for the current block."
          Protocol_hash.pp_short protocol
    | Some protocol ->
        return protocol
  end >>=? fun (module Proto) ->
  Context.reset_test_network
    predecessor_context predecessor
    timestamp >>= fun predecessor_context ->
  Proto.begin_construction
    ~predecessor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    ~predecessor_level
    ~predecessor
    ~timestamp
    ?proto_header
    ()
  >>=? fun state ->
  return (State { proto = (module Proto) ; state })

type error += Parse_error

let prevalidate
    (State { proto = (module Proto) ; state })
    ~sort (ops : (Operation_hash.t * Operation.t) list)=
  let ops =
    List.map
      (fun (h, op) ->
         (h, op, Proto.parse_operation h op |> record_trace Parse_error))
      ops in
  let invalid_ops =
    Utils.filter_map
      (fun (h, op, parsed_op) -> match parsed_op with
         | Ok _ -> None
         | Error err -> Some (h, op, err)) ops
  and parsed_ops =
    Utils.filter_map
      (fun (h, op, parsed_op) -> match parsed_op with
         | Ok parsed_op -> Some (h, op, parsed_op)
         | Error _ -> None) ops in
  let sorted_ops =
    if sort then
      let compare (_, _, op1) (_, _, op2) = Proto.compare_operations op1 op2 in
      List.sort compare parsed_ops
    else parsed_ops in
  apply_operations
    Proto.apply_operation
    state empty_result ~sort sorted_ops >>= fun (state, r) ->
  let r =
    { r with
      applied = List.rev r.applied ;
      branch_refused =
        List.fold_left
          (fun map (h, op, err) -> Operation_hash.Map.add h (op, err) map)
          r.branch_refused invalid_ops } in
  Lwt.return (State { proto = (module Proto) ; state }, r)

let end_prevalidation (State { proto = (module Proto) ; state }) =
  Proto.finalize_block state
