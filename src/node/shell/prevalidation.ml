(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type 'error preapply_result = {
  applied: Operation_hash.t list;
  refused: 'error list Operation_hash.Map.t;
  branch_refused: 'error list Operation_hash.Map.t;
  branch_delayed: 'error list Operation_hash.Map.t;
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
  let refused_encoding = tup2 Operation_hash.encoding error_encoding in
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
       (req "applied" (list Operation_hash.encoding))
       (req "refused" (list refused_encoding))
       (req "branch_refused" (list refused_encoding))
       (req "branch_delayed" (list refused_encoding)))

let preapply_result_operations t =
  let ops =
    List.fold_left
      (fun acc x -> Operation_hash.Set.add x acc)
      Operation_hash.Set.empty t.applied in
  let ops =
    Operation_hash.Map.fold
      (fun x _ acc -> Operation_hash.Set.add x acc)
      t.branch_delayed ops in
  let ops =
    Operation_hash.Map.fold
      (fun x _ acc -> Operation_hash.Set.add x acc)
      t.branch_refused ops in
  ops

let empty_result =
  { applied = [] ;
    refused = Operation_hash.Map.empty ;
    branch_refused = Operation_hash.Map.empty ;
    branch_delayed = Operation_hash.Map.empty }

let merge_result r r' =
  let open Updater in
  let merge _key a b =
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | _, Some y -> Some y in
  let merge_map =
    Operation_hash.Map.merge merge in
  { applied = r'.applied @ r.applied ;
    refused = merge_map r.refused r'.refused ;
    branch_refused = merge_map r.branch_refused r'.branch_refused ;
    branch_delayed = r'.branch_delayed }

let rec apply_operations apply_operation state ~sort ops =
  Lwt_list.fold_left_s
    (fun (state, r) (hash, op) ->
       apply_operation state op >>= function
       | Ok state ->
           let applied = hash :: r.applied in
           Lwt.return (state, { r with applied} )
       | Error errors ->
           match classify_errors errors with
           | `Branch ->
               let branch_refused =
                 Operation_hash.Map.add hash errors r.branch_refused in
               Lwt.return (state, { r with branch_refused })
           | `Permanent ->
               let refused =
                 Operation_hash.Map.add hash errors r.refused in
               Lwt.return (state, { r with refused })
           | `Temporary ->
               let branch_delayed =
                 Operation_hash.Map.add hash errors r.branch_delayed in
               Lwt.return (state, { r with branch_delayed }))
    (state, empty_result)
    ops >>= fun (state, r) ->
  match r.applied with
  | _ :: _ when sort ->
      let rechecked_operations =
        List.filter
          (fun (hash, _) -> Operation_hash.Map.mem hash r.branch_delayed)
          ops in
      apply_operations apply_operation
        state ~sort rechecked_operations >>=? fun (state, r') ->
      return (state, merge_result r r')
  | _ ->
      return (state, r)

type prevalidation_state =
    State : { proto : 'a proto ; state : 'a }
    -> prevalidation_state

and 'a proto =
  (module Updater.REGISTRED_PROTOCOL
    with type validation_state = 'a)

let start_prevalidation
    ~predecessor
    ~timestamp =
  let { Block_header.shell =
          { fitness = predecessor_fitness ;
            timestamp = predecessor_timestamp ;
            level = predecessor_level } } =
    State.Block.header predecessor in
  State.Block.context predecessor >>= fun predecessor_context ->
  Context.get_protocol predecessor_context >>= fun protocol ->
  let predecessor = State.Block.hash predecessor in
  let (module Proto) =
    match Updater.get protocol with
    | None -> assert false (* FIXME, this should not happen! *)
    | Some protocol -> protocol in
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
    ()
  >>=? fun state ->
  return (State { proto = (module Proto) ; state })

let prevalidate
    (State { proto = (module Proto) ; state })
    ~sort ops =
  (* The operations list length is bounded by the size of the mempool,
     where eventually an operation should not stay more than one hours. *)
  Lwt_list.map_p
    (fun (h, op) ->
       match Proto.parse_operation h op with
       | Error _ ->
           (* the operation will never be validated in the
              current context, it is silently ignored. It may be
              reintroduced in the loop by the next `flush`. *)
           Lwt.return_none
       | Ok p -> Lwt.return (Some (h, p)))
    ops >>= fun ops ->
  let ops = Utils.unopt_list ops in
  let ops =
    if sort then
      let compare (_, op1) (_, op2) = Proto.compare_operations op1 op2 in
      List.sort compare ops
    else ops in
  apply_operations Proto.apply_operation state ~sort ops >>=? fun (state, r) ->
  return (State { proto = (module Proto) ; state }, r)

let end_prevalidation (State { proto = (module Proto) ; state }) =
  Proto.finalize_block state
