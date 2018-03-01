(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Prevalidator_worker_state

type limits = {
  max_refused_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}

module Name = struct
  type t = Chain_id.t
  let encoding = Chain_id.encoding
  let base = [ "prevalidator" ]
  let pp = Chain_id.pp_short
end

module Types = struct
  (* Invariants:
     - an operation is in only one of these sets (map domains):
       pv.refusals pv.pending pv.fetching pv.live_operations pv.in_mempool
     - pv.in_mempool is the domain of all fields of pv.prevalidation_result
     - pv.prevalidation_result.refused = Ø, refused ops are in pv.refused
     - the 'applied' operations in pv.validation_result are in reverse order. *)
  type state = {
    chain_db : Distributed_db.chain_db ;
    limits : limits ;
    mutable predecessor : State.Block.t ;
    mutable timestamp : Time.t ;
    mutable live_blocks : Block_hash.Set.t ; (* just a cache *)
    mutable live_operations : Operation_hash.Set.t ; (* just a cache *)
    refused : Operation_hash.t Ring.t ;
    mutable refusals : error list Operation_hash.Map.t ;
    mutable fetching : Operation_hash.Set.t ;
    mutable pending : Operation.t Operation_hash.Map.t ;
    mutable mempool : Mempool.t ;
    mutable in_mempool : Operation_hash.Set.t ;
    mutable validation_result : error Preapply_result.t ;
    mutable validation_state : Prevalidation.prevalidation_state tzresult ;
    mutable advertisement : [ `Pending of Mempool.t | `None ] ;
  }
  type parameters = limits * Distributed_db.chain_db

  include Worker_state

  let view (state : state) _ : view =
    let domain map =
      Operation_hash.Map.fold
        (fun elt _ acc -> Operation_hash.Set.add elt acc)
        map Operation_hash.Set.empty in
    { head = State.Block.hash state.predecessor ;
      timestamp = state.timestamp ;
      fetching = state.fetching ;
      pending = domain state.pending ;
      applied =
        List.rev
          (List.map (fun (h, _) -> h)
             state.validation_result.applied) ;
      delayed =
        Operation_hash.Set.union
          (domain state.validation_result.branch_delayed)
          (domain state.validation_result.branch_refused) }

end

module Worker = Worker.Make (Name) (Event) (Request) (Types)

open Types

type t = Worker.infinite Worker.queue Worker.t
type error += Closed = Worker.Closed

let debug w =
  Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

let list_pendings ?maintain_chain_db  ~from_block ~to_block old_mempool =
  let rec pop_blocks ancestor block mempool =
    let hash = State.Block.hash block in
    if Block_hash.equal hash ancestor then
      Lwt.return mempool
    else
      State.Block.all_operations block >>= fun operations ->
      Lwt_list.fold_left_s
        (Lwt_list.fold_left_s (fun mempool op ->
             let h = Operation.hash op in
             Lwt_utils.may maintain_chain_db
               ~f:begin fun chain_db ->
                 Distributed_db.inject_operation chain_db h op >>= fun _ ->
                 Lwt.return_unit
               end >>= fun () ->
             Lwt.return (Operation_hash.Map.add h op mempool)))
        mempool operations >>= fun mempool ->
      State.Block.predecessor block >>= function
      | None -> assert false
      | Some predecessor -> pop_blocks ancestor predecessor mempool
  in
  let push_block mempool block =
    State.Block.all_operation_hashes block >|= fun operations ->
    Option.iter maintain_chain_db
      ~f:(fun chain_db ->
          List.iter
            (List.iter (Distributed_db.Operation.clear_or_cancel chain_db))
            operations) ;
    List.fold_left
      (List.fold_left (fun mempool h -> Operation_hash.Map.remove h mempool))
      mempool operations
  in
  Chain_traversal.new_blocks ~from_block ~to_block >>= fun (ancestor, path) ->
  pop_blocks
    (State.Block.hash ancestor)
    from_block old_mempool >>= fun mempool ->
  Lwt_list.fold_left_s push_block mempool path >>= fun new_mempool ->
  Lwt.return new_mempool

let already_handled pv oph =
  Operation_hash.Map.mem oph pv.refusals
  || Operation_hash.Map.mem oph pv.pending
  || Operation_hash.Set.mem oph pv.fetching
  || Operation_hash.Set.mem oph pv.live_operations
  || Operation_hash.Set.mem oph pv.in_mempool

let mempool_of_prevalidation_result (r : error Preapply_result.t) : Mempool.t =
  { Mempool.known_valid = List.map fst r.applied ;
    pending =
      Operation_hash.Map.fold
        (fun k _ s -> Operation_hash.Set.add k s)
        r.branch_delayed @@
      Operation_hash.Map.fold
        (fun k _ s -> Operation_hash.Set.add k s)
        r.branch_refused @@
      Operation_hash.Set.empty }

let merge_validation_results ~old ~neu =
  let open Preapply_result in
  let merge _key a b =
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | _, Some y -> Some y in
  let filter_out s m =
    List.fold_right (fun (h, _op) -> Operation_hash.Map.remove h) s m in
  { applied = List.rev_append neu.applied old.applied ;
    refused = Operation_hash.Map.empty ;
    branch_refused =
      Operation_hash.Map.merge merge
        (* filtering should not be required if the protocol is sound *)
        (filter_out neu.applied old.branch_refused)
        neu.branch_refused ;
    branch_delayed =
      Operation_hash.Map.merge merge
        (filter_out neu.applied old.branch_delayed)
        neu.branch_delayed }

let advertise (w : t) pv mempool =
  match pv.advertisement with
  | `Pending { Mempool.known_valid ; pending } ->
      pv.advertisement <-
        `Pending
          { known_valid = known_valid @ mempool.Mempool.known_valid ;
            pending = Operation_hash.Set.union pending mempool.pending }
  | `None ->
      pv.advertisement <- `Pending mempool ;
      Lwt.async (fun () ->
          Lwt_unix.sleep 0.01 >>= fun () ->
          Worker.push_request_now w Advertise ;
          Lwt.return_unit)

let handle_unprocessed w pv =
  begin match pv.validation_state with
    | Error err ->
        pv.validation_result <-
          { Preapply_result.empty with
            branch_delayed =
              Operation_hash.Map.fold
                (fun h op m -> Operation_hash.Map.add h (op, err) m)
                pv.pending Operation_hash.Map.empty } ;
        pv.pending <-
          Operation_hash.Map.empty ;
        Lwt.return ()
    | Ok validation_state ->
        match Operation_hash.Map.cardinal pv.pending with
        | 0 -> Lwt.return ()
        | n -> debug w "processing %d operations" n ;
            Prevalidation.prevalidate validation_state ~sort:true
              (Operation_hash.Map.bindings pv.pending)
            >>= fun (validation_state, validation_result) ->
            pv.validation_state <-
              Ok validation_state ;
            pv.in_mempool <-
              (Operation_hash.Map.fold
                 (fun h _ in_mempool -> Operation_hash.Set.add h in_mempool)
                 pv.pending @@
               Operation_hash.Map.fold
                 (fun h _ in_mempool -> Operation_hash.Set.remove h in_mempool)
                 pv.validation_result.refused @@
               pv.in_mempool) ;
            Operation_hash.Map.iter
              (fun h (_, errs) ->
                 Option.iter (Ring.add_and_return_erased pv.refused h)
                   ~f:(fun e -> pv.refusals <- Operation_hash.Map.remove e pv.refusals) ;
                 pv.refusals <-
                   Operation_hash.Map.add h errs pv.refusals)
              pv.validation_result.refused ;
            Operation_hash.Map.iter
              (fun oph _ -> Distributed_db.Operation.clear_or_cancel pv.chain_db oph)
              pv.validation_result.refused ;
            pv.validation_result <-
              merge_validation_results
                ~old:pv.validation_result
                ~neu:validation_result ;
            pv.pending <-
              Operation_hash.Map.empty ;
            advertise w pv
              (mempool_of_prevalidation_result validation_result) ;
            Lwt.return ()
  end >>= fun () ->
  pv.mempool <-
    { Mempool.known_valid =
        List.rev_map fst pv.validation_result.applied ;
      pending =
        Operation_hash.Map.fold
          (fun k _ s -> Operation_hash.Set.add k s)
          pv.validation_result.branch_delayed @@
        Operation_hash.Map.fold
          (fun k _ s -> Operation_hash.Set.add k s)
          pv.validation_result.branch_refused @@
        Operation_hash.Set.empty } ;
  State.Current_mempool.set (Distributed_db.chain_state pv.chain_db)
    ~head:(State.Block.hash pv.predecessor) pv.mempool >>= fun () ->
  Lwt.return ()

let fetch_operation w pv ?peer oph =
  debug w
    "fetching operation %a"
    Operation_hash.pp_short oph ;
  Distributed_db.Operation.fetch
    ~timeout:pv.limits.operation_timeout
    pv.chain_db ?peer oph () >>= function
  | Ok op ->
      Worker.push_request_now w (Arrived (oph, op)) ;
      Lwt.return_unit
  | Error [ Distributed_db.Operation.Canceled _ ] ->
      debug w
        "operation %a included before being prevalidated"
        Operation_hash.pp_short oph ;
      Lwt.return_unit
  | Error _ -> (* should not happen *)
      Lwt.return_unit

let on_operation_arrived (pv : state) oph op =
  pv.fetching <- Operation_hash.Set.remove oph pv.fetching ;
  if not (Block_hash.Set.mem op.Operation.shell.branch pv.live_blocks) then begin
    Distributed_db.Operation.clear_or_cancel pv.chain_db oph
    (* TODO: put in a specific delayed map ? *)
  end else if not (already_handled pv oph) (* prevent double inclusion on flush *) then begin
    pv.pending <- Operation_hash.Map.add oph op pv.pending
  end

let on_inject pv op =
  let oph = Operation.hash op in
  begin
    if already_handled pv oph then
      return pv.validation_result
    else
      Lwt.return pv.validation_state >>=? fun validation_state ->
      Prevalidation.prevalidate
        validation_state ~sort:false [ (oph, op) ] >>= fun (_, result) ->
      match result.applied with
      | [ app_oph, _ ] when Operation_hash.equal app_oph oph ->
          Distributed_db.inject_operation pv.chain_db oph op >>= fun (_ : bool) ->
          pv.pending <- Operation_hash.Map.add oph op pv.pending ;
          return result
      | _ ->
          return result
  end >>=? fun result ->
  if List.mem_assoc oph result.applied then
    return ()
  else
    let try_in_map map proj or_else =
      try
        Lwt.return (Error (proj (Operation_hash.Map.find oph map)))
      with Not_found -> or_else () in
    try_in_map pv.refusals (fun h -> h) @@ fun () ->
    try_in_map result.refused snd @@ fun () ->
    try_in_map result.branch_refused snd @@ fun () ->
    try_in_map result.branch_delayed snd @@ fun () ->
    if Operation_hash.Set.mem oph pv.live_operations then
      failwith "Injected operation %a included in a previous block."
        Operation_hash.pp oph
    else
      failwith "Injected operation %a is not in prevalidation result."
        Operation_hash.pp oph

let on_notify w pv peer mempool =
  let all_ophs =
    List.fold_left
      (fun s oph -> Operation_hash.Set.add oph s)
      mempool.Mempool.pending mempool.known_valid in
  let to_fetch =
    Operation_hash.Set.filter
      (fun oph -> not (already_handled pv oph))
      all_ophs in
  pv.fetching <-
    Operation_hash.Set.union
      to_fetch
      pv.fetching ;
  Operation_hash.Set.iter
    (fun oph -> Lwt.ignore_result (fetch_operation w pv ~peer oph))
    to_fetch

let on_flush w pv predecessor =
  list_pendings
    ~maintain_chain_db:pv.chain_db
    ~from_block:pv.predecessor ~to_block:predecessor
    (Preapply_result.operations pv.validation_result) >>= fun pending ->
  let timestamp = Time.now () in
  Chain_traversal.live_blocks
    predecessor
    (State.Block.max_operations_ttl predecessor)
  >>= fun (new_live_blocks, new_live_operations) ->
  Prevalidation.start_prevalidation
    ~predecessor ~timestamp () >>= fun validation_state ->
  begin match validation_state with
    | Error _ -> Lwt.return (validation_state, Preapply_result.empty)
    | Ok validation_state ->
        Prevalidation.prevalidate
          validation_state ~sort:false [] >>= fun (state, result) ->
        Lwt.return (Ok state, result)
  end >>= fun (validation_state, validation_result) ->
  debug w "%d operations were not washed by the flush"
    (Operation_hash.Map.cardinal pending) ;
  pv.predecessor <- predecessor ;
  pv.live_blocks <- new_live_blocks ;
  pv.live_operations <- new_live_operations ;
  pv.timestamp <- timestamp ;
  pv.mempool <- { known_valid = [] ; pending = Operation_hash.Set.empty };
  pv.pending <- pending ;
  pv.in_mempool <- Operation_hash.Set.empty ;
  pv.validation_result <- validation_result ;
  pv.validation_state <- validation_state ;
  return ()

let on_advertise pv =
  match pv.advertisement with
  | `None -> () (* should not happen *)
  | `Pending mempool ->
      pv.advertisement <- `None ;
      Distributed_db.Advertise.current_head pv.chain_db ~mempool pv.predecessor

let on_request
  : type r. t -> r Request.t -> r tzresult Lwt.t
  = fun w request ->
    let pv = Worker.state w in
    begin match request with
      | Request.Flush hash ->
          on_advertise pv ;
          (* TODO: rebase the advertisement instead *)
          let chain_state = Distributed_db.chain_state pv.chain_db in
          State.Block.read chain_state hash >>=? fun block ->
          on_flush w pv block >>=? fun () ->
          return (() : r)
      | Request.Notify (peer, mempool) ->
          on_notify w pv peer mempool ;
          return ()
      | Request.Inject op ->
          on_inject pv op
      | Request.Arrived (oph, op) ->
          on_operation_arrived pv oph op ;
          return ()
      | Request.Advertise ->
          on_advertise pv ;
          return ()
    end >>=? fun r ->
    handle_unprocessed w pv >>= fun () ->
    return r

let on_close w =
  let pv = Worker.state w in
  Operation_hash.Set.iter
    (Distributed_db.Operation.clear_or_cancel pv.chain_db)
    pv.fetching ;
  Lwt.return_unit

let on_launch w _ (limits, chain_db) =
  let chain_state = Distributed_db.chain_state chain_db in
  Chain.data chain_state >>= fun
    { current_head = predecessor ; current_mempool = mempool ;
      live_blocks ; live_operations } ->
  let timestamp = Time.now () in
  Prevalidation.start_prevalidation
    ~predecessor ~timestamp () >>= fun validation_state ->
  begin match validation_state with
    | Error _ -> Lwt.return (validation_state, Preapply_result.empty)
    | Ok validation_state ->
        Prevalidation.prevalidate validation_state ~sort:false []
        >>= fun (validation_state, validation_result) ->

        Lwt.return (Ok validation_state, validation_result)
  end >>= fun (validation_state, validation_result) ->
  let fetching =
    List.fold_left
      (fun s h -> Operation_hash.Set.add h s)
      Operation_hash.Set.empty mempool.known_valid in
  let pv =
    { limits ; chain_db ;
      predecessor ; timestamp ; live_blocks ; live_operations ;
      mempool = { known_valid = [] ; pending = Operation_hash.Set.empty };
      refused = Ring.create limits.max_refused_operations ;
      refusals = Operation_hash.Map.empty ;
      fetching ;
      pending = Operation_hash.Map.empty ;
      in_mempool = Operation_hash.Set.empty ;
      validation_result ; validation_state ;
      advertisement = `None } in
  List.iter
    (fun oph -> Lwt.ignore_result (fetch_operation w pv oph))
    mempool.known_valid ;
  Lwt.return pv

let on_error w r st errs =
  Worker.record_event w (Event.Request (r, st, Some errs)) ;
  match r with
  | Request.(View (Inject _)) -> return ()
  | _ -> Lwt.return (Error errs)

let on_completion w r _ st =
  Worker.record_event w (Event.Request (Request.view r, st, None )) ;
  Lwt.return ()

let table = Worker.create_table Queue

let create limits chain_db =
  let chain_state = Distributed_db.chain_state chain_db in
  let module Handlers = struct
    type self = t
    let on_launch = on_launch
    let on_request = on_request
    let on_close = on_close
    let on_error = on_error
    let on_completion = on_completion
    let on_no_request _ = return ()
  end in
  Worker.launch table limits.worker_limits
    (State.Chain.id chain_state)
    (limits, chain_db)
    (module Handlers)

let shutdown = Worker.shutdown

let flush w head =
  Worker.push_request_and_wait w (Flush head)

let notify_operations w peer mempool =
  Worker.push_request_now w (Notify (peer, mempool))

let operations w =
  let pv = Worker.state w in
  { pv.validation_result with
    applied = List.rev pv.validation_result.applied },
  pv.pending

let pending ?block w =
  let pv = Worker.state w in
  let ops = Preapply_result.operations pv.validation_result in
  match block with
  | Some to_block ->
      list_pendings
        ~maintain_chain_db:pv.chain_db
        ~from_block:pv.predecessor ~to_block ops
  | None -> Lwt.return ops

let timestamp w =
  let pv = Worker.state w in
  pv.timestamp

let context w =
  let pv = Worker.state w in
  Lwt.return pv.validation_state >>=? fun validation_state ->
  Prevalidation.end_prevalidation validation_state

let inject_operation w op =
  Worker.push_request_and_wait w (Inject op)

let status = Worker.status

let running_workers () = Worker.list table

let pending_requests t = Worker.pending_requests t

let current_request t = Worker.current_request t

let last_events = Worker.last_events
