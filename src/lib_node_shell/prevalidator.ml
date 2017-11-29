(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Prevalidator

let list_pendings ?maintain_net_db  ~from_block ~to_block old_mempool =
  let rec pop_blocks ancestor block mempool =
    let hash = State.Block.hash block in
    if Block_hash.equal hash ancestor then
      Lwt.return mempool
    else
      State.Block.all_operations block >>= fun operations ->
      Lwt_list.fold_left_s
        (Lwt_list.fold_left_s (fun mempool op ->
             let h = Operation.hash op in
             Lwt_utils.may maintain_net_db
               ~f:begin fun net_db ->
                 Distributed_db.inject_operation net_db h op >>= fun _ ->
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
    Option.iter maintain_net_db
      ~f:(fun net_db ->
          List.iter
            (List.iter (Distributed_db.Operation.clear_or_cancel net_db))
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

type 'a request =
  | Flush : State.Block.t -> unit request
  | Notify : P2p.Peer_id.t * Mempool.t -> unit request
  | Inject : Operation.t -> unit tzresult request
  | Arrived : Operation_hash.t * Operation.t -> unit request
  | Advertise : unit request

type message = Message: 'a request * 'a tzresult Lwt.u option -> message

let wakeup_with_result
  : type t.
    t request ->
    t tzresult Lwt.u option ->
    (t request -> t tzresult Lwt.t) ->
    unit tzresult Lwt.t
  = fun req u cb -> match u with
    | None ->
        cb req >>=? fun _res -> return ()
    | Some u ->
        cb req >>= fun res ->
        Lwt.wakeup_later u res ;
        Lwt.return (res >>? fun _res -> ok ())

type limits = {
  max_refused_operations : int ;
  operation_timeout : float
}

(* Invariants:
   - an operation is in only one of these sets (map domains):
     pv.refusals pv.pending pv.fetching pv.live_operations pv.in_mempool
   - pv.in_mempool is the domain of all fields of pv.prevalidation_result
   - pv.prevalidation_result.refused = Ø, refused ops are in pv.refused *)
type t = {
  net_db : Distributed_db.net_db ;
  limits : limits ;
  canceler : Lwt_canceler.t ;
  message_queue : message Lwt_pipe.t ;
  mutable (* just for init *) worker : unit Lwt.t ;
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

type error += Closed

let push_request pv request =
  Lwt_pipe.safe_push_now pv.message_queue (Message (request, None))

let push_request_and_wait pv request =
  let t, u = Lwt.wait () in
  Lwt.catch
    (fun () ->
       Lwt_pipe.push_now_exn pv.message_queue (Message (request, Some u)) ;
       t)
    (function
      | Lwt_pipe.Closed -> fail Closed
      | exn -> fail (Exn exn))

let close_queue pv =
  let messages = Lwt_pipe.pop_all_now pv.message_queue in
  List.iter
    (function
      | Message (_, Some u) -> Lwt.wakeup_later u (Error [ Closed ])
      | _ -> ())
    messages ;
  Lwt_pipe.close pv.message_queue

let already_handled pv oph =
  Operation_hash.Map.mem oph pv.refusals
  || Operation_hash.Map.mem oph pv.pending
  || Operation_hash.Set.mem oph pv.fetching
  || Operation_hash.Set.mem oph pv.live_operations
  || Operation_hash.Set.mem oph pv.in_mempool

let mempool_of_prevalidation_result (r : error Preapply_result.t) : Mempool.t =
  { Mempool.known_valid = fst (List.split r.applied) ;
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

let advertise pv mempool =
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
          push_request pv Advertise ;
          Lwt.return_unit)

let handle_unprocessed pv =
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
        if Operation_hash.Map.is_empty pv.pending then
          Lwt.return ()
        else
          begin match Operation_hash.Map.cardinal pv.pending with
            | 0 -> Lwt.return ()
            | n -> lwt_debug "processing %d operations" n
          end >>= fun () ->
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
            (fun oph _ -> Distributed_db.Operation.clear_or_cancel pv.net_db oph)
            pv.validation_result.refused ;
          pv.validation_result <-
            merge_validation_results
              ~old:pv.validation_result
              ~neu:validation_result ;
          pv.pending <-
            Operation_hash.Map.empty ;
          advertise pv
            (mempool_of_prevalidation_result validation_result) ;
          Lwt.return ()
  end >>= fun () ->
  pv.mempool <-
    { Mempool.known_valid =
        fst (List.split pv.validation_result.applied) ;
      pending =
        Operation_hash.Map.fold
          (fun k _ s -> Operation_hash.Set.add k s)
          pv.validation_result.branch_delayed @@
        Operation_hash.Map.fold
          (fun k _ s -> Operation_hash.Set.add k s)
          pv.validation_result.branch_refused @@
        Operation_hash.Set.empty } ;
  Mempool.set (Distributed_db.net_state pv.net_db)
    ~head:(State.Block.hash pv.predecessor) pv.mempool >>= fun () ->
  Lwt.return ()

let fetch_operation pv ?peer oph =
  debug "fetching operation %a" Operation_hash.pp_short oph ;
  Distributed_db.Operation.fetch
    ~timeout:pv.limits.operation_timeout
    pv.net_db ?peer oph () >>= function
  | Ok op ->
      push_request pv (Arrived (oph, op)) ;
      Lwt.return_unit
  | Error [ Distributed_db.Operation.Canceled _ ] ->
      lwt_debug
        "operation %a included before being prevalidated"
        Operation_hash.pp_short oph >>= fun () ->
      Lwt.return_unit
  | Error _ -> (* should not happen *)
      Lwt.return_unit

let clear_fetching pv =
  Operation_hash.Set.iter
    (Distributed_db.Operation.clear_or_cancel pv.net_db)
    pv.fetching

let on_operation_arrived pv oph op =
  debug "operation %a retrieved" Operation_hash.pp_short oph ;
  pv.fetching <- Operation_hash.Set.remove oph pv.fetching ;
  if not (Block_hash.Set.mem op.Operation.shell.branch pv.live_blocks) then begin
    Distributed_db.Operation.clear_or_cancel pv.net_db oph
    (* TODO: put in a specific delayed map ? *)
  end else if not (already_handled pv oph) (* prevent double inclusion on flush *) then begin
    pv.pending <- Operation_hash.Map.add oph op pv.pending
  end

let on_inject pv op =
  let oph = Operation.hash op in
  log_notice "injection of operation %a" Operation_hash.pp_short oph ;
  begin
    begin if already_handled pv oph then
        return pv.validation_result
      else
        Lwt.return pv.validation_state >>=? fun validation_state ->
        Prevalidation.prevalidate
          validation_state ~sort:false [ (oph, op) ] >>= fun (_, result) ->
        match result.applied with
        | [ app_oph, _ ] when Operation_hash.equal app_oph oph ->
            Distributed_db.inject_operation pv.net_db oph op >>= fun (_ : bool) ->
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
      try_in_map pv.refusals (fun h -> h)  @@ fun () ->
      try_in_map result.refused snd @@ fun () ->
      try_in_map result.branch_refused snd @@ fun () ->
      try_in_map result.branch_delayed snd @@ fun () ->
      if Operation_hash.Set.mem oph pv.live_operations then
        failwith "Injected operation %a included in a previous block."
          Operation_hash.pp oph
      else
        failwith "Injected operation %a is not in prevalidation result."
          Operation_hash.pp oph
  end >>= fun tzresult ->
  return tzresult

let on_notify pv peer mempool =
  let all_ophs =
    List.fold_left
      (fun s oph -> Operation_hash.Set.add oph s)
      mempool.Mempool.pending mempool.known_valid in
  let to_fetch =
    Operation_hash.Set.filter
      (fun oph -> not (already_handled pv oph))
      all_ophs in
  debug "notification of %d new operations" (Operation_hash.Set.cardinal to_fetch) ;
  pv.fetching <-
    Operation_hash.Set.union
      to_fetch
      pv.fetching ;
  Operation_hash.Set.iter
    (fun oph -> Lwt.ignore_result (fetch_operation ~peer pv oph))
    to_fetch

let on_flush pv predecessor =
  list_pendings
    ~maintain_net_db:pv.net_db
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
  lwt_log_notice "flushing the mempool for new head %a (%d operations)"
    Block_hash.pp_short (State.Block.hash predecessor)
    (Operation_hash.Map.cardinal pending) >>= fun () ->
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
      Distributed_db.Advertise.current_head pv.net_db ~mempool pv.predecessor

let rec worker_loop pv =
  begin
    handle_unprocessed pv >>= fun () ->
    Lwt_utils.protect ~canceler:pv.canceler begin fun () ->
      Lwt_pipe.pop pv.message_queue >>= return
    end >>=? fun (Message (message, u)) ->
    wakeup_with_result message u @@ function
    | Flush block ->
        on_advertise pv ;
        (* TODO: rebase the advertisement instead *)
        on_flush pv block >>=? fun () ->
        return ()
    | Notify (peer, mempool) ->
        on_notify pv peer mempool ;
        return ()
    | Inject op ->
        on_inject pv op
    | Arrived (oph, op) ->
        on_operation_arrived pv oph op ;
        return ()
    | Advertise ->
        on_advertise pv ;
        return ()
  end >>= function
  | Ok () ->
      worker_loop pv
  | Error [Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      clear_fetching pv ;
      close_queue pv ;
      Lwt.return_unit
  | Error err ->
      lwt_log_error "@[Unexpected error:@ %a@]"
        pp_print_error err >>= fun () ->
      close_queue pv ;
      clear_fetching pv ;
      Lwt_canceler.cancel pv.canceler >>= fun () ->
      Lwt.return_unit

let create limits net_db =
  let net_state = Distributed_db.net_state net_db in
  let canceler = Lwt_canceler.create () in
  let message_queue = Lwt_pipe.create () in
  State.read_chain_store net_state
    (fun _ { current_head ; current_mempool ; live_blocks ; live_operations } ->
       Lwt.return (current_head, current_mempool, live_blocks, live_operations))
  >>= fun (predecessor, mempool, live_blocks, live_operations) ->
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
    { limits ; net_db ; canceler ;
      worker = Lwt.return_unit ; message_queue ;
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
    (fun oph -> Lwt.ignore_result (fetch_operation pv oph))
    mempool.known_valid ;
  pv.worker <-
    Lwt_utils.worker
      (Format.asprintf "net_prevalidator.%a" Net_id.pp (State.Net.id net_state))
      ~run:(fun () -> worker_loop pv)
      ~cancel:(fun () -> Lwt_canceler.cancel pv.canceler) ;
  Lwt.return pv

let shutdown pv =
  lwt_debug "shutdown" >>= fun () ->
  Lwt_canceler.cancel pv.canceler >>= fun () ->
  pv.worker

let flush pv head =
  push_request pv (Flush head)

let notify_operations pv peer mempool =
  push_request pv (Notify (peer, mempool))

let operations pv =
  { pv.validation_result with
    applied = List.rev pv.validation_result.applied },
  pv.pending

let pending ?block pv =
  let ops = Preapply_result.operations pv.validation_result in
  match block with
  | Some to_block ->
      list_pendings
        ~maintain_net_db:pv.net_db
        ~from_block:pv.predecessor ~to_block ops
  | None -> Lwt.return ops

let timestamp pv = pv.timestamp

let context pv =
  Lwt.return pv.validation_state >>=? fun validation_state ->
  Prevalidation.end_prevalidation validation_state

let inject_operation pv op =
  push_request_and_wait pv (Inject op) >>=? fun result ->
  Lwt.return result
