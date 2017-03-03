(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

 open Logging.Node.Prevalidator

let preapply
    net_db ctxt (module Proto : Updater.REGISTRED_PROTOCOL)
    block timestamp sort ops =
  lwt_debug "-> prevalidate (%d)" (List.length ops) >>= fun () ->
  (* The operations list length is bounded by the size of the mempool,
     where eventually an operation should not stay more than one hours. *)
  Lwt_list.map_p
    (fun h ->
       Distributed_db.Operation.read net_db h >>= function
       | None -> Lwt.return_none
       | Some op ->
           match Proto.parse_operation h op with
           | Error _ ->
               (* the operation will never be validated in the
                  current context, it is silently ignored. It may be
                  reintroduced in the loop by the next `flush`. *)
               Lwt.return_none
           | Ok p -> Lwt.return (Some p))
    ops >>= fun ops ->
  Context.set_timestamp ctxt timestamp >>= fun ctxt ->
  Proto.preapply ctxt block sort (Utils.unopt_list ops) >>= function
  | Ok (ctxt, r)  ->
      lwt_debug "<- prevalidate (%d/%d/%d/%d)"
        (List.length r.Updater.applied)
        (Operation_hash.Map.cardinal r.Updater.refused)
        (Operation_hash.Map.cardinal r.Updater.branch_refused)
        (Operation_hash.Map.cardinal r.Updater.branch_delayed) >>= fun () ->
      Lwt.return (Ok (ctxt, r))
  | Error errors ->
      (* FIXME report internal error *)
      lwt_debug "<- prevalidate (internal error)" >>= fun () ->
      Lwt.return (Error errors)

let list_pendings net_db ~from_block ~to_block old_mempool =
  let rec pop_blocks ancestor hash mempool =
    if Block_hash.equal hash ancestor then
      Lwt.return mempool
    else
      Distributed_db.Block_header.read_exn net_db hash >>= fun { shell } ->
      let mempool =
        List.fold_left
          (fun mempool h -> Operation_hash.Set.add h mempool)
          mempool shell.operations in
      pop_blocks ancestor shell.predecessor mempool
  in
  let push_block mempool (_hash, shell) =
    List.fold_left
      (fun mempool h -> Operation_hash.Set.remove h mempool)
      mempool shell.Store.Block_header.operations
  in
  let net_state = Distributed_db.state net_db in
  State.Valid_block.Current.new_blocks
    net_state ~from_block ~to_block >>= fun (ancestor, path) ->
  pop_blocks ancestor from_block.hash old_mempool >>= fun mempool ->
  let new_mempool = List.fold_left push_block mempool path in
  Lwt.return new_mempool


(** Worker *)

exception Invalid_operation of Operation_hash.t

type t = {
  net_db: Distributed_db.net ;
  flush: State.Valid_block.t -> unit;
  notify_operation: P2p.Peer_id.t -> Operation_hash.t -> unit ;
  prevalidate_operations:
    bool -> Store.Operation.t list ->
    (Operation_hash.t list * error Updater.preapply_result) tzresult Lwt.t ;
  operations: unit -> error Updater.preapply_result * Operation_hash.Set.t ;
  pending: ?block:State.Valid_block.t -> unit -> Operation_hash.Set.t Lwt.t ;
  timestamp: unit -> Time.t ;
  context: unit -> Context.t ;
  protocol: unit -> (module Updater.REGISTRED_PROTOCOL) ;
  shutdown: unit -> unit Lwt.t ;
}


let merge _key a b =
  match a, b with
  | None, None -> None
  | Some x, None -> Some x
  | _, Some y -> Some y

let create net_db =

  let net_state = Distributed_db.state net_db in

  let cancelation, cancel, _on_cancel = Lwt_utils.canceler () in
  let push_to_worker, worker_waiter = Lwt_utils.queue () in

  State.Valid_block.Current.head net_state >>= fun head ->
  State.Valid_block.Current.protocol net_state >>= fun protocol ->
  State.Operation.list_pending net_state >>= fun initial_mempool ->
  let timestamp = ref (Time.now ()) in
  begin
    let (module Proto) = protocol in
    Context.set_timestamp head.context !timestamp >>= fun ctxt ->
    Proto.preapply ctxt head.hash false [] >|= function
    | Error _ -> ref head.context
    | Ok (ctxt, _) -> ref ctxt
  end >>= fun context ->
  let protocol = ref protocol in
  let head = ref head in
  let operations = ref Updater.empty_result in
  let running_validation = ref Lwt.return_unit in
  let unprocessed = ref initial_mempool in
  let broadcast_unprocessed = ref false in

  let set_context ctxt =
    context := ctxt;
    Lwt.return_unit in

  let broadcast_operation ops =
    Distributed_db.broadcast_head net_db !head.hash ops in

  let handle_unprocessed () =
    if Operation_hash.Set.is_empty !unprocessed then
      Lwt.return ()
    else
      (* We assume that `!unprocessed` does not contain any operations
         from `!operations`. *)
      let ops = !unprocessed in
      let broadcast = !broadcast_unprocessed in
      unprocessed := Operation_hash.Set.empty ;
      broadcast_unprocessed := false ;
      running_validation := begin
        begin
          preapply
            net_db !context !protocol !head.hash !timestamp true
            (Operation_hash.Set.elements ops) >>= function
          | Ok (ctxt, r) -> Lwt.return (ctxt, r)
          | Error err ->
              let r =
                { Updater.empty_result with
                  branch_delayed =
                    Operation_hash.Set.fold
                      (fun op m -> Operation_hash.Map.add op err m)
                      ops Operation_hash.Map.empty ; } in
              Lwt.return (!context, r)
        end >>= fun (ctxt, r) ->
        let filter_out s m =
          List.fold_right Operation_hash.Map.remove s m in
        operations := {
          Updater.applied = List.rev_append r.applied !operations.applied ;
             refused = Operation_hash.Map.empty ;
             branch_refused =
               Operation_hash.Map.merge merge
                 (* filter_out should not be required here, TODO warn ? *)
                 (filter_out r.applied !operations.branch_refused)
                 r.branch_refused ;
             branch_delayed =
               Operation_hash.Map.merge merge
                 (filter_out r.applied !operations.branch_delayed)
                 r.branch_delayed ;
        } ;
        if broadcast then broadcast_operation r.Updater.applied ;
        Lwt_list.iter_s
          (fun (_op, _exns) ->
             (* FIXME *)
             (* Distributed_db.Operation.mark_invalid net_db op exns >>= fun _ -> *)
             Lwt.return_unit)
          (Operation_hash.Map.bindings r.Updater.refused) >>= fun () ->
        (* TODO. Keep a bounded set of 'refused' operations. *)
        (* TODO. Log the error in some statistics associated to
                 the peers that informed us of the operations. And
                 eventually blacklist bad peers. *)
        (* TODO. Keep a bounded set of 'branch_refused' operations
                 into the 'state'. It should be associated to the
                 current block, and updated on 'set_current_head'. *)
        set_context ctxt
      end;
      Lwt.catch
        (fun () -> !running_validation)
        (fun _ -> lwt_debug "<- prevalidate (cancel)")
  in

  let prevalidation_worker =

    let rec worker_loop () =
      (* TODO cleanup the mempool from outdated operation (1h like
              Bitcoin ?). And log the removal in some statistic associated
              to then peers that informed us of the operation. *)
      (* TODO lookup in `!pending` for 'outdated' ops and re-add them
              in `unprocessed` (e.g. if the previous tentative was
              more 5 seconds ago) *)
      handle_unprocessed () >>= fun () ->
      Lwt.pick [(worker_waiter () >|= fun q -> `Process q);
                (cancelation () >|= fun () -> `Cancel)] >>= function
      | `Cancel -> Lwt.return_unit
      | `Process q ->
          Lwt_list.iter_s
            (function
              | `Prevalidate (ops, w, force) -> begin
                  let (module Proto) = !protocol in
                  let result =
                    map_s (fun (h, b) ->
                        Distributed_db.Operation.known net_db h >>= function
                        | true ->
                            failwith
                              "Previously injected operation %a"
                              Operation_hash.pp_short h
                        | false ->
                            Lwt.return
                              (Proto.parse_operation h b
                               |> record_trace_exn (Invalid_operation h)))
                      (Operation_hash.Map.bindings ops) >>=? fun parsed_ops ->
                    Context.set_timestamp !context (Time.now ()) >>= fun ctxt ->
                    Proto.preapply
                      ctxt !head.hash true parsed_ops >>=? fun (ctxt, res) ->
                    let register h =
                      let op = Operation_hash.Map.find h ops in
                      Distributed_db.Operation.inject
                        net_db h op >>= fun _ ->
                      Lwt.return_unit in
                    Lwt_list.iter_s
                      (fun h ->
                         register h >>= fun () ->
                         operations :=
                           { !operations with
                             applied = h :: !operations.applied };
                         Lwt.return_unit )
                      res.Updater.applied >>= fun () ->
                    broadcast_operation res.Updater.applied ;
                    begin
                      if force then
                        Lwt_list.iter_p
                          (fun (h, _exns) -> register h)
                          (Operation_hash.Map.bindings
                             res.Updater.branch_delayed) >>= fun () ->
                        Lwt_list.iter_p
                          (fun (h, _exns) -> register h)
                          (Operation_hash.Map.bindings
                             res.Updater.branch_refused) >>= fun () ->
                        operations :=
                          { !operations with
                            branch_delayed =
                              Operation_hash.Map.merge merge
                                !operations.branch_delayed res.branch_delayed ;
                            branch_refused =
                              Operation_hash.Map.merge merge
                                !operations.branch_refused res.branch_refused ;
                          } ;
                        Lwt.return_unit
                      else
                        Lwt.return_unit
                    end >>= fun () ->
                    set_context ctxt >>= fun () ->
                    return res
                  in
                  result >>= fun result ->
                  Lwt.wakeup w result ;
                  Lwt.return_unit
                end
              | `Register op ->
                  lwt_debug "register %a" Operation_hash.pp_short op >>= fun () ->
                  broadcast_unprocessed := true ;
                  unprocessed := Operation_hash.Set.singleton op ;
                  Lwt.return_unit
              | `Flush (new_head : State.Valid_block.t) ->
                  let new_protocol =
                    match new_head.protocol with
                    | None ->
                        assert false (* FIXME, this should not happen! *)
                    | Some protocol -> protocol in
                  list_pendings
                    net_db ~from_block:!head ~to_block:new_head
                    (Updater.operations !operations) >>= fun new_mempool ->
                  lwt_debug "flush %a (mempool: %d)"
                    Block_hash.pp_short new_head.hash
                    (Operation_hash.Set.cardinal new_mempool) >>= fun () ->
                  (* Reset the pre-validation context *)
                  head := new_head ;
                  protocol := new_protocol ;
                  operations := Updater.empty_result ;
                  broadcast_unprocessed := false ;
                  unprocessed := new_mempool ;
                  timestamp := Time.now () ;
                  (* Tag the context as a prevalidation context. *)
                  let (module Proto) = new_protocol in
                  Context.set_timestamp
                    new_head.context !timestamp >>= fun ctxt ->
                  Proto.preapply
                    ctxt new_head.hash false [] >>= function
                  | Error _ -> set_context new_head.context
                  | Ok (ctxt, _) -> set_context ctxt)
            q >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker "prevalidator" ~run:worker_loop ~cancel in

  let flush head =
    push_to_worker (`Flush head) ;
    if not (Lwt.is_sleeping !running_validation) then
      Lwt.cancel !running_validation
  in
  let notify_operation gid op =
    Lwt.async begin fun () ->
      Distributed_db.Operation.fetch net_db ~peer:gid op >>= fun _ ->
      push_to_worker (`Register op) ;
      Lwt.return_unit
    end in
  let prevalidate_operations force raw_ops =
    let ops = List.map Store.Operation.hash raw_ops in
    let ops_map =
      List.fold_left
        (fun map op ->
           Operation_hash.Map.add (Store.Operation.hash op) op map)
        Operation_hash.Map.empty raw_ops in
    let wait, waker = Lwt.wait () in
    push_to_worker (`Prevalidate (ops_map, waker, force));
    wait >>=? fun result ->
    return (ops, result) in
  let shutdown () =
    lwt_debug "shutdown" >>= fun () ->
    if not (Lwt.is_sleeping !running_validation) then
      Lwt.cancel !running_validation;
    cancel () >>= fun () ->
    prevalidation_worker in

  let pending ?block () =
    let ops = Updater.operations !operations in
    match block with
    | None -> Lwt.return ops
    | Some to_block ->
        list_pendings net_db ~from_block:!head ~to_block ops
  in
  Lwt.return {
    net_db ;
    flush ;
    notify_operation ;
    prevalidate_operations ;
    operations =
      (fun () ->
         { !operations with applied = List.rev !operations.applied },
         !unprocessed) ;
    pending ;
    timestamp = (fun () -> !timestamp) ;
    context = (fun () -> !context) ;
    protocol = (fun () -> !protocol) ;
    shutdown ;
  }

let flush pv head = pv.flush head
let notify_operation pv = pv.notify_operation
let prevalidate_operations pv = pv.prevalidate_operations
let operations pv = pv.operations ()
let pending ?block pv = pv.pending ?block ()
let timestamp pv = pv.timestamp ()
let context pv = pv.context ()
let protocol pv = pv.protocol ()
let shutdown pv = pv.shutdown ()

let inject_operation pv ?(force = false) (op: Store.Operation.t) =
  let net_id = State.Net.id (Distributed_db.state pv.net_db) in
  let wrap_error h map =
    begin
      try return (Operation_hash.Map.find h map)
      with Not_found ->
        failwith "unexpected protocol result"
    end >>=? fun errors ->
    Lwt.return (Error errors) in
  fail_unless (Store.Net_id.equal net_id op.shell.net_id)
    (Unclassified
       "Prevalidator.inject_operation: invalid network") >>=? fun () ->
  pv.prevalidate_operations force [op] >>=? function
  | ([h], { Updater.applied = [h'] }) when Operation_hash.equal h h' ->
      return ()
  | ([h], { Updater.refused })
    when Operation_hash.Map.cardinal refused = 1 ->
      wrap_error h refused
  | ([h], { Updater.branch_refused })
    when Operation_hash.Map.cardinal branch_refused = 1 && not force  ->
      wrap_error h branch_refused
  | ([h], { Updater.branch_delayed })
    when Operation_hash.Map.cardinal branch_delayed = 1 && not force  ->
      wrap_error h branch_delayed
  | _ ->
      if force then
        return ()
      else
        failwith "Unexpected result for prevalidation."
