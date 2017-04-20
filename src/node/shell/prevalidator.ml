(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Prevalidator

let list_pendings ~from_block ~to_block old_mempool =
  let rec pop_blocks ancestor block mempool =
    let hash = State.Block.hash block in
    if Block_hash.equal hash ancestor then
      Lwt.return mempool
    else
      State.Block.all_operation_hashes block >>= fun operations ->
      let mempool =
        List.fold_left
          (List.fold_left (fun mempool h -> Operation_hash.Set.add h mempool))
          mempool operations in
      State.Block.predecessor block >>= function
      | None -> assert false
      | Some predecessor -> pop_blocks ancestor predecessor mempool
  in
  let push_block mempool block =
    State.Block.all_operation_hashes block >|= fun operations ->
    List.fold_left
      (List.fold_left (fun mempool h -> Operation_hash.Set.remove h mempool))
      mempool operations
  in
  Chain_traversal.new_blocks ~from_block ~to_block >>= fun (ancestor, path) ->
  pop_blocks
    (State.Block.hash ancestor)
    from_block old_mempool >>= fun mempool ->
  Lwt_list.fold_left_s push_block mempool path >>= fun new_mempool ->
  Lwt.return new_mempool


(** Worker *)

exception Invalid_operation of Operation_hash.t

open Prevalidation

type t = {
  net_db: Distributed_db.net_db ;
  flush: State.Block.t -> unit;
  notify_operations: P2p.Peer_id.t -> Operation_hash.t list -> unit ;
  prevalidate_operations:
    bool -> Operation.t list ->
    (Operation_hash.t list * error preapply_result) tzresult Lwt.t ;
  operations: unit -> error preapply_result * Operation_hash.Set.t ;
  pending: ?block:State.Block.t -> unit -> Operation_hash.Set.t Lwt.t ;
  timestamp: unit -> Time.t ;
  context: unit -> Updater.validation_result tzresult Lwt.t ;
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

  Chain.head net_state >>= fun head ->
  let timestamp = ref (Time.now ()) in
  (start_prevalidation head !timestamp () >|= ref) >>= fun validation_state ->
  let pending = Operation_hash.Table.create 53 in
  let head = ref head in
  let operations = ref empty_result in
  Chain_traversal.live_blocks
    !head
    (State.Block.max_operations_ttl !head)
    >>= fun (live_blocks, live_operations) ->
  let live_blocks = ref live_blocks in
  let live_operations = ref live_operations in
  let running_validation = ref Lwt.return_unit in
  let unprocessed = ref Operation_hash.Set.empty in
  let broadcast_unprocessed = ref false in

  let set_validation_state state =
    validation_state := state;
    Lwt.return_unit in

  let reset_validation_state head timestamp =
    start_prevalidation head timestamp () >>= fun state ->
    validation_state := state;
    Lwt.return_unit in

  let broadcast_operation ops =
    let hash = State.Block.hash !head in
    Distributed_db.broadcast_head net_db hash ops in

  let handle_unprocessed () =
    if Operation_hash.Set.is_empty !unprocessed then
      Lwt.return ()
    else
      let ops = !unprocessed in
      let broadcast = !broadcast_unprocessed in
      unprocessed := Operation_hash.Set.empty ;
      broadcast_unprocessed := false ;
      let ops = Operation_hash.Set.diff ops !live_operations in
      live_operations := Operation_hash.Set.(fold add) !live_operations ops ;
      running_validation := begin
        begin
          Lwt_list.filter_map_p
            (fun h ->
               Distributed_db.Operation.read_opt net_db h >>= function
               | Some po when Block_hash.Set.mem po.shell.branch !live_blocks ->
                   (* FIXME add the operation on a bounded set of
                            to-be-ignored operations.*)
                   Distributed_db.Operation.clear net_db h ;
                   Lwt.return_some (h, po)
               | Some _ | None -> Lwt.return_none)
            (Operation_hash.Set.elements ops) >>= fun rops ->
          (Lwt.return !validation_state >>=? fun validation_state ->
           (prevalidate validation_state ~sort:true rops >>= return)) >>= function
          | Ok (state, r) -> Lwt.return (Ok state, r)
          | Error err ->
              let r =
                { empty_result with
                  branch_delayed =
                    Operation_hash.Set.fold
                      (fun op m -> Operation_hash.Map.add op err m)
                      ops Operation_hash.Map.empty ; } in
              Lwt.return (!validation_state, r)
        end >>= fun (state, r) ->
        let filter_out s m =
          List.fold_right Operation_hash.Map.remove s m in
        operations := {
          applied = List.rev_append r.applied !operations.applied ;
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
        if broadcast then broadcast_operation r.applied ;
        Lwt_list.iter_s
          (fun (_op, _exns) ->
             (* FIXME *)
             (* Distributed_db.Operation.mark_invalid net_db op exns >>= fun _ -> *)
             Lwt.return_unit)
          (Operation_hash.Map.bindings r.refused) >>= fun () ->
        (* TODO. Keep a bounded set of 'refused' operations. *)
        (* TODO. Log the error in some statistics associated to
                 the peers that informed us of the operations. And
                 eventually blacklist bad peers. *)
        (* TODO. Keep a bounded set of 'branch_refused' operations
                 into the 'state'. It should be associated to the
                 current block, and updated on 'set_current_head'. *)
        set_validation_state state
      end;
      Lwt.catch
        (fun () -> !running_validation)
        (fun _ -> lwt_debug "<- prevalidate (cancel)")
  in

  let prevalidation_worker =

    let rec worker_loop () =
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
                  let result =
                    let rops = Operation_hash.Map.bindings ops in
                    Lwt.return !validation_state >>=? fun validation_state ->
                    prevalidate validation_state
                      ~sort:true rops >>= fun (state, res) ->
                    let register h =
                      let op = Operation_hash.Map.find h ops in
                      Distributed_db.inject_operation
                        net_db h op >>=? fun (_ : bool) ->
                      return () in
                    iter_s
                      (fun h ->
                         register h >>=? fun () ->
                         operations :=
                           { !operations with
                             applied = h :: !operations.applied };
                         return () )
                      res.applied >>=? fun () ->
                    broadcast_operation res.applied ;
                    begin
                      if force then
                        iter_p
                          (fun (h, _exns) -> register h)
                          (Operation_hash.Map.bindings
                             res.branch_delayed) >>=? fun () ->
                        iter_p
                          (fun (h, _exns) -> register h)
                          (Operation_hash.Map.bindings
                             res.branch_refused) >>=? fun () ->
                        operations :=
                          { !operations with
                            branch_delayed =
                              Operation_hash.Map.merge merge
                                !operations.branch_delayed res.branch_delayed ;
                            branch_refused =
                              Operation_hash.Map.merge merge
                                !operations.branch_refused res.branch_refused ;
                          } ;
                        return ()
                      else
                        return ()
                    end >>=? fun () ->
                    set_validation_state (Ok state) >>= fun () ->
                    return res
                  in
                  result >>= fun result ->
                  Lwt.wakeup w result ;
                  Lwt.return_unit
                end
              | `Register (gid, ops) ->
                  let known_ops, unknown_ops =
                    List.partition
                      (fun op ->
                         Operation_hash.Table.mem pending op
                         || Operation_hash.Set.mem op !live_operations)
                      ops in
                  let fetch op =
                    Distributed_db.Operation.fetch
                      net_db ~peer:gid op () >>= fun _op ->
                    push_to_worker (`Handle op) ;
                    Lwt.return_unit
                  in
                  List.iter
                    (fun op -> Operation_hash.Table.add pending op (fetch op))
                    unknown_ops ;
                  List.iter (fun op ->
                      Lwt.ignore_result
                        (Distributed_db.Operation.fetch net_db ~peer:gid op ()))
                    known_ops ;
                  Lwt.return_unit
              | `Handle op ->
                  lwt_debug "register %a" Operation_hash.pp_short op >>= fun () ->
                  Operation_hash.Table.remove pending op ;
                  broadcast_unprocessed := true ;
                  unprocessed := Operation_hash.Set.singleton op ;
                  lwt_debug "register %a" Operation_hash.pp_short op >>= fun () ->
                  Lwt.return_unit
              | `Flush (new_head : State.Block.t) ->
                  list_pendings ~from_block:!head ~to_block:new_head
                    (preapply_result_operations !operations) >>= fun new_mempool ->
                  Chain_traversal.live_blocks
                    new_head
                    (State.Block.max_operations_ttl new_head)
                  >>= fun (new_live_blocks, new_live_operations) ->
                  lwt_debug "flush %a (mempool: %d)"
                    Block_hash.pp_short (State.Block.hash new_head)
                    (Operation_hash.Set.cardinal new_mempool) >>= fun () ->
                  (* Reset the pre-validation context *)
                  head := new_head ;
                  operations := empty_result ;
                  broadcast_unprocessed := false ;
                  unprocessed := new_mempool ;
                  timestamp := Time.now () ;
                  live_blocks := new_live_blocks ;
                  live_operations := new_live_operations ;
                  (* Reset the prevalidation context. *)
                  reset_validation_state new_head !timestamp)
            q >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker "prevalidator" ~run:worker_loop ~cancel in

  let flush head =
    push_to_worker (`Flush head) ;
    if not (Lwt.is_sleeping !running_validation) then
      Lwt.cancel !running_validation
  in
  let notify_operations gid ops =
    Lwt.async begin fun () ->
      push_to_worker (`Register (gid, ops)) ;
      Lwt.return_unit
    end in
  let prevalidate_operations force raw_ops =
    let ops = List.map Operation.hash raw_ops in
    let ops_map =
      List.fold_left
        (fun map op ->
           Operation_hash.Map.add (Operation.hash op) op map)
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
    let ops = preapply_result_operations !operations in
    match block with
    | None -> Lwt.return ops
    | Some to_block -> list_pendings ~from_block:!head ~to_block ops in
  let context () =
    Lwt.return !validation_state >>=? fun prevalidation_state ->
    Prevalidation.end_prevalidation prevalidation_state in
  Lwt.return {
    net_db ;
    flush ;
    notify_operations ;
    prevalidate_operations ;
    operations =
      (fun () ->
         { !operations with applied = List.rev !operations.applied },
         !unprocessed) ;
    pending ;
    timestamp = (fun () -> !timestamp) ;
    context ;
    shutdown ;
  }

let flush pv head = pv.flush head
let notify_operations pv = pv.notify_operations
let prevalidate_operations pv = pv.prevalidate_operations
let operations pv = pv.operations ()
let pending ?block pv = pv.pending ?block ()
let timestamp pv = pv.timestamp ()
let context pv = pv.context ()
let shutdown pv = pv.shutdown ()

let inject_operation pv ?(force = false) (op: Operation.t) =
  let net_id = State.Net.id (Distributed_db.state pv.net_db) in
  let wrap_error h map =
    begin
      try return (Operation_hash.Map.find h map)
      with Not_found ->
        failwith "unexpected protocol result"
    end >>=? fun errors ->
    Lwt.return (Error errors) in
  fail_unless (Net_id.equal net_id op.shell.net_id)
    (failure
       "Prevalidator.inject_operation: invalid network") >>=? fun () ->
  pv.prevalidate_operations force [op] >>=? function
  | ([h], { applied = [h'] }) when Operation_hash.equal h h' ->
      return ()
  | ([h], { refused })
    when Operation_hash.Map.cardinal refused = 1 ->
      wrap_error h refused
  | ([h], { branch_refused })
    when Operation_hash.Map.cardinal branch_refused = 1 && not force  ->
      wrap_error h branch_refused
  | ([h], { branch_delayed })
    when Operation_hash.Map.cardinal branch_delayed = 1 && not force  ->
      wrap_error h branch_delayed
  | _ ->
      if force then
        return ()
      else
        failwith "Unexpected result for prevalidation."
