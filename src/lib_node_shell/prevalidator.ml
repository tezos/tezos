(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Prevalidator
open Preapply_result

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


(** Worker *)

open Prevalidation

type t = {
  net_db: Distributed_db.net_db ;
  flush: State.Block.t -> unit;
  notify_operations: P2p.Peer_id.t -> Mempool.t -> unit ;
  prevalidate_operations:
    bool -> Operation.t list ->
    (Operation_hash.t list * error Preapply_result.t) tzresult Lwt.t ;
  operations: unit -> error Preapply_result.t * Operation.t Operation_hash.Map.t ;
  pending: ?block:State.Block.t -> unit -> Operation.t Operation_hash.Map.t Lwt.t ;
  timestamp: unit -> Time.t ;
  context: unit -> Updater.validation_result tzresult Lwt.t ;
  shutdown: unit -> unit Lwt.t ;
}

let merge _key a b =
  match a, b with
  | None, None -> None
  | Some x, None -> Some x
  | _, Some y -> Some y

let create
    ~max_operations
    ~operation_timeout
    net_db =

  let net_state = Distributed_db.net_state net_db in

  let cancelation, cancel, _on_cancel = Lwt_utils.canceler () in
  let push_to_worker, worker_waiter = Lwt_utils.queue () in

  Chain.head net_state >>= fun head ->
  let timestamp = ref (Time.now ()) in
  let max_number_of_operations =
    try 2 * List.hd (State.Block.max_number_of_operations head)
    with _ -> 0 in
  (start_prevalidation
     ~max_number_of_operations
     ~predecessor:head
     ~timestamp:!timestamp () >|= ref) >>= fun validation_state ->
  let pending = Operation_hash.Table.create 53 in
  let head = ref head in
  let mempool = ref Mempool.empty in
  let operations = ref Preapply_result.empty in
  let operation_count = ref 0 in (* unprocessed + operations/mempool *)
  Chain_traversal.live_blocks
    !head
    (State.Block.max_operations_ttl !head)
  >>= fun (live_blocks, live_operations) ->
  let live_blocks = ref live_blocks in
  let live_operations = ref live_operations in
  let running_validation = ref Lwt.return_unit in
  let unprocessed = ref Operation_hash.Map.empty in
  let broadcast_unprocessed = ref false in

  let set_validation_state state =
    validation_state := state;
    Lwt.return_unit in

  let reset_validation_state head timestamp =
    start_prevalidation ~predecessor:head ~timestamp () >>= fun state ->
    validation_state := state;
    Lwt.return_unit in

  let broadcast_new_operations r =
    Distributed_db.Advertise.current_head
      net_db
      ~mempool:{
        known_valid = [] ;
        pending =
          List.fold_right
            (fun (k, _) s -> Operation_hash.Set.add k s)
            r.applied @@
          Operation_hash.Map.fold
            (fun k _ s -> Operation_hash.Set.add k s)
            r.branch_delayed @@
          Operation_hash.Map.fold
            (fun k _ s -> Operation_hash.Set.add k s)
            r.branch_refused @@
          Operation_hash.Set.empty ;
      }
      !head
  in

  let handle_unprocessed () =
    if Operation_hash.Map.is_empty !unprocessed then
      Lwt.return ()
    else
      let ops = !unprocessed in
      let broadcast = !broadcast_unprocessed in
      unprocessed := Operation_hash.Map.empty ;
      broadcast_unprocessed := false ;
      let ops =
        Operation_hash.Set.fold
          (fun k m -> Operation_hash.Map.remove k m)
          !live_operations ops in
      live_operations :=
        Operation_hash.Map.fold
          (fun k _ m -> Operation_hash.Set.add k m)
          ops !live_operations ;
      running_validation := begin
        begin
          Lwt_list.filter_map_p
            (fun (h, op) ->
               if Block_hash.Set.mem op.Operation.shell.branch !live_blocks then
                 Lwt.return_some (h, op)
               else begin
                 Distributed_db.Operation.clear_or_cancel net_db h ;
                 Lwt.return_none
               end)
            (Operation_hash.Map.bindings ops) >>= fun rops ->
          operation_count :=
            !operation_count - Operation_hash.Map.cardinal ops + List.length rops ;
          match !validation_state with
          | Ok validation_state ->
              prevalidate validation_state ~sort:true rops >>= fun (state, r) ->
              Lwt.return (Ok state, r)
          | Error err ->
              let r =
                { Preapply_result.empty with
                  branch_delayed =
                    List.fold_left
                      (fun m (h, op) -> Operation_hash.Map.add h (op, err) m)
                      Operation_hash.Map.empty rops ; } in
              Lwt.return (!validation_state, r)
        end >>= fun (state, r) ->
        let filter_out s m =
          List.fold_right (fun (h, _op) -> Operation_hash.Set.remove h) s m in
        mempool := {
          known_valid = !mempool.known_valid @ List.rev_map fst r.applied ;
          pending =
            Operation_hash.Map.fold
              (fun k _ s -> Operation_hash.Set.add k s)
              r.branch_delayed @@
            Operation_hash.Map.fold
              (fun k _ s -> Operation_hash.Set.add k s)
              r.branch_refused @@
            filter_out r.applied !mempool.pending ;
        } ;
        let filter_out s m =
          List.fold_right (fun (h, _op) -> Operation_hash.Map.remove h) s m in
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
        Mempool.set net_state
          ~head:(State.Block.hash !head) !mempool >>= fun () ->
        if broadcast then broadcast_new_operations r ;
        Lwt_list.iter_s
          (fun (op, _exns) ->
             Distributed_db.Operation.clear_or_cancel net_db op ;
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
                    let register h op =
                      incr operation_count ;
                      live_operations :=
                        Operation_hash.Set.add h !live_operations ;
                      Distributed_db.inject_operation
                        net_db h op >>=? fun (_ : bool) ->
                      return () in
                    iter_s
                      (fun (h, op) ->
                         register h op >>=? fun () ->
                         mempool := { !mempool with
                                      known_valid =
                                        !mempool.known_valid @ [h] } ;
                         operations :=
                           { !operations with
                             applied = (h, op) :: !operations.applied } ;
                         return () )
                      res.applied >>=? fun () ->
                    Mempool.set net_state
                      ~head:(State.Block.hash !head) !mempool >>= fun () ->
                    broadcast_new_operations res ;
                    begin
                      if force then
                        iter_p
                          (fun (h, (op, _exns)) -> register h op)
                          (Operation_hash.Map.bindings
                             res.branch_delayed) >>=? fun () ->
                        iter_p
                          (fun (h, (op, _exns)) -> register h op)
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
              | `Register (_gid, _mempool) when !operation_count >= max_operations ->
                  Lwt.return_unit
              | `Register (gid, mempool) ->
                  let ops =
                    Operation_hash.Set.elements mempool.Mempool.pending @
                    mempool.known_valid in
                  let known_ops, unknown_ops =
                    List.partition
                      (fun op ->
                         Operation_hash.Table.mem pending op
                         || Operation_hash.Set.mem op !live_operations)
                      ops in
                  let fetch h =
                    Distributed_db.Operation.fetch
                      ~timeout:operation_timeout
                      net_db ~peer:gid h () >>= function
                    | Ok op ->
                        push_to_worker (`Handle (h, op)) ;
                        Lwt.return_unit
                    | Error [ Distributed_db.Operation.Canceled _ ] ->
                        lwt_debug
                          "operation %a included before being prevalidated"
                          Operation_hash.pp_short h >>= fun () ->
                        Operation_hash.Table.remove pending h ;
                        Lwt.return_unit
                    | Error _ ->
                        Operation_hash.Table.remove pending h ;
                        Lwt.return_unit
                  in
                  List.iter
                    (fun op -> Operation_hash.Table.add pending op (fetch op))
                    unknown_ops ;
                  List.iter
                    (fun op ->
                       Lwt.ignore_result
                         (Distributed_db.Operation.fetch
                            ~timeout:operation_timeout
                            net_db ~peer:gid op ()))
                    known_ops ;
                  Lwt.return_unit
              | `Handle (h, op) ->
                  Operation_hash.Table.remove pending h ;
                  if !operation_count < max_operations then begin
                    broadcast_unprocessed := true ;
                    incr operation_count ;
                    unprocessed := Operation_hash.Map.singleton h op ;
                    lwt_debug "register %a" Operation_hash.pp_short h >>= fun () ->
                    Lwt.return_unit
                  end else begin
                    Distributed_db.Operation.clear_or_cancel net_db h ;
                    Lwt.return_unit
                  end
              | `Flush (new_head : State.Block.t) ->
                  list_pendings
                    ~maintain_net_db:net_db
                    ~from_block:!head ~to_block:new_head
                    (Preapply_result.operations !operations) >>= fun new_mempool ->
                  Chain_traversal.live_blocks
                    new_head
                    (State.Block.max_operations_ttl new_head)
                  >>= fun (new_live_blocks, new_live_operations) ->
                  lwt_debug "flush %a (mempool: %d)"
                    Block_hash.pp_short (State.Block.hash new_head)
                    (Operation_hash.Map.cardinal new_mempool) >>= fun () ->
                  (* Reset the pre-validation context *)
                  head := new_head ;
                  mempool := Mempool.empty ;
                  operations := Preapply_result.empty ;
                  broadcast_unprocessed := false ;
                  unprocessed := new_mempool ;
                  operation_count := Operation_hash.Map.cardinal new_mempool ;
                  timestamp := Time.now () ;
                  live_blocks := new_live_blocks ;
                  live_operations := new_live_operations ;
                  (* Reset the prevalidation context. *)
                  reset_validation_state new_head !timestamp)
            q >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker
      (Format.asprintf "prevalidator.%a"
         Net_id.pp (State.Net.id net_state))
      ~run:worker_loop ~cancel in

  let flush head =
    push_to_worker (`Flush head) ;
    if not (Lwt.is_sleeping !running_validation) then
      Lwt.cancel !running_validation
  in
  let notify_operations gid mempool =
    Lwt.async begin fun () ->
      push_to_worker (`Register (gid, mempool)) ;
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
    let ops = Preapply_result.operations !operations in
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
  let wrap_error h map =
    begin
      try return (snd (Operation_hash.Map.find h map))
      with Not_found ->
        failwith "unexpected protocol result"
    end >>=? fun errors ->
    Lwt.return (Error errors) in
  pv.prevalidate_operations force [op] >>=? function
  | ([h], { applied = [h', _] }) when Operation_hash.equal h h' ->
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
