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
    st ctxt (module Proto : Updater.REGISTRED_PROTOCOL) block timestamp sort ops =
  lwt_debug "-> prevalidate (%d)" (List.length ops) >>= fun () ->
  (* The operations list length is bounded by the size of the mempool,
     where eventually an operation should not stay more than one hours. *)
  Lwt_list.map_p
    (fun h ->
       State.Operation.read st h >>= function
       | None | Some { data = Error _ } ->
           Lwt.return_none
       | Some { data = Ok op } ->
           match Proto.parse_operation h op with
           | Error _ ->
               (* the operation will never be validated in the
                  current context, it is silently ignored. It may be
                  reintroduced in the loop by the next `flush`. *)
               Lwt.return_none
           | Ok p -> Lwt.return (Some p))
    ops >>= fun ops ->
  Proto.preapply ctxt block timestamp sort (Utils.unopt_list ops) >>= function
  | Ok (ctxt, r)  ->
      lwt_debug "<- prevalidate (%d/%d/%d/%d)"
        (List.length r.Updater.applied)
        (Operation_hash_map.cardinal r.Updater.refused)
        (Operation_hash_map.cardinal r.Updater.branch_refused)
        (Operation_hash_map.cardinal r.Updater.branch_delayed) >>= fun () ->
      Lwt.return (Ok (ctxt, r))
  | Error errors ->
      (* FIXME report internal error *)
      lwt_debug "<- prevalidate (internal error)" >>= fun () ->
      Lwt.return (Error errors)


(** Worker *)

exception Invalid_operation of Operation_hash.t

type t = {
  net: State.Net.t ;
  flush: unit -> unit;
  register_operation: Operation_hash.t -> unit ;
  prevalidate_operations:
    bool -> Store.operation list ->
    (Operation_hash.t list * error Updater.preapply_result) tzresult Lwt.t ;
  operations: unit -> error Updater.preapply_result * Operation_hash_set.t ;
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

let create p2p net =

  let st = State.Net.state net in

  let cancelation, cancel, _on_cancel = Lwt_utils.canceler () in
  let push_to_worker, worker_waiter = Lwt_utils.queue () in

  State.Net.Blockchain.head net >>= fun head ->
  State.Net.Blockchain.protocol net >>= fun protocol ->
  State.Net.Mempool.get net >>= fun mempool ->
  let timestamp = ref (Time.now ()) in
  begin
    let (module Proto) = protocol in
    Proto.preapply head.context head.hash !timestamp false [] >|= function
    | Error _ -> ref head.context
    | Ok (ctxt, _) -> ref ctxt
  end >>= fun context ->
  let protocol = ref protocol in
  let head = ref head.hash in
  let operations = ref Updater.empty_result in
  let running_validation = ref Lwt.return_unit in
  let unprocessed = ref mempool in
  let broadcast_unprocessed = ref false in

  let set_context ctxt =
    context := ctxt;
    Lwt.return_unit in

  let broadcast_operation ops =
    P2p.broadcast
      Messages.(to_frame @@ Operation_inventory (State.Net.id net, ops))
      p2p in

  let handle_unprocessed () =
    if Operation_hash_set.is_empty !unprocessed then
      Lwt.return ()
    else
      (* We assume that `!unprocessed` does not contain any operations
         from `!operations`. *)
      let ops = !unprocessed in
      let broadcast = !broadcast_unprocessed in
      unprocessed := Operation_hash_set.empty ;
      broadcast_unprocessed := false ;
      running_validation := begin
        begin
          preapply
            st !context !protocol !head !timestamp true
            (Operation_hash_set.elements ops) >>= function
          | Ok (ctxt, r) -> Lwt.return (ctxt, r)
          | Error err ->
              let r =
                { Updater.empty_result with
                  branch_delayed =
                    Operation_hash_set.fold
                      (fun op m -> Operation_hash_map.add op err m)
                      ops Operation_hash_map.empty ; } in
              Lwt.return (!context, r)
        end >>= fun (ctxt, r) ->
        let filter_out s m =
          List.fold_right Operation_hash_map.remove s m in
        operations := {
          Updater.applied = List.rev_append r.applied !operations.applied ;
             refused = Operation_hash_map.empty ;
             branch_refused =
               Operation_hash_map.merge merge
                 (* filter_out should not be required here, TODO warn ? *)
                 (filter_out r.applied !operations.branch_refused)
                 r.branch_refused ;
             branch_delayed =
               Operation_hash_map.merge merge
                 (filter_out r.applied !operations.branch_delayed)
                 r.branch_delayed ;
        } ;
        (* Update the Mempool *)
        Lwt_list.iter_s
          (fun op ->
             State.Net.Mempool.add net op >>= fun _ ->
             Lwt.return_unit)
          r.Updater.applied >>= fun () ->
        if broadcast then broadcast_operation r.Updater.applied ;
        Lwt_list.iter_s
             (fun (op, _exns) ->
              State.Net.Mempool.add net op >>= fun _ ->
              Lwt.return_unit)
             (Operation_hash_map.bindings r.Updater.branch_delayed) >>= fun () ->
        Lwt_list.iter_s
          (fun (op, _exns) ->
             State.Net.Mempool.add net op >>= fun _ ->
             Lwt.return_unit)
          (Operation_hash_map.bindings r.Updater.branch_refused) >>= fun () ->
        Lwt_list.iter_s
          (fun (op, exns) ->
             State.Net.Mempool.remove net op >>= fun _ ->
             State.Operation.mark_invalid st op exns >>= fun _ ->
             Lwt.return_unit)
          (Operation_hash_map.bindings r.Updater.refused) >>= fun () ->
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
                        State.Operation.known st h >>= function
                        | true ->
                            failwith
                              "Previously injected operation %a"
                              Operation_hash.pp_short h
                        | false ->
                            Lwt.return
                              (Proto.parse_operation h b
                               |> record_trace_exn (Invalid_operation h)))
                      (Operation_hash_map.bindings ops) >>=? fun parsed_ops ->
                    Proto.preapply
                      !context !head (Time.now ())
                      true parsed_ops >>=? fun (ctxt, res) ->
                    let register h =
                      let b =
                        Store.Operation.to_bytes @@
                        Operation_hash_map.find h ops in
                      State.Operation.(store st b) >>= fun _ ->
                      State.Net.Mempool.add net h >>= fun _ ->
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
                          (Operation_hash_map.bindings
                             res.Updater.branch_delayed) >>= fun () ->
                        Lwt_list.iter_p
                          (fun (h, _exns) -> register h)
                          (Operation_hash_map.bindings
                             res.Updater.branch_refused) >>= fun () ->
                        operations :=
                          { !operations with
                            branch_delayed =
                              Operation_hash_map.merge merge
                                !operations.branch_delayed res.branch_delayed ;
                            branch_refused =
                              Operation_hash_map.merge merge
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
                  unprocessed := Operation_hash_set.singleton op ;
                  Lwt.return_unit
              | `Flush ->
                  State.Net.Blockchain.head net >>= fun new_head ->
                  State.Net.Blockchain.protocol net >>= fun new_protocol ->
                  State.Net.Mempool.get net >>= fun new_mempool ->
                  lwt_debug "flush %a (mempool: %d)"
                    Block_hash.pp_short new_head.hash
                    (Operation_hash_set.cardinal new_mempool) >>= fun () ->
                  (* Reset the pre-validation context *)
                  head := new_head.hash ;
                  protocol := new_protocol ;
                  operations := Updater.empty_result;
                  broadcast_unprocessed := false ;
                  unprocessed := new_mempool;
                  timestamp := Time.now ();
                  (* Tag the context as a prevalidation context. *)
                  let (module Proto) = new_protocol in
                  Proto.preapply new_head.context
                    new_head.hash !timestamp false [] >>= function
                  | Error _ -> set_context new_head.context
                  | Ok (ctxt, _) -> set_context ctxt)
            q >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker "prevalidator" ~run:worker_loop ~cancel in

  let flush () =
    push_to_worker `Flush;
    if not (Lwt.is_sleeping !running_validation) then
      Lwt.cancel !running_validation
  in
  let register_operation op = push_to_worker (`Register op) in
  let prevalidate_operations force raw_ops =
    let ops = List.map Store.Operation.hash raw_ops in
    let ops_map =
      List.fold_left
        (fun map op ->
           Operation_hash_map.add (Store.Operation.hash op) op map)
        Operation_hash_map.empty raw_ops in
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

  Lwt.return {
    net ;
    flush ;
    register_operation ;
    prevalidate_operations ;
    operations =
      (fun () ->
         { !operations with applied = List.rev !operations.applied },
         !unprocessed) ;
    timestamp = (fun () -> !timestamp) ;
    context = (fun () -> !context) ;
    protocol = (fun () -> !protocol) ;
    shutdown ;
  }

let flush pv = pv.flush ()
let register_operation pv = pv.register_operation
let prevalidate_operations pv = pv.prevalidate_operations
let operations pv = pv.operations ()
let timestamp pv = pv.timestamp ()
let context pv = pv.context ()
let protocol pv = pv.protocol ()
let shutdown pv = pv.shutdown ()

let inject_operation pv ?(force = false) (op: Store.operation) =
  let State.Net net_id = op.shell.net_id
  and State.Net net_id' = State.Net.id pv.net in
  let wrap_error h map =
    begin
      try return (Operation_hash_map.find h map)
      with Not_found ->
        failwith "unexpected protocol result"
    end >>=? fun errors ->
    Lwt.return (Error errors) in
  fail_unless (Block_hash.equal net_id net_id')
    (Unclassified
       "Prevalidator.inject_operation: invalid network") >>=? fun () ->
  pv.prevalidate_operations force [op] >>=? function
  | ([h], { Updater.applied = [h'] }) when Operation_hash.equal h h' ->
      return ()
  | ([h], { Updater.refused })
    when Operation_hash_map.cardinal refused = 1 ->
      wrap_error h refused
  | ([h], { Updater.branch_refused })
    when Operation_hash_map.cardinal branch_refused = 1 && not force  ->
      wrap_error h branch_refused
  | ([h], { Updater.branch_delayed })
    when Operation_hash_map.cardinal branch_delayed = 1 && not force  ->
      wrap_error h branch_delayed
  | _ ->
      if force then
        return ()
      else
        failwith "Unexpected result for prevalidation."
