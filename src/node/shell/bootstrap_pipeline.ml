(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "node.validator.bootstrap_pipeline" end)
module Canceler = Lwt_utils.Canceler

type t = {
  canceler: Canceler.t ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  mutable headers_fetch_worker: unit Lwt.t ;
  mutable operations_fetch_worker: unit Lwt.t ;
  mutable validation_worker: unit Lwt.t ;
  peer_id: P2p.Peer_id.t ;
  net_db: Distributed_db.net_db ;
  locator: Block_locator.t ;
  block_validator: Block_validator.t ;
  notify_new_block: State.Block.t -> unit ;
  fetched_headers:
    (Block_hash.t * Block_header.t) Lwt_pipe.t ;
  fetched_blocks:
    (Block_hash.t * Block_header.t * Operation.t list list) Lwt_pipe.t ;
  (* HACK, a worker should be able to return the 'error'. *)
  mutable errors: Error_monad.error list ;
}

let fetch_step pipeline (step : Block_locator.step)  =
  lwt_log_info "fetching step %a -> %a (%d%s) from peer %a."
    Block_hash.pp_short step.block
    Block_hash.pp_short step.predecessor
    step.step
    (if step.strict_step then "" else " max")
    P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
  let rec fetch_loop acc hash cpt =
    Lwt_unix.yield () >>= fun () ->
    if cpt < 0 then
      lwt_log_info "invalid step from peer %a (too long)."
        P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
      fail (Block_locator.Invalid_locator
              (pipeline.peer_id, pipeline.locator))
    else if Block_hash.equal hash step.predecessor then
      if step.strict_step && cpt <> 0 then
        lwt_log_info "invalid step from peer %a (too short)."
          P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
        fail (Block_locator.Invalid_locator
                (pipeline.peer_id, pipeline.locator))
      else
        return acc
    else
      lwt_debug "fetching block header %a from peer %a."
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
      Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
        Distributed_db.Block_header.fetch
          ~timeout:pipeline.block_header_timeout
          pipeline.net_db ~peer:pipeline.peer_id
          hash ()
      end >>=? fun header ->
      lwt_debug "fetched block header %a from peer %a."
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
      fetch_loop ((hash, header) :: acc) header.shell.predecessor (cpt - 1)
  in
  fetch_loop [] step.block step.step >>=? fun headers ->
  iter_s
    begin fun header ->
      Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
        Lwt_pipe.push pipeline.fetched_headers header >>= return
      end
    end
    headers >>=? fun () ->
  return ()

let headers_fetch_worker_loop pipeline =
  begin
    let steps = Block_locator.to_steps pipeline.locator in
    iter_s (fetch_step pipeline) steps >>=? fun () ->
    return ()
  end >>= function
  | Ok () ->
      lwt_log_info "fetched all step from peer %a."
        P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
      Lwt_pipe.close pipeline.fetched_headers ;
      Lwt.return_unit
  | Error [Exn Lwt.Canceled | Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error "@[Unexpected error (headers fetch):@ %a@]"
        pp_print_error err >>= fun () ->
      Canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let rec operations_fetch_worker_loop pipeline =
  begin
    Lwt_unix.yield () >>= fun () ->
    Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.pop pipeline.fetched_headers >>= return
    end >>=? fun (hash, header) ->
    lwt_log_info "fetching operations of block %a from peer %a."
      Block_hash.pp_short hash
      P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
    map_p
      (fun i ->
         Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
           Distributed_db.Operations.fetch
             ~timeout:pipeline.block_operations_timeout
             pipeline.net_db ~peer:pipeline.peer_id
             (hash, i) header.shell.operations_hash
         end)
      (0 -- (header.shell.validation_passes - 1)) >>=? fun operations ->
    lwt_log_info "fetched operations of block %a from peer %a."
      Block_hash.pp_short hash
      P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
    Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.push pipeline.fetched_blocks
        (hash, header, operations) >>= return
    end
  end >>= function
  | Ok () ->
      operations_fetch_worker_loop pipeline
  | Error [Exn Lwt.Canceled | Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      Lwt_pipe.close pipeline.fetched_blocks ;
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error "@[Unexpected error (operations fetch):@ %a@]"
        pp_print_error err >>= fun () ->
      Canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let rec validation_worker_loop pipeline =
  begin
    Lwt_unix.yield () >>= fun () ->
    Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.pop pipeline.fetched_blocks >>= return
    end >>=? fun (hash, header, operations) ->
    lwt_log_info "requesting validation for block %a from peer %a."
      Block_hash.pp_short hash
      P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
    Lwt_utils.protect ~canceler:pipeline.canceler begin fun () ->
      Block_validator.validate
        ~canceler:pipeline.canceler
        ~notify_new_block:pipeline.notify_new_block
        pipeline.block_validator
        pipeline.net_db hash header operations
    end >>=? fun _block ->
    lwt_log_info "validated block %a from peer %a."
      Block_hash.pp_short hash
      P2p.Peer_id.pp_short pipeline.peer_id >>= fun () ->
    return ()
  end >>= function
  | Ok () -> validation_worker_loop pipeline
  | Error [Exn Lwt.Canceled | Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      Lwt.return_unit
  | Error ([ Block_validator.Invalid_block _
           | Block_validator.Unavailable_protocol _ ] as err ) ->
      (* Propagate the error to the peer validator. *)
      pipeline.errors <- pipeline.errors @ err ;
      Canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error "@[Unexpected error (validator):@ %a@]"
        pp_print_error err >>= fun () ->
      Canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let create
    ?(notify_new_block = fun _ -> ())
    ~block_header_timeout ~block_operations_timeout
    block_validator peer_id net_db locator =
  let canceler = Canceler.create () in
  let fetched_headers =
    Lwt_pipe.create ~size:(50, fun _ -> 1) () in
  let fetched_blocks =
    Lwt_pipe.create ~size:(50, fun _ -> 1) () in
  let pipeline = {
    canceler ;
    block_header_timeout ; block_operations_timeout ;
    headers_fetch_worker = Lwt.return_unit ;
    operations_fetch_worker = Lwt.return_unit ;
    validation_worker = Lwt.return_unit ;
    notify_new_block ;
    peer_id ; net_db ; locator ;
    block_validator ;
    fetched_headers ; fetched_blocks ;
    errors = [] ;
  } in
  Canceler.on_cancel pipeline.canceler begin fun () ->
    Lwt_pipe.close fetched_blocks ;
    Lwt_pipe.close fetched_headers ;
    Lwt.return_unit
  end ;
  let head, _ = (pipeline.locator : Block_locator.t :> _ * _) in
  let hash = Block_header.hash head in
  pipeline.headers_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-headers_fetch.%a.%a"
         P2p.Peer_id.pp_short peer_id Block_hash.pp_short hash)
      ~run:(fun () -> headers_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Canceler.cancel pipeline.canceler) ;
  pipeline.operations_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-operations_fetch.%a.%a"
         P2p.Peer_id.pp_short peer_id Block_hash.pp_short hash)
      ~run:(fun () -> operations_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Canceler.cancel pipeline.canceler) ;
  pipeline.validation_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-validation.%a.%a"
         P2p.Peer_id.pp_short peer_id Block_hash.pp_short hash)
      ~run:(fun () -> validation_worker_loop pipeline)
      ~cancel:(fun () -> Canceler.cancel pipeline.canceler) ;
  pipeline

let wait_workers pipeline =
  pipeline.headers_fetch_worker >>= fun () ->
  pipeline.operations_fetch_worker >>= fun () ->
  pipeline.validation_worker >>= fun () ->
  Lwt.return_unit

let wait pipeline =
  wait_workers pipeline >>= fun () ->
  match pipeline.errors with
  | [] -> return ()
  | errors -> Lwt.return_error errors

let cancel pipeline =
  Canceler.cancel pipeline.canceler >>= fun () ->
  wait_workers pipeline
