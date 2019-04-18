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

include Internal_event.Legacy_logging.Make_semantic
    (struct let name = "node.validator.bootstrap_pipeline" end)

let node_time_tag =
  Tag.def ~doc:"local time at this node" "node_time" Time.System.pp_hum
let block_time_tag =
  Tag.def
    ~doc:"claimed creation time of block"
    "block_time"
    (fun fmt prot_time -> Time.System.(pp_hum fmt (of_protocol_exn prot_time)))

open Validation_errors

type t = {
  canceler: Lwt_canceler.t ;
  block_header_timeout: Time.System.Span.t ;
  block_operations_timeout: Time.System.Span.t ;
  mutable headers_fetch_worker: unit Lwt.t ;
  mutable operations_fetch_worker: unit Lwt.t ;
  mutable validation_worker: unit Lwt.t ;
  peer_id: P2p_peer.Id.t ;
  chain_db: Distributed_db.chain_db ;
  locator: Block_locator.t ;
  block_validator: Block_validator.t ;
  notify_new_block: State.Block.t -> unit ;
  fetched_headers:
    (Block_hash.t * Block_header.t) Lwt_pipe.t ;
  fetched_blocks:
    (Block_hash.t * Block_header.t * Operation.t list list tzresult Lwt.t) Lwt_pipe.t ;
  (* HACK, a worker should be able to return the 'error'. *)
  mutable errors: Error_monad.error list ;
}

let operations_index_tag = Tag.def ~doc:"Operations index" "operations_index" Format.pp_print_int

let assert_acceptable_header pipeline
    hash (header : Block_header.t) =
  let chain_state = Distributed_db.chain_state pipeline.chain_db in
  let time_now = Systime_os.now () in
  fail_unless
    (Time.Protocol.compare
       (Time.Protocol.add (Time.System.to_protocol (Systime_os.now ())) 15L)
       header.shell.timestamp
     >= 0)
    (Future_block_header { block = hash; time = time_now;
                           block_time = header.shell.timestamp }) >>=? fun () ->
  State.Chain.checkpoint chain_state >>= fun (checkpoint_level, checkpoint) ->
  fail_when
    (Int32.equal header.shell.level checkpoint_level &&
     not (Block_hash.equal checkpoint hash))
    (Checkpoint_error (hash, Some pipeline.peer_id)) >>=? fun () ->
  Chain.head chain_state >>= fun head ->
  let checkpoint_reached = (State.Block.header head).shell.level >= checkpoint_level in
  if checkpoint_reached then
    (* If reached the checkpoint, every block before the checkpoint
       must be part of the chain. *)
    if header.shell.level <= checkpoint_level then
      Chain.mem chain_state hash >>= fun in_chain ->
      fail_unless in_chain
        (Checkpoint_error (hash, Some pipeline.peer_id)) >>=? fun () ->
      return_unit
    else
      return_unit
  else
    return_unit

let fetch_step pipeline (step : Block_locator.step)  =
  lwt_log_info Tag.DSL.(fun f ->
      f "fetching step %a -> %a (%a) from peer %a."
      -% t event "fetching_step_from_peer"
      -% a Block_hash.Logging.tag step.block
      -% a Block_hash.Logging.predecessor_tag step.predecessor
      -% a (Tag.def ~doc:"" "" Block_locator.pp_step) step
      -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
  let rec fetch_loop acc hash cpt =
    Lwt_unix.yield () >>= fun () ->
    if cpt < 0 then
      lwt_log_info Tag.DSL.(fun f ->
          f "invalid step from peer %a (too long)."
          -% t event "step_too_long"
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
    else if Block_hash.equal hash step.predecessor then
      if step.strict_step && cpt <> 0 then
        lwt_log_info Tag.DSL.(fun f ->
            f "invalid step from peer %a (too short)."
            -% t event "step_too_short"
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
        fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
      else
        return acc
    else
      lwt_debug Tag.DSL.(fun f ->
          f "fetching block header %a from peer %a."
          -% t event "fetching_block_header_from_peer"
          -% a Block_hash.Logging.tag hash
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      protect ~canceler:pipeline.canceler begin fun () ->
        Distributed_db.Block_header.fetch
          ~timeout:pipeline.block_header_timeout
          pipeline.chain_db ~peer:pipeline.peer_id
          hash ()
      end >>=? fun header ->
      assert_acceptable_header pipeline hash header >>=? fun () ->
      lwt_debug Tag.DSL.(fun f ->
          f "fetched block header %a from peer %a."
          -% t event "fetched_block_header_from_peer"
          -% a Block_hash.Logging.tag hash
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      fetch_loop ((hash, header) :: acc) header.shell.predecessor (cpt - 1)
  in
  fetch_loop [] step.block step.step >>=? fun headers ->
  iter_s
    begin fun header ->
      protect ~canceler:pipeline.canceler begin fun () ->
        Lwt_pipe.push pipeline.fetched_headers header >>= return
      end
    end
    headers >>=? fun () ->
  return_unit

let headers_fetch_worker_loop pipeline =
  begin
    let sender_id = Distributed_db.my_peer_id pipeline.chain_db in
    (* sender and receiver are inverted here because they are from
       the point of view of the node sending the locator *)
    let seed = {Block_locator.sender_id=pipeline.peer_id; receiver_id=sender_id } in
    let steps = Block_locator.to_steps seed pipeline.locator in
    iter_s (fetch_step pipeline) steps >>=? fun () ->
    return_unit
  end >>= function
  | Ok () ->
      lwt_log_info Tag.DSL.(fun f ->
          f "fetched all steps from peer %a."
          -% t event "fetched_all_steps_from_peer"
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      Lwt_pipe.close pipeline.fetched_headers ;
      Lwt.return_unit
  | Error [Exn Lwt.Canceled | Canceled | Exn Lwt_pipe.Closed] ->
      Lwt.return_unit
  | Error [ Distributed_db.Block_header.Timeout bh ] ->
      lwt_log_info Tag.DSL.(fun f ->
          f "request for header %a from peer %a timed out."
          -% t event "header_request_timeout"
          -% a Block_hash.Logging.tag bh
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit
  | Error [ Future_block_header { block; block_time; time } ] ->
      lwt_log_notice Tag.DSL.(fun f ->
          f "Block locator %a from peer %a contains future blocks. \
             local time: %a, block time: %a"
          -% t event "locator_contains_future_blocks"
          -% a Block_hash.Logging.tag block
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id
          -% a node_time_tag time
          -% a block_time_tag block_time) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error Tag.DSL.(fun f ->
          f "@[Unexpected error (headers fetch):@ %a@]"
          -% t event "unexpected_error"
          -% a errs_tag err) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let rec operations_fetch_worker_loop pipeline =
  begin
    Lwt_unix.yield () >>= fun () ->
    protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.pop pipeline.fetched_headers >>= return
    end >>=? fun (hash, header) ->
    lwt_log_info Tag.DSL.(fun f ->
        f "fetching operations of block %a from peer %a."
        -% t event "fetching_operations"
        -% a Block_hash.Logging.tag hash
        -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
    let operations =
      map_p
        (fun i ->
           protect ~canceler:pipeline.canceler begin fun () ->
             Distributed_db.Operations.fetch
               ~timeout:pipeline.block_operations_timeout
               pipeline.chain_db ~peer:pipeline.peer_id
               (hash, i) header.shell.operations_hash
           end)
        (0 -- (header.shell.validation_passes - 1)) >>=? fun operations ->
      lwt_log_info Tag.DSL.(fun f ->
          f "fetched operations of block %a from peer %a."
          -% t event "fetched_operations"
          -% a Block_hash.Logging.tag hash
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      return operations in
    protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.push pipeline.fetched_blocks
        (hash, header, operations) >>= return
    end
  end >>= function
  | Ok () ->
      operations_fetch_worker_loop pipeline
  | Error [Exn Lwt.Canceled | Canceled | Exn Lwt_pipe.Closed] ->
      Lwt_pipe.close pipeline.fetched_blocks ;
      Lwt.return_unit
  | Error [ Distributed_db.Operations.Timeout (bh, n) ] ->
      lwt_log_info Tag.DSL.(fun f ->
          f "request for operations %a:%d from peer %a timed out."
          -% t event "request_operations_timeout"
          -% a Block_hash.Logging.tag bh
          -% s operations_index_tag n
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error Tag.DSL.(fun f ->
          f "@[Unexpected error (operations fetch):@ %a@]"
          -% t event "unexpected_error"
          -% a errs_tag err) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let rec validation_worker_loop pipeline =
  begin
    Lwt_unix.yield () >>= fun () ->
    protect ~canceler:pipeline.canceler begin fun () ->
      Lwt_pipe.pop pipeline.fetched_blocks >>= return
    end >>=? fun (hash, header, operations) ->
    lwt_log_info Tag.DSL.(fun f ->
        f "requesting validation for block %a from peer %a."
        -% t event "requesting_validation"
        -% a Block_hash.Logging.tag hash
        -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
    operations >>=? fun operations ->
    protect ~canceler:pipeline.canceler begin fun () ->
      Block_validator.validate
        ~canceler:pipeline.canceler
        ~notify_new_block:pipeline.notify_new_block
        pipeline.block_validator
        pipeline.chain_db hash header operations
    end >>=? fun _block ->
    lwt_log_info Tag.DSL.(fun f ->
        f "validated block %a from peer %a."
        -% t event "validated_block"
        -% a Block_hash.Logging.tag hash
        -% a P2p_peer.Id.Logging.tag pipeline.peer_id) >>= fun () ->
    return_unit
  end >>= function
  | Ok () -> validation_worker_loop pipeline
  | Error [Exn Lwt.Canceled | Canceled | Exn Lwt_pipe.Closed] ->
      Lwt.return_unit
  | Error ([ Block_validator_errors.Invalid_block _
           | Block_validator_errors.Unavailable_protocol _
           | Block_validator_errors.System_error _
           | Timeout] as err ) ->
      (* Propagate the error to the peer validator. *)
      pipeline.errors <- pipeline.errors @ err ;
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error Tag.DSL.(fun f ->
          f "@[Unexpected error (validator):@ %a@]"
          -% t event "unexpected_error"
          -% a errs_tag err) >>= fun () ->
      Lwt_canceler.cancel pipeline.canceler >>= fun () ->
      Lwt.return_unit

let create
    ?(notify_new_block = fun _ -> ())
    ~block_header_timeout ~block_operations_timeout
    block_validator peer_id chain_db locator =
  let canceler = Lwt_canceler.create () in
  let fetched_headers =
    Lwt_pipe.create ~size:(1024, fun _ -> 1) () in
  let fetched_blocks =
    Lwt_pipe.create ~size:(128, fun _ -> 1) () in
  let pipeline = {
    canceler ;
    block_header_timeout ; block_operations_timeout ;
    headers_fetch_worker = Lwt.return_unit ;
    operations_fetch_worker = Lwt.return_unit ;
    validation_worker = Lwt.return_unit ;
    notify_new_block ;
    peer_id ; chain_db ; locator ;
    block_validator ;
    fetched_headers ; fetched_blocks ;
    errors = [] ;
  } in
  Lwt_canceler.on_cancel pipeline.canceler begin fun () ->
    Lwt_pipe.close fetched_blocks ;
    Lwt_pipe.close fetched_headers ;
    (* TODO proper cleanup of ressources... *)
    Lwt.return_unit
  end ;
  let head, _ = (pipeline.locator : Block_locator.t :> _ * _) in
  let hash = Block_header.hash head in
  pipeline.headers_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-headers_fetch.%a.%a"
         P2p_peer.Id.pp_short peer_id Block_hash.pp_short hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> headers_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline.operations_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-operations_fetch.%a.%a"
         P2p_peer.Id.pp_short peer_id Block_hash.pp_short hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> operations_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline.validation_worker <-
    Lwt_utils.worker
      (Format.asprintf "bootstrap_pipeline-validation.%a.%a"
         P2p_peer.Id.pp_short peer_id Block_hash.pp_short hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> validation_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline

let wait_workers pipeline =
  pipeline.headers_fetch_worker >>= fun () ->
  pipeline.operations_fetch_worker >>= fun () ->
  pipeline.validation_worker >>= fun () ->
  Lwt.return_unit

let wait pipeline =
  wait_workers pipeline >>= fun () ->
  match pipeline.errors with
  | [] -> return_unit
  | errors -> Lwt.return_error errors

let cancel pipeline =
  Lwt_canceler.cancel pipeline.canceler >>= fun () ->
  wait_workers pipeline
