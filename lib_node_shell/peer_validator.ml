(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* FIXME ignore/postpone fetching/validating of block in the future... *)

include Logging.Make(struct let name = "node.validator.peer" end)

type msg =
  | New_head of Block_hash.t * Block_header.t
  | New_branch of Block_hash.t * Block_locator.t

type t = {

  peer_id: P2p.Peer_id.t ;
  net_db: Distributed_db.net_db ;
  block_validator: Block_validator.t ;

  new_head_request_timeout: float ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  protocol_timeout: float ;

  (* callback to net_validator *)
  notify_new_block: State.Block.t -> unit ;
  notify_bootstrapped: unit -> unit ;

  mutable bootstrapped: bool ;
  mutable last_validated_head: Block_header.t ;
  mutable last_advertised_head: Block_header.t ;

  mutable worker: unit Lwt.t ;
  dropbox: msg Lwt_dropbox.t ;
  canceler: Lwt_canceler.t ;

}

type error +=
  | Unknown_ancestor
  | Known_invalid

let set_bootstrapped pv =
  if not pv.bootstrapped then begin
    pv.bootstrapped <- true ;
    pv.notify_bootstrapped () ;
  end

let bootstrap_new_branch pv _ancestor _head unknown_prefix =
  let len = Block_locator.estimated_length unknown_prefix in
  lwt_log_info
    "validating new branch from peer %a (approx. %d blocks)"
    P2p.Peer_id.pp_short pv.peer_id len >>= fun () ->
  let pipeline =
    Bootstrap_pipeline.create
      ~notify_new_block:pv.notify_new_block
      ~block_header_timeout:pv.block_header_timeout
      ~block_operations_timeout:pv.block_operations_timeout
      pv.block_validator
      pv.peer_id pv.net_db unknown_prefix in
  Lwt_utils.protect ~canceler:pv.canceler
    ~on_error:begin fun error ->
      (* if the peer_validator is killed, let's cancel the pipeline *)
      Bootstrap_pipeline.cancel pipeline >>= fun () ->
      Lwt.return_error error
    end
    begin fun () ->
      Bootstrap_pipeline.wait pipeline
    end >>=? fun () ->
  set_bootstrapped pv ;
  lwt_log_info
    "done validating new branch from peer %a."
    P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
  return ()

let validate_new_head pv hash (header : Block_header.t) =
  let net_state = Distributed_db.net_state pv.net_db in
  State.Block.known net_state header.shell.predecessor >>= function
  | false ->
      lwt_debug
        "missing predecessor for new head %a from peer %a"
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      Distributed_db.Request.current_branch pv.net_db ~peer:pv.peer_id () ;
      return ()
  | true ->
      lwt_debug
        "fetching operations for new head %a from peer %a"
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      map_p
        (fun i ->
           Lwt_utils.protect ~canceler:pv.canceler begin fun () ->
             Distributed_db.Operations.fetch
               ~timeout:pv.block_operations_timeout
               pv.net_db ~peer:pv.peer_id
               (hash, i) header.shell.operations_hash
           end)
        (0 -- (header.shell.validation_passes - 1)) >>=? fun operations ->
      lwt_debug
        "requesting validation for new head %a from peer %a"
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      Block_validator.validate
        ~notify_new_block:pv.notify_new_block
        pv.block_validator pv.net_db
        hash header operations >>=? fun _block ->
      lwt_debug "end of validation for new head %a from peer %a"
        Block_hash.pp_short hash
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      set_bootstrapped pv ;
      return ()

let may_validate_new_head pv hash header =
  let net_state = Distributed_db.net_state pv.net_db in
  State.Block.known net_state hash >>= function
  | true -> begin
      State.Block.known_valid net_state hash >>= function
      | true ->
          lwt_debug
            "ignoring previously validated block %a from peer %a"
            Block_hash.pp_short hash
            P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
          set_bootstrapped pv ;
          pv.last_validated_head <- header ;
          return ()
      | false ->
          lwt_log_info
            "ignoring known invalid block %a from peer %a"
            Block_hash.pp_short hash
            P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
          fail Known_invalid
    end
  | false ->
      validate_new_head pv hash header

let may_validate_new_branch pv distant_hash locator =
  let distant_header, _ = (locator : Block_locator.t :> Block_header.t * _) in
  let net_state = Distributed_db.net_state pv.net_db in
  Chain.head net_state >>= fun local_header ->
  if Fitness.compare
      distant_header.Block_header.shell.fitness
      (State.Block.fitness local_header) < 0 then begin
    set_bootstrapped pv ;
    lwt_debug
      "ignoring branch %a with low fitness from peer: %a."
      Block_hash.pp_short distant_hash
      P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
    (* Don't bother with downloading a branch with a low fitness. *)
    return ()
  end else begin
    let net_state = Distributed_db.net_state pv.net_db in
    Block_locator.known_ancestor net_state locator >>= function
    | None ->
        lwt_log_info
          "ignoring branch %a without common ancestor from peer: %a."
          Block_hash.pp_short distant_hash
          P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
        fail Unknown_ancestor
    | Some (ancestor, unknown_prefix) ->
        bootstrap_new_branch pv ancestor distant_header unknown_prefix
  end

let rec worker_loop pv =
  begin
    Lwt_utils.protect ~canceler:pv.canceler begin fun () ->
      Lwt_dropbox.take_with_timeout
        pv.new_head_request_timeout
        pv.dropbox >>= return
    end >>=? function
    | None ->
        lwt_log_info "no new head from peer %a for 90 seconds."
          P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
        Distributed_db.Request.current_head pv.net_db ~peer:pv.peer_id () ;
        return ()
    | Some (New_head (hash, header)) ->
        lwt_log_info "processing new head %a from peer %a."
          Block_hash.pp_short hash
          P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
        may_validate_new_head pv hash header
    | Some (New_branch (hash, locator)) ->
        (* TODO penalize empty locator... ?? *)
        lwt_log_info "processing new branch %a from peer %a."
          Block_hash.pp_short hash
          P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
        may_validate_new_branch pv hash locator
  end >>= function
  | Ok () ->
      worker_loop pv
  | Error ((( Unknown_ancestor
           | Block_locator.Invalid_locator _
           | Block_validator.Invalid_block _ ) :: _) as errors ) ->
      (* TODO ban the peer_id... *)
      lwt_log_info "Terminating the validation worker for peer %a (kickban)."
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      lwt_debug "%a" Error_monad.pp_print_error errors >>= fun () ->
      Lwt_canceler.cancel pv.canceler >>= fun () ->
      Lwt.return_unit
  | Error [Block_validator.Unavailable_protocol { protocol } ] -> begin
      Block_validator.fetch_and_compile_protocol
        pv.block_validator
        ~peer:pv.peer_id
        ~timeout:pv.protocol_timeout
        protocol >>= function
      | Ok _ -> worker_loop pv
      | Error _ ->
          (* TODO penality... *)
          lwt_log_info "Terminating the validation worker for peer %a \
                       \ (missing protocol %a)."
            P2p.Peer_id.pp_short pv.peer_id
            Protocol_hash.pp_short protocol >>= fun () ->
          Lwt_canceler.cancel pv.canceler >>= fun () ->
          Lwt.return_unit
    end
  | Error [Exn Lwt.Canceled | Lwt_utils.Canceled | Exn Lwt_dropbox.Closed] ->
      lwt_log_info "Terminating the validation worker for peer %a."
        P2p.Peer_id.pp_short pv.peer_id >>= fun () ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error
        "@[<v 2>Unexpected error in the validation worker for peer %a:@ \
        \ %a@]"
        P2p.Peer_id.pp_short pv.peer_id
        pp_print_error err >>= fun () ->
      Lwt_canceler.cancel pv.canceler >>= fun () ->
      Lwt.return_unit

let create
    ?notify_new_block:(external_notify_new_block = fun _ -> ())
    ?(notify_bootstrapped = fun () -> ())
    ?(notify_termination = fun _ -> ())
    ~new_head_request_timeout
    ~block_header_timeout
    ~block_operations_timeout
    ~protocol_timeout
    block_validator net_db peer_id =
  lwt_debug "creating validator for peer %a."
    P2p.Peer_id.pp_short peer_id >>= fun () ->
  let canceler = Lwt_canceler.create () in
  let dropbox = Lwt_dropbox.create () in
  let net_state = Distributed_db.net_state net_db in
  State.Block.read_exn net_state
    (State.Net.genesis net_state).block >>= fun genesis ->
  let rec notify_new_block block =
    pv.last_validated_head <- State.Block.header block ;
    external_notify_new_block block
  and pv = {
    block_validator ;
    notify_new_block ;
    notify_bootstrapped ;
    new_head_request_timeout ;
    block_header_timeout ;
    block_operations_timeout ;
    protocol_timeout ;
    net_db ;
    peer_id ;
    bootstrapped = false ;
    last_validated_head = State.Block.header genesis ;
    last_advertised_head = State.Block.header genesis ;
    canceler ;
    dropbox ;
    worker = Lwt.return_unit ;
  } in
  Lwt_canceler.on_cancel pv.canceler begin fun () ->
    Lwt_dropbox.close pv.dropbox ;
    Distributed_db.disconnect pv.net_db pv.peer_id >>= fun () ->
    notify_termination pv ;
    Lwt.return_unit
  end ;
  pv.worker <-
    Lwt_utils.worker
      (Format.asprintf "peer_validator.%a.%a"
         Net_id.pp (State.Net.id net_state) P2p.Peer_id.pp_short peer_id)
      ~run:(fun () -> worker_loop pv)
      ~cancel:(fun () -> Lwt_canceler.cancel pv.canceler) ;
  Lwt.return pv

let notify_branch pv locator =
  let header, _ = (locator : Block_locator.t :> _ * _) in
  let hash = Block_header.hash header in
  (* TODO penalize decreasing fitness *)
  pv.last_advertised_head <- header ;
  try Lwt_dropbox.put pv.dropbox (New_branch (hash, locator))
  with Lwt_dropbox.Closed -> ()

let notify_head pv header =
  let hash = Block_header.hash header in
  pv.last_advertised_head <- header ;
  (* TODO penalize decreasing fitness *)
  match Lwt_dropbox.peek pv.dropbox with
  | Some (New_branch _) -> () (* ignore *)
  | None | Some (New_head _) ->
      try Lwt_dropbox.put pv.dropbox (New_head (hash, header))
      with Lwt_dropbox.Closed -> ()

let shutdown pv =
  Lwt_canceler.cancel pv.canceler >>= fun () ->
  pv.worker

let peer_id pv = pv.peer_id
let bootstrapped pv = pv.bootstrapped
let current_head pv = pv.last_validated_head
