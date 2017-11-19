(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "node.validator.net" end)
module Canceler = Lwt_utils.Canceler

type t = {

  db: Distributed_db.t ;
  net_state: State.Net.t ;
  net_db: Distributed_db.net_db ;
  block_validator: Block_validator.t ;

  timeout: timeout ;
  bootstrap_threshold: int ;
  mutable bootstrapped: bool ;
  bootstrapped_wakener: unit Lwt.u ;
  valid_block_input: State.Block.t Watcher.input ;
  global_valid_block_input: State.Block.t Watcher.input ;
  new_head_input: State.Block.t Watcher.input ;

  parent: t option ;
  max_child_ttl: int option ;

  mutable child: t option ;
  prevalidator: Prevalidator.t ;
  active_peers: Peer_validator.t Lwt.t P2p.Peer_id.Table.t ;
  bootstrapped_peers: unit P2p.Peer_id.Table.t ;

  mutable worker: unit Lwt.t ;
  queue: State.Block.t Lwt_pipe.t ;
  canceler: Canceler.t ;

}

and timeout = {
  operation: float ;
  block_header: float ;
  block_operations: float ;
  protocol: float ;
  new_head_request: float ;
}


let rec shutdown nv =
  Canceler.cancel nv.canceler >>= fun () ->
  Distributed_db.deactivate nv.net_db >>= fun () ->
  Lwt.join
    ( nv.worker ::
      Prevalidator.shutdown nv.prevalidator ::
      Lwt_utils.may ~f:shutdown nv.child ::
      P2p.Peer_id.Table.fold
        (fun _ pv acc -> (pv >>= Peer_validator.shutdown) :: acc)
        nv.active_peers [] ) >>= fun () ->
  Lwt.return_unit

let shutdown_child nv =
  Lwt_utils.may ~f:shutdown nv.child

let notify_new_block nv block =
  iter_option nv.parent
    ~f:(fun nv -> Watcher.notify nv.valid_block_input block) ;
  Watcher.notify nv.valid_block_input block ;
  Watcher.notify nv.global_valid_block_input block ;
  assert (Lwt_pipe.push_now nv.queue block)

let may_toggle_bootstrapped_network nv =
  if not nv.bootstrapped &&
     P2p.Peer_id.Table.length nv.bootstrapped_peers >= nv.bootstrap_threshold
  then begin
    nv.bootstrapped <- true ;
    Lwt.wakeup_later nv.bootstrapped_wakener () ;
  end

let may_activate_peer_validator nv peer_id =
  try P2p.Peer_id.Table.find nv.active_peers peer_id
  with Not_found ->
    let pv =
      Peer_validator.create
        ~new_head_request_timeout:nv.timeout.new_head_request
        ~block_header_timeout:nv.timeout.block_header
        ~block_operations_timeout:nv.timeout.block_operations
        ~protocol_timeout:nv.timeout.protocol
        ~notify_new_block:(notify_new_block nv)
        ~notify_bootstrapped: begin fun () ->
          P2p.Peer_id.Table.add nv.bootstrapped_peers peer_id () ;
          may_toggle_bootstrapped_network nv
        end
        ~notify_termination: begin fun _pv ->
          P2p.Peer_id.Table.remove nv.active_peers peer_id ;
          P2p.Peer_id.Table.remove nv.bootstrapped_peers peer_id ;
        end
        nv.block_validator nv.net_db peer_id in
    P2p.Peer_id.Table.add nv.active_peers peer_id pv ;
    pv

let broadcast_head nv ~previous block =
  if not nv.bootstrapped then
    Lwt.return_unit
  else begin
    begin
      State.Block.predecessor block >>= function
      | None -> Lwt.return_true
      | Some predecessor ->
          Lwt.return (State.Block.equal predecessor previous)
    end >>= fun successor ->
    if successor then begin
      Distributed_db.Advertise.current_head nv.net_db block ;
      Lwt.return_unit
    end else begin
      Distributed_db.Advertise.current_branch nv.net_db block
    end
  end


let rec create
    ?max_child_ttl ?parent
    ?(bootstrap_threshold = 1)
    timeout block_validator
    global_valid_block_input db net_state =
  let net_db = Distributed_db.activate db net_state in
  Prevalidator.create
    ~max_operations:2000 (* FIXME temporary constant *)
    ~operation_timeout:timeout.operation net_db >>= fun prevalidator ->
  let valid_block_input = Watcher.create_input () in
  let new_head_input = Watcher.create_input () in
  let canceler = Canceler.create () in
  let _, bootstrapped_wakener = Lwt.wait () in
  let nv = {
    db ; net_state ; net_db ; block_validator ;
    prevalidator ;
    timeout ;
    valid_block_input ; global_valid_block_input ;
    new_head_input ;
    parent ; max_child_ttl ; child = None ;
    bootstrapped = (bootstrap_threshold <= 0) ;
    bootstrapped_wakener ;
    bootstrap_threshold ;
    active_peers =
      P2p.Peer_id.Table.create 50 ; (* TODO use `2 * max_connection` *)
    bootstrapped_peers =
      P2p.Peer_id.Table.create 50 ; (* TODO use `2 * max_connection` *)
    worker = Lwt.return_unit ;
    queue = Lwt_pipe.create () ;
    canceler ;
  } in
  if nv.bootstrapped then Lwt.wakeup_later bootstrapped_wakener () ;
  Distributed_db.set_callback net_db {
    notify_branch = begin fun peer_id locator ->
      Lwt.async begin fun () ->
        may_activate_peer_validator nv peer_id >>= fun pv ->
        Peer_validator.notify_branch pv locator ;
        Lwt.return_unit
      end
    end ;
    notify_head = begin fun peer_id block ops ->
      Lwt.async begin fun () ->
        may_activate_peer_validator nv peer_id >>= fun pv ->
        Peer_validator.notify_head pv block ;
        (* TODO notify prevalidator only if head is known ??? *)
        Prevalidator.notify_operations nv.prevalidator peer_id ops ;
        Lwt.return_unit
      end;
    end ;
    disconnection = begin fun peer_id ->
      Lwt.async begin fun () ->
        may_activate_peer_validator nv peer_id >>= fun pv ->
        Peer_validator.shutdown pv >>= fun () ->
        Lwt.return_unit
      end
    end ;
  } ;
  nv.worker <-
    Lwt_utils.worker
      (Format.asprintf "net_validator.%a" Net_id.pp (State.Net.id net_state))
      ~run:(fun () -> worker_loop nv)
      ~cancel:(fun () -> Canceler.cancel nv.canceler) ;
  Lwt.return nv

(** Current block computation *)

and worker_loop nv =
  begin
    Lwt_utils.protect ~canceler:nv.canceler begin fun () ->
      Lwt_pipe.pop nv.queue >>= return
    end >>=? fun block ->
    Chain.head nv.net_state >>= fun head ->
    let head_header = State.Block.header head
    and head_hash = State.Block.hash head
    and block_header = State.Block.header block
    and block_hash = State.Block.hash block in
    if
      Fitness.(block_header.shell.fitness <= head_header.shell.fitness)
    then
      return ()
    else begin
      Chain.set_head nv.net_state block >>= fun previous ->
      broadcast_head nv ~previous block >>= fun () ->
      Prevalidator.flush nv.prevalidator block ; (* FIXME *)
      may_switch_test_network nv block >>= fun () ->
      Watcher.notify nv.new_head_input block ;
      lwt_log_notice "update current head %a %a %a(%t)"
        Block_hash.pp_short block_hash
        Fitness.pp block_header.shell.fitness
        Time.pp_hum block_header.shell.timestamp
        (fun ppf ->
           if Block_hash.equal head_hash block_header.shell.predecessor then
             Format.fprintf ppf "same branch"
           else
             Format.fprintf ppf "changing branch") >>= fun () ->
      return ()
    end
  end >>= function
  | Ok () ->
      worker_loop nv
  | Error [Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error "@[Unexpected error:@ %a@]"
        pp_print_error err >>= fun () ->
      Canceler.cancel nv.canceler >>= fun () ->
      Lwt.return_unit

and may_switch_test_network nv block =

  let create_child genesis protocol expiration =
    if State.Net.allow_forked_network nv.net_state then begin
      shutdown_child nv >>= fun () ->
      begin
        let net_id = Net_id.of_block_hash (State.Block.hash genesis) in
        State.Net.get
          (State.Net.global_state nv.net_state) net_id >>= function
        | Ok net_state -> return net_state
        | Error _ ->
            State.fork_testnet
              genesis protocol expiration >>=? fun net_state ->
            Chain.head net_state >>= fun new_genesis_block ->
            Watcher.notify nv.global_valid_block_input new_genesis_block ;
            Watcher.notify nv.valid_block_input new_genesis_block ;
            return net_state
      end >>=? fun net_state ->
      create
        ~parent:nv nv.timeout nv.block_validator
        nv.global_valid_block_input
        nv.db net_state >>= fun child ->
      nv.child <- Some child ;
      return ()
    end else begin
      (* Ignoring request... *)
      return ()
    end in

  let check_child genesis protocol expiration current_time =
    let activated =
      match nv.child with
      | None -> false
      | Some child ->
          Block_hash.equal
            (State.Net.genesis child.net_state).block
            genesis in
    State.Block.read nv.net_state genesis >>=? fun genesis ->
    begin
      match nv.max_child_ttl with
      | None -> Lwt.return expiration
      | Some ttl ->
          Lwt.return
            (Time.min expiration
               (Time.add (State.Block.timestamp genesis) (Int64.of_int ttl)))
    end >>= fun local_expiration ->
    let expired = Time.(local_expiration <= current_time) in
    if expired && activated then
      shutdown_child nv >>= return
    else if not activated && not expired then
      create_child genesis protocol expiration
    else
      return () in

  begin
    let block_header = State.Block.header block in
    State.Block.test_network block >>= function
    | Not_running -> shutdown_child nv >>= return
    | Running { genesis ; protocol ; expiration } ->
        check_child genesis protocol expiration
          block_header.shell.timestamp
    | Forking { protocol ; expiration } ->
        create_child block protocol expiration
  end >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      lwt_log_error "@[<v 2>Error while switch test network:@ %a@]"
        Error_monad.pp_print_error err >>= fun () ->
      Lwt.return_unit


(* TODO check the initial sequence of message when connecting to a new
        peer, and the one when activating a network. *)


let create
    ?max_child_ttl
    ?bootstrap_threshold
    timeout
    block_validator global_valid_block_input global_db state =
  (* hide the optional ?parent *)
  create
    ?max_child_ttl
    ?bootstrap_threshold
    timeout block_validator global_valid_block_input global_db state

let net_id { net_state } = State.Net.id net_state
let net_state { net_state } = net_state
let prevalidator { prevalidator } = prevalidator
let net_db { net_db } = net_db
let child { child } = child

let validate_block nv ?(force = false) hash block operations =
  assert (Block_hash.equal hash (Block_header.hash block)) ;
  Chain.head nv.net_state >>= fun head ->
  let head = State.Block.header head in
  if
    force || Fitness.(head.shell.fitness <= block.shell.fitness)
  then
    Block_validator.validate
      ~canceler:nv.canceler
      ~notify_new_block:(notify_new_block nv)
      nv.block_validator nv.net_db hash block operations
  else
    failwith "Fitness too low"

let bootstrapped { bootstrapped_wakener } =
  Lwt.protected (Lwt.waiter_of_wakener bootstrapped_wakener)

let valid_block_watcher { valid_block_input } =
  Watcher.create_stream valid_block_input

let new_head_watcher { new_head_input } =
  Watcher.create_stream new_head_input
