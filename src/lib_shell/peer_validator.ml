(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* FIXME ignore/postpone fetching/validating of block in the future... *)

open Peer_validator_worker_state

module Name = struct
  type t = Chain_id.t * P2p_peer.Id.t
  let encoding =
    Data_encoding.tup2 Chain_id.encoding P2p_peer.Id.encoding
  let base = [ "validator.peer" ]
  let pp ppf (chain, peer) =
    Format.fprintf ppf "%a:%a"
      Chain_id.pp_short chain P2p_peer.Id.pp_short peer
end

module Request = struct
  include Request

  type _ t =
    | New_head : Block_hash.t * Block_header.t -> unit t
    | New_branch : Block_hash.t * Block_locator.t * Block_locator.seed -> unit t

  let view (type a) (req : a t) : view = match req with
    | New_head (hash, _) ->
        New_head hash
    | New_branch (hash, locator, seed) ->
        (* the seed is associated to each locator
           w.r.t. the peer_id of the sender *)
        New_branch (hash, Block_locator.estimated_length seed locator)
end

type limits = {
  new_head_request_timeout: Time.System.Span.t ;
  block_header_timeout: Time.System.Span.t ;
  block_operations_timeout: Time.System.Span.t ;
  protocol_timeout: Time.System.Span.t ;
  worker_limits: Worker_types.limits
}

module Types = struct
  include Worker_state

  type parameters = {
    chain_db: Distributed_db.chain_db ;
    block_validator: Block_validator.t ;
    (* callback to chain_validator *)
    notify_new_block: State.Block.t -> unit ;
    notify_bootstrapped: unit -> unit ;
    notify_termination: unit -> unit ;
    limits: limits;
  }

  type state = {
    peer_id: P2p_peer.Id.t ;
    parameters : parameters ;
    mutable bootstrapped: bool ;
    mutable last_validated_head: Block_header.t ;
    mutable last_advertised_head: Block_header.t ;
  }

  let view (state : state) _ : view =
    let { bootstrapped ; last_validated_head ; last_advertised_head ; _ } = state in
    { bootstrapped ;
      last_validated_head = Block_header.hash last_validated_head ;
      last_advertised_head = Block_header.hash last_advertised_head }

end

module Worker = Worker.Make (Name) (Event) (Request) (Types)

open Types

type t = Worker.dropbox Worker.t

let debug w =
  Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

let set_bootstrapped pv =
  if not pv.bootstrapped then begin
    pv.bootstrapped <- true ;
    pv.parameters.notify_bootstrapped () ;
  end

let bootstrap_new_branch w _ancestor _head unknown_prefix =
  let pv = Worker.state w in
  let sender_id = Distributed_db.my_peer_id pv.parameters.chain_db in
  (* sender and receiver are inverted here because they are from
     the point of view of the node sending the locator *)
  let seed = {Block_locator.sender_id=pv.peer_id; receiver_id = sender_id } in
  let len = Block_locator.estimated_length seed unknown_prefix in
  debug w
    "validating new branch from peer %a (approx. %d blocks)"
    P2p_peer.Id.pp_short pv.peer_id len ;
  let pipeline =
    Bootstrap_pipeline.create
      ~notify_new_block:pv.parameters.notify_new_block
      ~block_header_timeout:pv.parameters.limits.block_header_timeout
      ~block_operations_timeout:pv.parameters.limits.block_operations_timeout
      pv.parameters.block_validator
      pv.peer_id pv.parameters.chain_db unknown_prefix in
  Worker.protect w
    ~on_error:begin fun error ->
      (* if the peer_validator is killed, let's cancel the pipeline *)
      Bootstrap_pipeline.cancel pipeline >>= fun () ->
      Lwt.return_error error
    end
    begin fun () ->
      Bootstrap_pipeline.wait pipeline
    end >>=? fun () ->
  set_bootstrapped pv ;
  debug w
    "done validating new branch from peer %a."
    P2p_peer.Id.pp_short pv.peer_id ;
  return_unit

let validate_new_head w hash (header : Block_header.t) =
  let pv = Worker.state w in
  debug w
    "fetching operations for new head %a from peer %a"
    Block_hash.pp_short hash
    P2p_peer.Id.pp_short pv.peer_id ;
  map_p
    (fun i ->
       Worker.protect w begin fun () ->
         Distributed_db.Operations.fetch
           ~timeout:pv.parameters.limits.block_operations_timeout
           pv.parameters.chain_db ~peer:pv.peer_id
           (hash, i) header.shell.operations_hash
       end)
    (0 -- (header.shell.validation_passes - 1)) >>=? fun operations ->
  debug w
    "requesting validation for new head %a from peer %a"
    Block_hash.pp_short hash
    P2p_peer.Id.pp_short pv.peer_id ;
  Block_validator.validate
    ~notify_new_block:pv.parameters.notify_new_block
    pv.parameters.block_validator pv.parameters.chain_db
    hash header operations >>=? fun _block ->
  debug w
    "end of validation for new head %a from peer %a"
    Block_hash.pp_short hash
    P2p_peer.Id.pp_short pv.peer_id ;
  set_bootstrapped pv ;
  let meta = Distributed_db.get_peer_metadata pv.parameters.chain_db pv.peer_id in
  Peer_metadata.incr meta Valid_blocks;
  return_unit

let only_if_fitness_increases w distant_header cont =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  Chain.head chain_state >>= fun local_header ->
  if Fitness.compare
      distant_header.Block_header.shell.fitness
      (State.Block.fitness local_header) <= 0 then begin
    set_bootstrapped pv ;
    debug w
      "ignoring head %a with non increasing fitness from peer: %a."
      Block_hash.pp_short (Block_header.hash distant_header)
      P2p_peer.Id.pp_short pv.peer_id ;
    (* Don't download a branch that cannot beat the current head. *)
    let meta = Distributed_db.get_peer_metadata pv.parameters.chain_db pv.peer_id in
    Peer_metadata.incr meta Old_heads;
    return_unit
  end else cont ()

let assert_acceptable_head w hash (header: Block_header.t) =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  State.Chain.acceptable_block chain_state hash header >>= fun acceptable ->
  fail_unless acceptable
    (Validation_errors.Checkpoint_error (hash, Some pv.peer_id))

let may_validate_new_head w hash (header : Block_header.t) =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  State.Block.known_valid chain_state hash >>= fun valid_block ->
  State.Block.known_invalid chain_state hash >>= fun invalid_block ->
  State.Block.known_valid chain_state
    header.shell.predecessor >>= fun valid_predecessor ->
  State.Block.known_invalid chain_state
    header.shell.predecessor >>= fun invalid_predecessor ->
  if valid_block then begin
    debug w
      "ignoring previously validated block %a from peer %a"
      Block_hash.pp_short hash
      P2p_peer.Id.pp_short pv.peer_id ;
    set_bootstrapped pv ;
    pv.last_validated_head <- header ;
    return_unit
  end else if invalid_block then begin
    debug w
      "ignoring known invalid block %a from peer %a"
      Block_hash.pp_short hash
      P2p_peer.Id.pp_short pv.peer_id ;
    fail Validation_errors.Known_invalid
  end else if invalid_predecessor then begin
    debug w
      "ignoring known invalid block %a from peer %a"
      Block_hash.pp_short hash
      P2p_peer.Id.pp_short pv.peer_id ;
    Distributed_db.commit_invalid_block pv.parameters.chain_db
      hash header [Validation_errors.Known_invalid] >>=? fun _ ->
    fail Validation_errors.Known_invalid
  end else if not valid_predecessor then begin
    debug w
      "missing predecessor for new head %a from peer %a"
      Block_hash.pp_short hash
      P2p_peer.Id.pp_short pv.peer_id ;
    Distributed_db.Request.current_branch
      pv.parameters.chain_db ~peer:pv.peer_id () ;
    return_unit
  end else begin
    only_if_fitness_increases w header @@ fun () ->
    assert_acceptable_head w hash header >>=? fun () ->
    validate_new_head w hash header
  end

let may_validate_new_branch w distant_hash locator =
  let pv = Worker.state w in
  let distant_header, _ = (locator : Block_locator.t :> Block_header.t * _) in
  only_if_fitness_increases w distant_header @@ fun () ->
  assert_acceptable_head w
    (Block_header.hash distant_header) distant_header >>=? fun () ->
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  State.Block.known_ancestor chain_state locator >>= function
  | None ->
      debug w
        "ignoring branch %a without common ancestor from peer: %a."
        Block_hash.pp_short distant_hash
        P2p_peer.Id.pp_short pv.peer_id ;
      fail Validation_errors.Unknown_ancestor
  | Some (ancestor, unknown_prefix) ->
      bootstrap_new_branch w ancestor distant_header unknown_prefix

let on_no_request w =
  let pv = Worker.state w in
  debug w "no new head from peer %a for %g seconds."
    P2p_peer.Id.pp_short pv.peer_id
    (Ptime.Span.to_float_s pv.parameters.limits.new_head_request_timeout) ;
  Distributed_db.Request.current_head pv.parameters.chain_db ~peer:pv.peer_id () ;
  return_unit

let on_request (type a) w (req : a Request.t) : a tzresult Lwt.t =
  let pv = Worker.state w in
  match req with
  | Request.New_head (hash, header) ->
      debug w
        "processing new head %a from peer %a."
        Block_hash.pp_short hash
        P2p_peer.Id.pp_short pv.peer_id ;
      may_validate_new_head w hash header
  | Request.New_branch (hash, locator, _seed) ->
      (* TODO penalize empty locator... ?? *)
      debug w "processing new branch %a from peer %a."
        Block_hash.pp_short hash
        P2p_peer.Id.pp_short pv.peer_id ;
      may_validate_new_branch w hash locator

let on_completion w r _ st =
  Worker.record_event w (Event.Request (Request.view r, st, None )) ;
  Lwt.return_unit

let on_error w r st errs =
  let pv = Worker.state w in
  match errs with
    ((( Validation_errors.Unknown_ancestor
      | Validation_errors.Invalid_locator _
      | Block_validator_errors.Invalid_block _ ) :: _) as errors ) ->
      Distributed_db.greylist pv.parameters.chain_db pv.peer_id >>= fun () ->
      debug w
        "Terminating the validation worker for peer %a (kickban)."
        P2p_peer.Id.pp_short pv.peer_id ;
      debug w "%a" Error_monad.pp_print_error errors ;
      Worker.trigger_shutdown w ;
      Worker.record_event w (Event.Request (r, st, Some errs)) ;
      Lwt.return_error errs
  | [Block_validator_errors.System_error _ ] as errs ->
      Worker.record_event w (Event.Request (r, st, Some errs)) ;
      return_unit
  | [Block_validator_errors.Unavailable_protocol { protocol ; _ } ] -> begin
      Block_validator.fetch_and_compile_protocol
        pv.parameters.block_validator
        ~peer:pv.peer_id
        ~timeout:pv.parameters.limits.protocol_timeout
        protocol >>= function
      | Ok _ ->
          Distributed_db.Request.current_head
            pv.parameters.chain_db ~peer:pv.peer_id () ;
          return_unit
      | Error _ ->
          (* TODO: punish *)
          debug w
            "Terminating the validation worker for peer %a \
             (missing protocol %a)."
            P2p_peer.Id.pp_short pv.peer_id
            Protocol_hash.pp_short protocol ;
          Worker.record_event w (Event.Request (r, st, Some errs)) ;
          Lwt.return_error errs
    end
  | _ ->
      Worker.record_event w (Event.Request (r, st, Some errs)) ;
      Lwt.return_error errs

let on_close w =
  let pv = Worker.state w in
  Distributed_db.disconnect pv.parameters.chain_db pv.peer_id >>= fun () ->
  pv.parameters.notify_termination () ;
  Lwt.return_unit

let on_launch _ name parameters =
  let chain_state = Distributed_db.chain_state parameters.chain_db in
  State.Block.read_opt chain_state
    (State.Chain.genesis chain_state).block >|= Option.unopt_assert ~loc:__POS__ >>= fun genesis ->
  let rec pv = {
    peer_id = snd name ;
    parameters = { parameters with notify_new_block } ;
    bootstrapped = false ;
    last_validated_head = State.Block.header genesis ;
    last_advertised_head = State.Block.header genesis ;
  }
  and notify_new_block block =
    pv.last_validated_head <- State.Block.header block ;
    parameters.notify_new_block block in
  return pv

let table =
  let merge w (Worker.Any_request neu) old =
    let pv = Worker.state w in
    match neu with
    | Request.New_branch (_, locator, _) ->
        let header, _ = (locator : Block_locator.t :> _ * _) in
        pv.last_advertised_head <- header ;
        Some (Worker.Any_request neu)
    | Request.New_head (_, header) ->
        pv.last_advertised_head <- header ;
        (* TODO penalize decreasing fitness *)
        match old with
        | Some (Worker.Any_request (Request.New_branch _) as old) ->
            Some old (* ignore *)
        | Some (Worker.Any_request (Request.New_head _)) ->
            Some (Any_request neu)
        | None ->
            Some (Any_request neu) in
  Worker.create_table (Dropbox { merge })

let create
    ?(notify_new_block = fun _ -> ())
    ?(notify_bootstrapped = fun () -> ())
    ?(notify_termination = fun _ -> ())
    limits block_validator chain_db peer_id =
  let name = (State.Chain.id (Distributed_db.chain_state chain_db), peer_id) in
  let parameters = {
    chain_db ;
    notify_termination ;
    block_validator ;
    notify_new_block ;
    notify_bootstrapped ;
    limits ;
  } in
  let module Handlers = struct
    type self = t
    let on_launch = on_launch
    let on_request = on_request
    let on_close = on_close
    let on_error = on_error
    let on_completion = on_completion
    let on_no_request = on_no_request
  end in
  Worker.launch table ~timeout: limits.new_head_request_timeout limits.worker_limits
    name parameters
    (module Handlers)

let notify_branch w locator =
  let header, _ = (locator : Block_locator.t :> _ * _) in
  let hash = Block_header.hash header in
  let pv = Worker.state w in
  let sender_id = Distributed_db.my_peer_id pv.parameters.chain_db in
  (* sender and receiver are inverted here because they are from
     the point of view of the node sending the locator *)
  let seed = {Block_locator.sender_id=pv.peer_id; receiver_id=sender_id } in
  Worker.Dropbox.put_request w (New_branch (hash, locator, seed))

let notify_head w header =
  let hash = Block_header.hash header in
  Worker.Dropbox.put_request w (New_head (hash, header))

let shutdown w =
  Worker.shutdown w

let peer_id w =
  let pv = Worker.state w in
  pv.peer_id

let bootstrapped w =
  let pv = Worker.state w in
  pv.bootstrapped

let current_head w =
  let pv = Worker.state w in
  pv.last_validated_head

let status = Worker.status

let running_workers () = Worker.list table

let current_request t = Worker.current_request t

let last_events = Worker.last_events
