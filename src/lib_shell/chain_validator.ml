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

open Chain_validator_worker_state

module Log =
  Internal_event.Legacy_logging.Make(struct let name = "node.chain_validator" end)

module Name = struct
  type t = Chain_id.t
  let encoding = Chain_id.encoding
  let base = [ "validator.chain" ]
  let pp = Chain_id.pp_short
end

module Request = struct
  include Request
  type _ t = Validated : State.Block.t -> Event.update t
  let view (type a) (Validated block : a t) : view =
    State.Block.hash block
end

type limits = {
  bootstrap_threshold: int ;
  worker_limits: Worker_types.limits ;
}

module Types = struct
  include Worker_state

  type parameters = {
    parent: Name.t option ;
    db: Distributed_db.t ;
    chain_state: State.Chain.t ;
    chain_db: Distributed_db.chain_db ;
    block_validator: Block_validator.t ;
    global_valid_block_input: State.Block.t Lwt_watcher.input ;
    global_chains_input: (Chain_id.t * bool) Lwt_watcher.input ;

    prevalidator_limits: Prevalidator.limits ;
    peer_validator_limits: Peer_validator.limits ;
    max_child_ttl: int option ;
    limits: limits;
  }

  type state = {
    parameters: parameters ;

    mutable bootstrapped: bool ;
    bootstrapped_waiter: unit Lwt.t ;
    bootstrapped_wakener: unit Lwt.u ;
    valid_block_input: State.Block.t Lwt_watcher.input ;
    new_head_input: State.Block.t Lwt_watcher.input ;

    mutable child:
      (state * (unit -> unit Lwt.t (* shutdown *))) option ;
    mutable prevalidator: Prevalidator.t option ;
    active_peers: Peer_validator.t tzresult Lwt.t P2p_peer.Table.t ;
    bootstrapped_peers: unit P2p_peer.Table.t ;
  }

  let view (state : state) _ : view =
    let { bootstrapped ; active_peers ; bootstrapped_peers ; _ } = state in
    { bootstrapped ;
      active_peers =
        P2p_peer.Table.fold (fun id _ l -> id :: l) active_peers [] ;
      bootstrapped_peers =
        P2p_peer.Table.fold (fun id _ l -> id :: l) bootstrapped_peers [] }
end

module Worker = Worker.Make (Name) (Event) (Request) (Types)

open Types

type t = Worker.infinite Worker.queue Worker.t

let table = Worker.create_table Queue

let shutdown w =
  Worker.shutdown w

let shutdown_child nv active_chains =
  Lwt_utils.may ~f:(fun
                     ({ parameters = { chain_state ; global_chains_input ; _ } ; _ }, shutdown) ->
                     Lwt_watcher.notify global_chains_input (State.Chain.id chain_state, false) ;
                     Chain_id.Table.remove active_chains (State.Chain.id chain_state) ;
                     State.update_chain_data nv.parameters.chain_state begin fun _ chain_data ->
                       Lwt.return (Some { chain_data with test_chain = None }, ())
                     end >>= fun () ->
                     shutdown () >>= fun () ->
                     nv.child <- None ;
                     Lwt.return_unit
                   ) nv.child

let notify_new_block w block =
  let nv = Worker.state w in
  Option.iter nv.parameters.parent
    ~f:(fun id -> try
           let w = List.assoc id (Worker.list table) in
           let nv = Worker.state w in
           Lwt_watcher.notify nv.valid_block_input block
         with Not_found -> ()) ;
  Lwt_watcher.notify nv.valid_block_input block ;
  Lwt_watcher.notify nv.parameters.global_valid_block_input block ;
  Worker.Queue.push_request_now w (Validated block)

let may_toggle_bootstrapped_chain w =
  let nv = Worker.state w in
  if not nv.bootstrapped &&
     P2p_peer.Table.length nv.bootstrapped_peers >= nv.parameters.limits.bootstrap_threshold
  then begin
    Log.log_info "bootstrapped";
    nv.bootstrapped <- true ;
    Lwt.wakeup_later nv.bootstrapped_wakener () ;
  end

let with_activated_peer_validator w peer_id f =
  let nv = Worker.state w in
  begin
    match P2p_peer.Table.find_opt nv.active_peers peer_id with
    | Some pv -> pv
    | None ->
        let pv =
          Peer_validator.create
            ~notify_new_block:(notify_new_block w)
            ~notify_bootstrapped: begin fun () ->
              P2p_peer.Table.add nv.bootstrapped_peers peer_id () ;
              may_toggle_bootstrapped_chain w
            end
            ~notify_termination: begin fun _pv ->
              P2p_peer.Table.remove nv.active_peers peer_id ;
              P2p_peer.Table.remove nv.bootstrapped_peers peer_id ;
            end
            nv.parameters.peer_validator_limits
            nv.parameters.block_validator
            nv.parameters.chain_db
            peer_id in
        P2p_peer.Table.add nv.active_peers peer_id pv ;
        pv
  end >>= function
  | Error _ as e ->
      P2p_peer.Table.remove nv.active_peers peer_id ;
      Lwt.return e
  | Ok pv ->
      match Peer_validator.status pv with
      | Worker_types.Running _ -> f pv
      | Worker_types.Closing (_, _)
      | Worker_types.Closed (_, _, _)
      | Worker_types.Launching _ -> return_unit

let may_update_checkpoint chain_state new_head =
  State.Chain.checkpoint chain_state >>= fun (old_level, _old_block) ->
  let new_level = State.Block.last_allowed_fork_level new_head in
  if new_level <= old_level then
    Lwt.return_unit
  else
    let head_level = State.Block.level new_head in
    State.Block.predecessor_n new_head
      (Int32.to_int (Int32.sub head_level new_level)) >>= function
    | None -> Lwt.return_unit (* should not happen *)
    | Some new_block ->
        State.Chain.set_checkpoint chain_state (new_level, new_block)

let may_switch_test_chain w active_chains spawn_child block =
  let nv = Worker.state w in
  let create_child block protocol expiration forking_block =
    let block_header = State.Block.header block in
    let genesis = Context.compute_testchain_genesis (State.Block.hash forking_block) in
    let chain_id = Context.compute_testchain_chain_id genesis in
    let activated =
      match nv.child with
      | None -> false
      | Some (child , _) ->
          Block_hash.equal
            (State.Chain.genesis child.parameters.chain_state).block
            genesis in
    begin
      match nv.parameters.max_child_ttl with
      | None -> Lwt.return_false
      | Some ttl ->
          let forking_block_timestamp =
            (State.Block.shell_header forking_block).Block_header.timestamp
          in
          let expiration =
            let open Time.Protocol in
            min expiration (add forking_block_timestamp (Int64.of_int ttl)) in
          Lwt.return (expiration < block_header.shell.timestamp)
    end >>= fun locally_expired ->
    if locally_expired && activated then
      shutdown_child nv active_chains >>= return
    else if activated
         || locally_expired
         || not (State.Chain.allow_forked_chain nv.parameters.chain_state) then
      return_unit
    else begin
      begin
        State.Chain.get
          (State.Chain.global_state nv.parameters.chain_state)
          chain_id >>= function
        | Ok chain_state ->
            State.update_testchain block ~testchain_state:chain_state >>= fun () ->
            return chain_state
        | Error _ -> (* TODO proper error matching (Not_found ?) or use `get_opt` ? *)
            State.Block.context forking_block >>= fun context ->
            let try_init_test_chain cont =
              Block_validation.init_test_chain
                context (State.Block.header forking_block) >>= function
              | Ok genesis_header ->
                  State.fork_testchain
                    block chain_id genesis genesis_header protocol expiration >>=? fun chain_state ->
                  Chain.head chain_state >>= fun new_genesis_block ->
                  Lwt_watcher.notify nv.parameters.global_valid_block_input new_genesis_block ;
                  Lwt_watcher.notify nv.valid_block_input new_genesis_block ;
                  return chain_state
              | Error [ Block_validator_errors.Missing_test_protocol missing_protocol ] ->
                  Block_validator.fetch_and_compile_protocol
                    nv.parameters.block_validator
                    missing_protocol >>=? fun _ ->
                  cont ()
              | Error _ as errs -> Lwt.return errs
            in
            try_init_test_chain @@ fun () ->
            try_init_test_chain @@ fun () ->
            failwith "Could not retrieve test protocol"
      end >>=? fun chain_state ->
      (* [spawn_child] is a callback to [create_node]. Thus, it takes care of
         global initialization boilerplate (e.g. notifying [global_chains_input],
         adding the chain to the correct tables, ...) *)
      spawn_child
        ~parent:(State.Chain.id chain_state)
        nv.parameters.peer_validator_limits
        nv.parameters.prevalidator_limits
        nv.parameters.block_validator
        nv.parameters.global_valid_block_input
        nv.parameters.global_chains_input
        nv.parameters.db chain_state
        nv.parameters.limits (* TODO: different limits main/test ? *) >>=? fun child ->
      nv.child <- Some child ;
      return_unit
    end
  in
  begin
    State.Block.test_chain block >>= function
    | Not_running, _ -> shutdown_child nv active_chains >>= return
    | (Forking _ | Running _), None -> return_unit (* only for snapshots *)
    | (Forking { protocol ; expiration ; _ }
      | Running { protocol ; expiration ; _ }), Some forking_block ->
        create_child block protocol expiration forking_block
  end >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      Worker.record_event w (Could_not_switch_testchain err) ;
      Lwt.return_unit

let broadcast_head w ~previous block =
  let nv = Worker.state w in
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
      Distributed_db.Advertise.current_head
        nv.parameters.chain_db block ;
      Lwt.return_unit
    end else begin
      Distributed_db.Advertise.current_branch nv.parameters.chain_db
    end
  end

let safe_get_protocol hash =
  match Registered_protocol.get hash with
  | None ->
      (* FIXME. *)
      (* This should not happen: it should be handled in the validator. *)
      failwith "chain_validator: missing protocol '%a' for the current block."
        Protocol_hash.pp_short hash
  | Some protocol ->
      return protocol

let on_request (type a) w
    start_testchain active_chains spawn_child (req : a Request.t) : a tzresult Lwt.t =
  let Request.Validated block = req in
  let nv = Worker.state w in
  Chain.head nv.parameters.chain_state >>= fun head ->
  let head_header = State.Block.header head
  and head_hash = State.Block.hash head
  and block_header = State.Block.header block
  and block_hash = State.Block.hash block in
  begin
    match nv.prevalidator with
    | None ->
        Lwt.return head_header.shell.fitness
    | Some pv ->
        Prevalidator.fitness pv
  end >>= fun context_fitness ->
  let head_fitness = head_header.shell.fitness in
  let new_fitness = block_header.shell.fitness in
  let accepted_head =
    if Fitness.(context_fitness = head_fitness) then
      Fitness.(new_fitness > head_fitness)
    else
      Fitness.(new_fitness >= context_fitness) in
  if not accepted_head then
    return Event.Ignored_head
  else begin
    Chain.set_head nv.parameters.chain_state block >>= fun previous ->
    may_update_checkpoint nv.parameters.chain_state block >>= fun () ->
    broadcast_head w ~previous block >>= fun () ->
    begin match nv.prevalidator with
      | Some old_prevalidator ->
          State.Block.protocol_hash block >>= fun new_protocol ->
          let old_protocol = Prevalidator.protocol_hash old_prevalidator in
          begin
            if not (Protocol_hash.equal old_protocol new_protocol) then begin
              safe_get_protocol new_protocol >>=? fun (module Proto) ->
              let (limits, chain_db) = Prevalidator.parameters old_prevalidator in
              (* TODO inject in the new prevalidator the operation
                 from the previous one. *)
              Prevalidator.create limits (module Proto) chain_db >>= function
              | Error errs ->
                  Log.lwt_log_error "@[Failed to reinstantiate prevalidator:@ %a@]"
                    pp_print_error errs >>= fun () ->
                  nv.prevalidator <- None ;
                  Prevalidator.shutdown old_prevalidator >>= fun () ->
                  return_unit
              | Ok prevalidator ->
                  nv.prevalidator <- Some prevalidator ;
                  Prevalidator.shutdown old_prevalidator >>= fun () ->
                  return_unit
            end else begin
              Prevalidator.flush old_prevalidator block_hash >>=? fun () ->
              return_unit
            end
          end >>=? fun () ->
          return_unit
      | None -> return_unit
    end >>=? fun () ->
    (if start_testchain then
       may_switch_test_chain w active_chains spawn_child block
     else
       Lwt.return_unit) >>= fun () ->
    Lwt_watcher.notify nv.new_head_input block ;
    if Block_hash.equal head_hash block_header.shell.predecessor then
      return Event.Head_incrememt
    else
      return Event.Branch_switch
  end

let on_completion (type a) w  (req : a Request.t) (update : a) request_status =
  let Request.Validated block = req in
  let fitness = State.Block.fitness block in
  let request = State.Block.hash block in
  Worker.record_event w (Processed_block { request ; request_status ; update ; fitness }) ;
  Lwt.return_unit

let on_close w =
  let nv = Worker.state w in
  Distributed_db.deactivate nv.parameters.chain_db >>= fun () ->
  begin
    P2p_peer.Table.fold
      (fun peer_id pv acc ->
         acc >>= fun acc ->
         pv >|= function
         | Ok pv -> Peer_validator.shutdown pv :: acc
         | Error _ ->
             P2p_peer.Table.remove nv.active_peers peer_id ;
             acc)
      nv.active_peers (Lwt.return_nil)
  end >>= fun pvs ->
  Lwt.join
    (begin match nv.prevalidator with
       | Some prevalidator -> Prevalidator.shutdown prevalidator
       | None -> Lwt.return_unit
     end ::
     Lwt_utils.may ~f:(fun (_, shutdown) -> shutdown ()) nv.child ::
     pvs) >>= fun () ->
  Lwt.return_unit

let on_launch start_prevalidator w _ parameters =
  Chain.init_head parameters.chain_state >>= fun () ->
  (if start_prevalidator then
     State.read_chain_data parameters.chain_state
       (fun _ { State.current_head ; _ } -> Lwt.return current_head) >>= fun head ->
     State.Block.protocol_hash head >>= fun head_hash ->
     safe_get_protocol head_hash >>= function
     | Ok (module Proto) -> begin
         Prevalidator.create
           parameters.prevalidator_limits
           (module Proto)
           parameters.chain_db >>= function
         | Error err ->
             Log.lwt_log_error "@[Failed to instantiate prevalidator:@ %a@]"
               pp_print_error err >>= fun () ->
             return_none
         | Ok prevalidator ->
             return_some prevalidator
       end
     | Error err ->
         Log.lwt_log_error "@[Failed to instantiate prevalidator:@ %a@]"
           pp_print_error err >>= fun () ->
         return_none
   else return_none) >>=? fun prevalidator ->
  let valid_block_input = Lwt_watcher.create_input () in
  let new_head_input = Lwt_watcher.create_input () in
  let bootstrapped_waiter, bootstrapped_wakener = Lwt.wait () in
  let nv =
    { parameters ;
      valid_block_input ;
      new_head_input ;
      bootstrapped_wakener ;
      bootstrapped_waiter ;
      bootstrapped = (parameters.limits.bootstrap_threshold <= 0) ;
      active_peers =
        P2p_peer.Table.create 50 ; (* TODO use `2 * max_connection` *)
      bootstrapped_peers =
        P2p_peer.Table.create 50 ; (* TODO use `2 * max_connection` *)
      child = None ;
      prevalidator } in
  if nv.bootstrapped then Lwt.wakeup_later bootstrapped_wakener () ;
  Distributed_db.set_callback parameters.chain_db {
    notify_branch = begin fun peer_id locator ->
      Lwt.async begin fun () ->
        with_activated_peer_validator w peer_id @@ fun pv ->
        Peer_validator.notify_branch pv locator ;
        return_unit
      end
    end ;
    notify_head = begin fun peer_id block ops ->
      Lwt.async begin fun () ->
        with_activated_peer_validator w peer_id (fun pv ->
            Peer_validator.notify_head pv block ;
            return_unit) >>=? fun () ->
        (* TODO notify prevalidator only if head is known ??? *)
        match nv.prevalidator with
        | Some prevalidator ->
            Prevalidator.notify_operations prevalidator peer_id ops >>= fun () ->
            return_unit
        | None -> return_unit
      end;
    end ;
    disconnection = begin fun peer_id ->
      Lwt.async begin fun () ->
        let nv = Worker.state w in
        match P2p_peer.Table.find_opt nv.active_peers peer_id with
        | None -> return_unit
        | Some pv ->
            pv >>= function
            | Error _ as e ->
                P2p_peer.Table.remove nv.active_peers peer_id ;
                Lwt.return e
            | Ok pv ->
                Peer_validator.shutdown pv >>= fun () ->
                return_unit
      end
    end ;
  } ;
  return nv

let rec create
    ?max_child_ttl ~start_prevalidator ~start_testchain ~active_chains ?parent
    peer_validator_limits prevalidator_limits block_validator
    global_valid_block_input
    global_chains_input
    db chain_state limits =
  let spawn_child ~parent pvl pl bl gvbi gci db n l =
    create ~start_prevalidator ~start_testchain ~active_chains ~parent pvl pl bl gvbi gci db n l >>=? fun w ->
    return (Worker.state w, (fun () -> Worker.shutdown w)) in
  let module Handlers = struct
    type self = t
    let on_launch = on_launch start_prevalidator
    let on_request w = on_request w start_testchain active_chains spawn_child
    let on_close = on_close
    let on_error _ _ _ errs = Lwt.return_error errs
    let on_completion = on_completion
    let on_no_request _ = return_unit
  end in
  let parameters =
    { max_child_ttl ;
      parent ;
      peer_validator_limits ;
      prevalidator_limits ;
      block_validator ;
      global_valid_block_input ;
      global_chains_input ;
      db ;
      chain_db = Distributed_db.activate db chain_state ;
      chain_state ;
      limits } in
  Worker.launch table
    prevalidator_limits.worker_limits
    (State.Chain.id chain_state)
    parameters
    (module Handlers) >>=? fun w ->
  Chain_id.Table.add active_chains (State.Chain.id chain_state) w ;
  Lwt_watcher.notify global_chains_input (State.Chain.id chain_state, true) ;
  return w

(** Current block computation *)

let create
    ?max_child_ttl
    ~start_prevalidator
    ~start_testchain
    ~active_chains
    peer_validator_limits prevalidator_limits
    block_validator
    global_valid_block_input
    global_chains_input
    global_db state limits =
  (* hide the optional ?parent *)
  create
    ?max_child_ttl
    ~start_prevalidator
    ~start_testchain
    ~active_chains
    peer_validator_limits prevalidator_limits
    block_validator
    global_valid_block_input
    global_chains_input
    global_db state limits

let chain_id w =
  let { parameters = { chain_state ; _ } ; _ } = Worker.state w in
  State.Chain.id chain_state

let chain_state w =
  let { parameters = { chain_state ; _ } ; _ } = Worker.state w in
  chain_state

let prevalidator w =
  let { prevalidator ; _ } = Worker.state w in
  prevalidator

let chain_db w =
  let { parameters = { chain_db ; _ } ; _ } = Worker.state w in
  chain_db

let child w =
  match (Worker.state w).child with
  | None -> None
  | Some ({ parameters = { chain_state ;_ } ; _ }, _) ->
      try Some (List.assoc (State.Chain.id chain_state) (Worker.list table))
      with Not_found -> None

let assert_fitness_increases ?(force = false) w distant_header =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  Chain.head chain_state >>= fun local_header ->
  fail_when
    (not force &&
     Fitness.compare
       distant_header.Block_header.shell.fitness
       (State.Block.fitness local_header) <= 0)
    (failure "Fitness too low")

let assert_checkpoint w hash (header: Block_header.t) =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  State.Chain.acceptable_block chain_state hash header >>= fun acceptable ->
  fail_unless acceptable
    (Validation_errors.Checkpoint_error (hash, None))

let validate_block w ?force hash block operations =
  let nv = Worker.state w in
  assert (Block_hash.equal hash (Block_header.hash block)) ;
  assert_fitness_increases ?force w block >>=? fun () ->
  assert_checkpoint w hash block >>=? fun () ->
  Block_validator.validate
    ~canceler:(Worker.canceler w)
    ~notify_new_block:(notify_new_block w)
    nv.parameters.block_validator
    nv.parameters.chain_db
    hash block operations

let bootstrapped w =
  let { bootstrapped_waiter ; _ } = Worker.state w in
  Lwt.protected bootstrapped_waiter

let valid_block_watcher w =
  let { valid_block_input ; _ } = Worker.state w in
  Lwt_watcher.create_stream valid_block_input

let new_head_watcher w =
  let { new_head_input ; _ } = Worker.state w in
  Lwt_watcher.create_stream new_head_input

let status = Worker.status

let running_workers () = Worker.list table

let pending_requests t = Worker.Queue.pending_requests t

let current_request t = Worker.current_request t

let last_events = Worker.last_events
