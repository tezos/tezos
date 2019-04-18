(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Message = Distributed_db_message

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net
type connection = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.connection

type 'a request_param = {
  p2p : (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.t ;
  data: 'a ;
  active: unit -> P2p_peer.Set.t ;
  send: P2p_peer.Id.t -> Message.t -> unit ;
}

module Make_raw
    (Hash : sig
       type t
       val name : string
       val encoding : t Data_encoding.t
       val pp : Format.formatter -> t -> unit

       module Logging : sig
         val tag : t Tag.def
       end
     end)
    (Disk_table :
       Distributed_db_functors.DISK_TABLE with type key := Hash.t)
    (Memory_table :
       Distributed_db_functors.MEMORY_TABLE with type key := Hash.t)
    (Request_message : sig
       type param
       val max_length : int
       val initial_delay : Time.System.Span.t
       val forge : param -> Hash.t list -> Message.t
     end)
    (Precheck : Distributed_db_functors.PRECHECK
     with type key := Hash.t
      and type value := Disk_table.value) = struct

  module Request = struct
    type param = Request_message.param request_param
    let active { active ; _ } = active ()
    let initial_delay  = Request_message.initial_delay

    let rec send state gid keys =
      let first_keys, keys = List.split_n Request_message.max_length keys in
      let msg = (Request_message.forge state.data first_keys) in
      state.send gid msg ;
      let open Peer_metadata in
      let (req : requests_kind) = match msg with
        | Get_current_branch _ -> Branch
        | Get_current_head _ -> Head
        | Get_block_headers _ -> Block_header
        | Get_operations _ -> Operations
        | Get_protocols _ -> Protocols
        | Get_operation_hashes_for_blocks _ -> Operation_hashes_for_block
        | Get_operations_for_blocks _ -> Operations_for_block
        | _ -> Other in
      let meta = P2p.get_peer_metadata state.p2p gid in
      Peer_metadata.incr meta @@ Scheduled_request req ;
      if keys <> [] then send state gid keys
  end

  module Scheduler =
    Distributed_db_functors.Make_request_scheduler
      (Hash) (Memory_table) (Request)
  module Table =
    Distributed_db_functors.Make_table
      (Hash) (Disk_table) (Memory_table) (Scheduler) (Precheck)

  type t = {
    scheduler: Scheduler.t ;
    table: Table.t ;
  }

  let create ?global_input request_param param =
    let scheduler = Scheduler.create request_param in
    let table = Table.create ?global_input scheduler param in
    { scheduler ; table }

  let shutdown { scheduler ; _ } =
    Scheduler.shutdown scheduler

end

module Fake_operation_storage = struct
  type store = State.Chain.t
  type value = Operation.t
  let known _ _ = Lwt.return_false
  let read _ _ = Lwt.return (Error_monad.error_exn Not_found)
  let read_opt _ _ = Lwt.return_none
end

module Raw_operation =
  Make_raw
    (Operation_hash)
    (Fake_operation_storage)
    (Operation_hash.Table)
    (struct
      type param = unit
      let max_length = 10
      let initial_delay = Time.System.Span.of_seconds_exn 0.5
      let forge () keys = Message.Get_operations keys
    end)
    (struct
      type param = unit
      type notified_value = Operation.t
      let precheck _ _ v = Some v
    end)

module Block_header_storage = struct
  type store = State.Chain.t
  type value = Block_header.t
  let known = State.Block.known_valid
  let read chain_state h =
    State.Block.read chain_state h >>=? fun b ->
    return (State.Block.header b)
  let read_opt chain_state h =
    State.Block.read_opt chain_state h >>= fun b ->
    Lwt.return (Option.map ~f:State.Block.header b)
end

module Raw_block_header =
  Make_raw
    (Block_hash)
    (Block_header_storage)
    (Block_hash.Table)
    (struct
      type param = unit
      let max_length = 10
      let initial_delay = Time.System.Span.of_seconds_exn 0.5
      let forge () keys = Message.Get_block_headers keys
    end)
    (struct
      type param = unit
      type notified_value = Block_header.t
      let precheck _ _ v = Some v
    end)

module Operation_hashes_storage = struct
  type store = State.Chain.t
  type value = Operation_hash.t list
  let known chain_state (h, _) = State.Block.known_valid chain_state h
  let read chain_state (h, i) =
    State.Block.read chain_state h >>=? fun b ->
    State.Block.operation_hashes b i >>= fun (ops, _) ->
    return ops
  let read_opt chain_state (h, i) =
    State.Block.read_opt chain_state h >>= function
    | None -> Lwt.return_none
    | Some b ->
        State.Block.operation_hashes b i >>= fun (ops, _) ->
        Lwt.return_some ops
end

module Operations_table =
  Hashtbl.Make(struct
    type t = Block_hash.t * int
    let hash = Hashtbl.hash
    let equal (b1, i1) (b2, i2) =
      Block_hash.equal b1 b2 && i1 = i2
  end)

module Raw_operation_hashes = struct

  include
    Make_raw
      (struct
        type t = Block_hash.t * int
        let name = "operation_hashes"
        let pp ppf (h, n) = Format.fprintf ppf "%a:%d" Block_hash.pp h n
        let encoding =
          let open Data_encoding in
          obj2 (req "block" Block_hash.encoding) (req "index" uint16)
        module Logging = struct
          let tag = Tag.def ~doc:"Operation hashes" "operation_hashes" pp
        end
      end)
      (Operation_hashes_storage)
      (Operations_table)
      (struct
        type param = unit
        let max_length = 10
        let initial_delay = Time.System.Span.of_seconds_exn 1.
        let forge () keys =
          Message.Get_operation_hashes_for_blocks keys
      end)
      (struct
        type param = Operation_list_list_hash.t
        type notified_value =
          Operation_hash.t list * Operation_list_list_hash.path
        let precheck (_block, expected_ofs) expected_hash (ops, path) =
          let received_hash, received_ofs =
            Operation_list_list_hash.check_path path
              (Operation_list_hash.compute ops) in
          if
            received_ofs = expected_ofs &&
            Operation_list_list_hash.compare expected_hash received_hash = 0
          then
            Some ops
          else
            None
      end)

  let clear_all table hash n =
    List.iter (fun i -> Table.clear_or_cancel table (hash, i)) (0 -- (n-1))

end

module Operations_storage = struct
  type store = State.Chain.t
  type value = Operation.t list
  let known chain_state (h, _) = State.Block.known_valid chain_state h
  let read chain_state (h, i) =
    State.Block.read chain_state h >>=? fun b ->
    State.Block.operations b i >>= fun (ops, _) ->
    return ops
  let read_opt chain_state (h, i) =
    State.Block.read_opt chain_state h >>= function
    | None -> Lwt.return_none
    | Some b ->
        State.Block.operations b i >>= fun (ops, _) ->
        Lwt.return_some ops
end

module Raw_operations = struct
  include
    Make_raw
      (struct
        type t = Block_hash.t * int
        let name = "operations"
        let pp ppf (h, n) = Format.fprintf ppf "%a:%d" Block_hash.pp h n
        let encoding =
          let open Data_encoding in
          obj2 (req "block" Block_hash.encoding) (req "index" uint16)
        module Logging = struct
          let tag = Tag.def ~doc:"Operations" "operations" pp
        end
      end)
      (Operations_storage)
      (Operations_table)
      (struct
        type param = unit
        let max_length = 10
        let initial_delay = Time.System.Span.of_seconds_exn 1.
        let forge () keys =
          Message.Get_operations_for_blocks keys
      end)
      (struct
        type param = Operation_list_list_hash.t
        type notified_value = Operation.t list * Operation_list_list_hash.path
        let precheck (_block, expected_ofs) expected_hash (ops, path) =
          let received_hash, received_ofs =
            Operation_list_list_hash.check_path path
              (Operation_list_hash.compute
                 (List.map Operation.hash ops)) in
          if
            received_ofs = expected_ofs &&
            Operation_list_list_hash.compare expected_hash received_hash = 0
          then
            Some ops
          else
            None
      end)

  let clear_all table hash n =
    List.iter (fun i -> Table.clear_or_cancel table (hash, i)) (0 -- (n-1))

end

module Protocol_storage = struct
  type store = State.t
  type value = Protocol.t
  let known = State.Protocol.known
  let read = State.Protocol.read
  let read_opt = State.Protocol.read_opt
end

module Raw_protocol =
  Make_raw
    (Protocol_hash)
    (Protocol_storage)
    (Protocol_hash.Table)
    (struct
      type param = unit
      let initial_delay = Time.System.Span.of_seconds_exn 10.
      let max_length = 10
      let forge () keys = Message.Get_protocols keys
    end)
    (struct
      type param = unit
      type notified_value = Protocol.t
      let precheck _ _ v = Some v
    end)

type callback = {
  notify_branch:
    P2p_peer.Id.t -> Block_locator.t -> unit ;
  notify_head:
    P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit ;
  disconnection: P2p_peer.Id.t -> unit ;
}

type db = {
  p2p: p2p ;
  p2p_readers: p2p_reader P2p_peer.Table.t ;
  disk: State.t ;
  active_chains: chain_db Chain_id.Table.t ;
  protocol_db: Raw_protocol.t ;
  block_input: (Block_hash.t * Block_header.t) Lwt_watcher.input ;
  operation_input: (Operation_hash.t * Operation.t) Lwt_watcher.input ;
}

and chain_db = {
  chain_state: State.Chain.t ;
  global_db: db ;
  operation_db: Raw_operation.t ;
  block_header_db: Raw_block_header.t ;
  operation_hashes_db: Raw_operation_hashes.t ;
  operations_db: Raw_operations.t ;
  mutable callback: callback ;
  active_peers: P2p_peer.Set.t ref ;
  active_connections: p2p_reader P2p_peer.Table.t ;
}

and p2p_reader = {
  gid: P2p_peer.Id.t ;
  conn: connection ;
  peer_active_chains: chain_db Chain_id.Table.t ;
  canceler: Lwt_canceler.t ;
  mutable worker: unit Lwt.t ;
}

let noop_callback = {
  notify_branch = begin fun _gid _locator -> () end ;
  notify_head =  begin fun _gid _block _ops -> () end ;
  disconnection = begin fun _gid -> () end ;
}

type t = db

let state { disk ; _ } = disk
let chain_state { chain_state ; _ } = chain_state
let db { global_db ; _ } = global_db

let my_peer_id chain_db = P2p.peer_id chain_db.global_db.p2p

let get_peer_metadata chain_db = P2p.get_peer_metadata chain_db.global_db.p2p

let read_block_header { disk ; _ } h =
  State.read_block disk h >>= function
  | Some b ->
      Lwt.return_some (State.Block.chain_id b, State.Block.header b)
  | None ->
      Lwt.return_none

let find_pending_block_header { peer_active_chains ; _ } h  =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_block_header.Table.pending
             chain_db.block_header_db.table h ->
           Some chain_db
       | None -> None)
    peer_active_chains
    None

let find_pending_operations { peer_active_chains ; _ } h i =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_operations.Table.pending
             chain_db.operations_db.table (h, i) ->
           Some chain_db
       | None -> None)
    peer_active_chains
    None

let find_pending_operation_hashes { peer_active_chains ; _ } h i =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_operation_hashes.Table.pending
             chain_db.operation_hashes_db.table (h, i) ->
           Some chain_db
       | None -> None)
    peer_active_chains
    None

let find_pending_operation { peer_active_chains ; _ } h =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_operation.Table.pending
             chain_db.operation_db.table h ->
           Some chain_db
       | None -> None)
    peer_active_chains
    None

let read_operation { active_chains ; _ } h =
  Chain_id.Table.fold
    (fun chain_id chain_db acc ->
       acc >>= function
       | Some _ -> acc
       | None ->
           Raw_operation.Table.read_opt
             chain_db.operation_db.table h >>= function
           | None -> Lwt.return_none
           | Some bh -> Lwt.return_some (chain_id, bh))
    active_chains
    Lwt.return_none

module P2p_reader = struct

  let may_activate global_db state chain_id f =
    match Chain_id.Table.find_opt state.peer_active_chains chain_id with
    | Some chain_db ->
        f chain_db
    | None ->
        match Chain_id.Table.find_opt global_db.active_chains chain_id with
        | Some chain_db ->
            chain_db.active_peers :=
              P2p_peer.Set.add state.gid !(chain_db.active_peers) ;
            P2p_peer.Table.add chain_db.active_connections
              state.gid state ;
            Chain_id.Table.add state.peer_active_chains chain_id chain_db ;
            f chain_db
        | None ->
            let meta = P2p.get_peer_metadata global_db.p2p state.gid in
            Peer_metadata.incr meta Unactivated_chain ;
            Lwt.return_unit

  let deactivate state chain_db =
    chain_db.callback.disconnection state.gid ;
    chain_db.active_peers :=
      P2p_peer.Set.remove state.gid !(chain_db.active_peers) ;
    P2p_peer.Table.remove chain_db.active_connections state.gid

  (* check if the chain advertized by a peer is (still) active *)
  let may_handle global_db state chain_id f =
    match Chain_id.Table.find_opt state.peer_active_chains chain_id with
    | None ->
        let meta = P2p.get_peer_metadata global_db.p2p state.gid in
        Peer_metadata.incr meta Inactive_chain ;
        Lwt.return_unit
    | Some chain_db ->
        f chain_db

  let may_handle_global global_db chain_id f =
    match Chain_id.Table.find_opt global_db.active_chains chain_id with
    | None ->
        Lwt.return_unit
    | Some chain_db ->
        f chain_db

  module Handle_msg_Logging =
    Internal_event.Legacy_logging.Make_semantic
      (struct let name = "node.distributed_db.p2p_reader" end)


  let soon () =
    let now = Systime_os.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 15) with
    | Some s -> s
    | None -> invalid_arg "Distributed_db.handle_msg: end of time"

  let handle_msg global_db state msg =

    let open Message in
    let open Handle_msg_Logging in
    let meta = P2p.get_peer_metadata global_db.p2p state.gid in

    lwt_debug Tag.DSL.(fun f ->
        f "Read message from %a: %a"
        -% t event "read_message"
        -% a P2p_peer.Id.Logging.tag state.gid
        -% a Message.Logging.tag msg) >>= fun () ->

    match msg with
    | Get_current_branch chain_id ->
        Peer_metadata.incr meta @@ Received_request Branch;
        may_handle_global global_db chain_id @@ fun chain_db ->
        if not (Chain_id.Table.mem state.peer_active_chains chain_id) then
          Peer_metadata.update_requests meta Branch @@
          P2p.try_send global_db.p2p state.conn @@
          Get_current_branch chain_id ;
        let seed = {
          Block_locator.receiver_id=state.gid;
          sender_id=(my_peer_id chain_db) } in
        (Chain.locator chain_db.chain_state seed) >>= fun locator ->
        Peer_metadata.update_responses meta Branch @@
        P2p.try_send global_db.p2p state.conn @@
        Current_branch (chain_id, locator)  ;
        Lwt.return_unit

    | Current_branch (chain_id, locator) ->
        may_activate global_db state chain_id @@ fun chain_db ->
        let head, hist = (locator :> Block_header.t * Block_hash.t list) in
        Lwt_list.exists_p
          (State.Block.known_invalid chain_db.chain_state)
          (Block_header.hash head :: hist) >>= fun known_invalid ->
        if known_invalid then begin
          P2p.disconnect global_db.p2p state.conn >>= fun () ->
          P2p.greylist_peer global_db.p2p state.gid ;
          Lwt.return_unit
        end else
        if Time.System.(soon () < of_protocol_exn head.shell.timestamp) then begin
          Peer_metadata.incr meta Future_block ;
          lwt_log_notice Tag.DSL.(fun f ->
              f "Received future block %a from peer %a."
              -% t event "received_future_block"
              -% a Block_hash.Logging.tag (Block_header.hash head)
              -% a P2p_peer.Id.Logging.tag state.gid) >>= fun () ->
          Lwt.return_unit
        end else begin
          chain_db.callback.notify_branch state.gid locator ;
          (* TODO discriminate between received advertisements
             and responses? *)
          Peer_metadata.incr meta @@ Received_advertisement Branch ;
          Lwt.return_unit
        end

    | Deactivate chain_id ->
        may_handle global_db state chain_id @@ fun chain_db ->
        deactivate state chain_db ;
        Chain_id.Table.remove state.peer_active_chains chain_id ;
        Lwt.return_unit

    | Get_current_head chain_id ->
        may_handle global_db state chain_id @@ fun chain_db ->
        Peer_metadata.incr meta @@ Received_request Head ;
        let { Connection_metadata.disable_mempool ; _ } =
          P2p.connection_remote_metadata chain_db.global_db.p2p state.conn in
        begin
          if disable_mempool then
            Chain.head chain_db.chain_state >>= fun head ->
            Lwt.return (State.Block.header head, Mempool.empty)
          else
            State.Current_mempool.get chain_db.chain_state
        end >>= fun (head, mempool) ->
        (* TODO bound the sent mempool size *)
        Peer_metadata.update_responses meta Head @@
        P2p.try_send global_db.p2p state.conn @@
        Current_head (chain_id, head, mempool) ;
        Lwt.return_unit

    | Current_head (chain_id, header, mempool) ->
        may_handle global_db state chain_id @@ fun chain_db ->
        let head = Block_header.hash header in
        State.Block.known_invalid chain_db.chain_state head >>= fun known_invalid ->
        let { Connection_metadata.disable_mempool ; _ } =
          P2p.connection_local_metadata chain_db.global_db.p2p state.conn in
        let known_invalid =
          known_invalid ||
          (disable_mempool && mempool <> Mempool.empty)
          (* A non-empty mempool was received while mempool is desactivated,
               so the message is ignored.
               This should probably warrant a reduction of the sender's score. *)
        in
        if known_invalid then begin
          P2p.disconnect global_db.p2p state.conn >>= fun () ->
          P2p.greylist_peer global_db.p2p state.gid ;
          Lwt.return_unit
        end else if Time.System.(soon () < of_protocol_exn header.shell.timestamp) then begin
          Peer_metadata.incr meta Future_block ;
          lwt_log_notice Tag.DSL.(fun f ->
              f "Received future block %a from peer %a."
              -% t event "received_future_block"
              -% a Block_hash.Logging.tag head
              -% a P2p_peer.Id.Logging.tag state.gid) >>= fun () ->
          Lwt.return_unit
        end else begin
          chain_db.callback.notify_head state.gid header mempool ;
          (* TODO discriminate between received advertisements
             and responses? *)
          Peer_metadata.incr meta @@ Received_advertisement Head ;
          Lwt.return_unit
        end

    | Get_block_headers hashes ->
        Peer_metadata.incr meta @@ Received_request Block_header ;
        Lwt_list.iter_p
          (fun hash ->
             read_block_header global_db hash >>= function
             | None ->
                 Peer_metadata.incr meta @@ Unadvertised Block ;
                 Lwt.return_unit
             | Some (_chain_id, header) ->
                 Peer_metadata.update_responses meta Block_header @@
                 P2p.try_send global_db.p2p state.conn @@
                 Block_header header ;
                 Lwt.return_unit)
          hashes
    | Block_header block -> begin
        let hash = Block_header.hash block in
        match find_pending_block_header state hash with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Raw_block_header.Table.notify
              chain_db.block_header_db.table state.gid hash block >>= fun () ->
            Peer_metadata.incr meta @@ Received_response Block_header ;
            Lwt.return_unit
      end

    | Get_operations hashes ->
        Peer_metadata.incr meta @@ Received_request Operations ;
        Lwt_list.iter_p
          (fun hash ->
             read_operation global_db hash >>= function
             | None ->
                 Peer_metadata.incr meta @@ Unadvertised Operations ;
                 Lwt.return_unit
             | Some (_chain_id, op) ->
                 Peer_metadata.update_responses meta Operations @@
                 P2p.try_send global_db.p2p state.conn @@
                 Operation op ;
                 Lwt.return_unit)
          hashes

    | Operation operation -> begin
        let hash = Operation.hash operation in
        match find_pending_operation state hash with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Raw_operation.Table.notify
              chain_db.operation_db.table state.gid hash operation >>= fun () ->
            Peer_metadata.incr meta @@ Received_response Operations ;
            Lwt.return_unit
      end

    | Get_protocols hashes ->
        Peer_metadata.incr meta @@ Received_request Protocols ;
        Lwt_list.iter_p
          (fun hash ->
             State.Protocol.read_opt global_db.disk hash >>= function
             | None ->
                 Peer_metadata.incr meta @@ Unadvertised Protocol ;
                 Lwt.return_unit
             | Some p ->
                 Peer_metadata.update_responses meta Protocols @@
                 P2p.try_send global_db.p2p state.conn @@
                 Protocol p ;
                 Lwt.return_unit)
          hashes

    | Protocol protocol ->
        let hash = Protocol.hash protocol in
        Raw_protocol.Table.notify
          global_db.protocol_db.table state.gid hash protocol >>= fun () ->
        Peer_metadata.incr meta @@ Received_response Protocols ;
        Lwt.return_unit

    | Get_operation_hashes_for_blocks blocks ->
        Peer_metadata.incr meta @@
        Received_request Operation_hashes_for_block ;
        Lwt_list.iter_p
          (fun (hash, ofs) ->
             State.read_block global_db.disk hash >>= function
             | None -> Lwt.return_unit
             | Some block ->
                 State.Block.operation_hashes
                   block ofs >>= fun (hashes, path) ->
                 Peer_metadata.update_responses meta
                   Operation_hashes_for_block @@
                 P2p.try_send global_db.p2p state.conn @@
                 Operation_hashes_for_block (hash, ofs, hashes, path) ;
                 Lwt.return_unit)
          blocks

    | Operation_hashes_for_block (block, ofs, ops, path) -> begin
        match find_pending_operation_hashes state block ofs with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Raw_operation_hashes.Table.notify
              chain_db.operation_hashes_db.table state.gid
              (block, ofs) (ops, path) >>= fun () ->
            Peer_metadata.incr meta @@
            Received_response Operation_hashes_for_block ;
            Lwt.return_unit
      end

    | Get_operations_for_blocks blocks ->
        Peer_metadata.incr meta @@
        Received_request Operations_for_block ;
        Lwt_list.iter_p
          (fun (hash, ofs) ->
             State.read_block global_db.disk hash >>= function
             | None -> Lwt.return_unit
             | Some block ->
                 State.Block.operations
                   block ofs >>= fun (ops, path) ->
                 Peer_metadata.update_responses meta
                   Operations_for_block @@
                 P2p.try_send global_db.p2p state.conn @@
                 Operations_for_block (hash, ofs, ops, path) ;
                 Lwt.return_unit)
          blocks

    | Operations_for_block (block, ofs, ops, path) -> begin
        match find_pending_operations state block ofs with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Raw_operations.Table.notify
              chain_db.operations_db.table state.gid
              (block, ofs) (ops, path) >>= fun () ->
            Peer_metadata.incr meta @@
            Received_response Operations_for_block ;
            Lwt.return_unit
      end

  let rec worker_loop global_db state =
    protect ~canceler:state.canceler begin fun () ->
      P2p.recv global_db.p2p state.conn
    end >>= function
    | Ok msg ->
        handle_msg global_db state msg >>= fun () ->
        worker_loop global_db state
    | Error _ ->
        Chain_id.Table.iter
          (fun _ -> deactivate state)
          state.peer_active_chains ;
        P2p_peer.Table.remove global_db.p2p_readers state.gid ;
        Lwt.return_unit

  let run db gid conn =
    let canceler = Lwt_canceler.create () in
    let state = {
      conn ; gid ; canceler ;
      peer_active_chains = Chain_id.Table.create 17 ;
      worker = Lwt.return_unit ;
    } in
    Chain_id.Table.iter (fun chain_id _chain_db ->
        Lwt.async begin fun () ->
          let meta = P2p.get_peer_metadata db.p2p gid in
          Peer_metadata.incr meta (Sent_request Branch) ;
          P2p.send db.p2p conn (Get_current_branch chain_id)
        end)
      db.active_chains ;
    state.worker <-
      Lwt_utils.worker
        (Format.asprintf "db_network_reader.%a"
           P2p_peer.Id.pp_short gid)
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop db state)
        ~cancel:(fun () -> Lwt_canceler.cancel canceler) ;
    P2p_peer.Table.add db.p2p_readers gid state

  let shutdown s =
    Lwt_canceler.cancel s.canceler >>= fun () ->
    s.worker

end

let active_peer_ids p2p () =
  List.fold_left
    (fun acc conn ->
       let { P2p_connection.Info.peer_id ; _ } = P2p.connection_info p2p conn in
       P2p_peer.Set.add peer_id acc)
    P2p_peer.Set.empty
    (P2p.connections p2p)

let raw_try_send p2p peer_id msg =
  match P2p.find_connection p2p peer_id with
  | None -> ()
  | Some conn -> ignore (P2p.try_send p2p conn msg : bool)


let create disk p2p =
  let global_request =
    { p2p ;
      data = () ;
      active = active_peer_ids p2p ;
      send = raw_try_send p2p ;
    } in
  let protocol_db = Raw_protocol.create global_request disk in
  let active_chains = Chain_id.Table.create 17 in
  let p2p_readers = P2p_peer.Table.create 17 in
  let block_input = Lwt_watcher.create_input () in
  let operation_input = Lwt_watcher.create_input () in
  let db =
    { p2p ; p2p_readers ; disk ;
      active_chains ; protocol_db ;
      block_input ; operation_input ;
    } in
  db

let activate ({ p2p ; active_chains ; _ } as global_db) chain_state =
  P2p.on_new_connection p2p (P2p_reader.run global_db) ;
  P2p.iter_connections p2p (P2p_reader.run global_db) ;
  P2p.activate p2p;
  let chain_id = State.Chain.id chain_state in
  match Chain_id.Table.find_opt active_chains chain_id with
  | None ->
      let active_peers = ref P2p_peer.Set.empty in
      let p2p_request =
        { p2p ;
          data = () ;
          active = (fun () -> !active_peers) ;
          send = raw_try_send p2p ;
        } in
      let operation_db =
        Raw_operation.create
          ~global_input:global_db.operation_input p2p_request chain_state in
      let block_header_db =
        Raw_block_header.create
          ~global_input:global_db.block_input p2p_request chain_state in
      let operation_hashes_db =
        Raw_operation_hashes.create p2p_request chain_state in
      let operations_db =
        Raw_operations.create p2p_request chain_state in
      let chain = {
        global_db ; operation_db ; block_header_db ;
        operation_hashes_db ; operations_db ;
        chain_state ; callback = noop_callback ; active_peers ;
        active_connections = P2p_peer.Table.create 53 ;
      } in
      P2p.iter_connections p2p (fun _peer_id conn ->
          Lwt.async begin fun () ->
            P2p.send p2p conn (Get_current_branch chain_id)
          end) ;
      Chain_id.Table.add active_chains chain_id chain ;
      chain
  | Some chain ->
      chain

let set_callback chain_db callback =
  chain_db.callback <- callback

let deactivate chain_db =
  let { active_chains ; p2p ; _ } = chain_db.global_db in
  let chain_id = State.Chain.id chain_db.chain_state in
  Chain_id.Table.remove active_chains chain_id ;
  P2p_peer.Table.iter
    (fun _peer_id reader ->
       P2p_reader.deactivate reader chain_db  ;
       Lwt.async begin fun () ->
         P2p.send p2p reader.conn (Deactivate chain_id)
       end)
    chain_db.active_connections ;
  Raw_operation.shutdown chain_db.operation_db >>= fun () ->
  Raw_block_header.shutdown chain_db.block_header_db >>= fun () ->
  Lwt.return_unit >>= fun () ->
  Lwt.return_unit

let get_chain { active_chains ; _ } chain_id =
  Chain_id.Table.find_opt active_chains chain_id

let greylist { global_db = { p2p ; _ } ; _ } peer_id =
  Lwt.return (P2p.greylist_peer p2p peer_id)

let disconnect { global_db = { p2p ; _ } ; _ } peer_id =
  match P2p.find_connection p2p peer_id with
  | None -> Lwt.return_unit
  | Some conn -> P2p.disconnect p2p conn

let shutdown { p2p_readers ; active_chains ; _ } =
  P2p_peer.Table.fold
    (fun _peer_id reader acc ->
       P2p_reader.shutdown reader >>= fun () -> acc)
    p2p_readers
    Lwt.return_unit >>= fun () ->
  Chain_id.Table.fold
    (fun _ chain_db acc ->
       Raw_operation.shutdown chain_db.operation_db >>= fun () ->
       Raw_block_header.shutdown chain_db.block_header_db >>= fun () ->
       acc)
    active_chains
    Lwt.return_unit >>= fun () ->
  Lwt.return_unit

let clear_block chain_db hash n =
  Raw_operations.clear_all chain_db.operations_db.table hash n ;
  Raw_operation_hashes.clear_all chain_db.operation_hashes_db.table hash n ;
  Raw_block_header.Table.clear_or_cancel chain_db.block_header_db.table hash

let commit_block chain_db hash
    header header_data operations operations_data result
    ~forking_testchain =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  assert (List.length operations = header.shell.validation_passes) ;
  State.Block.store chain_db.chain_state
    header header_data operations operations_data result
    ~forking_testchain >>=? fun res ->
  Raw_block_header.Table.resolve_pending chain_db.block_header_db.table hash header;
  clear_block chain_db hash header.shell.validation_passes ;
  return res

let commit_invalid_block chain_db hash header errors =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  State.Block.store_invalid chain_db.chain_state header errors >>=? fun res ->
  clear_block chain_db hash header.shell.validation_passes ;
  return res

let inject_operation chain_db h op =
  assert (Operation_hash.equal h (Operation.hash op)) ;
  Raw_operation.Table.inject chain_db.operation_db.table h op

let commit_protocol db h p =
  State.Protocol.store db.disk p >>= fun res ->
  Raw_protocol.Table.clear_or_cancel db.protocol_db.table h ;
  return (res <> None)

let watch_block_header { block_input ; _ } =
  Lwt_watcher.create_stream block_input
let watch_operation { operation_input ; _ } =
  Lwt_watcher.create_stream operation_input

module Raw = struct
  let encoding = P2p.Raw.encoding Message.cfg.encoding
  let chain_name = Message.cfg.chain_name
  let distributed_db_versions = Message.cfg.distributed_db_versions
end

module Make
    (Table : Distributed_db_functors.DISTRIBUTED_DB)
    (Kind : sig
       type t
       val proj: t -> Table.t
     end) = struct
  type key = Table.key
  type value = Table.value
  let known t k = Table.known (Kind.proj t) k
  type error += Missing_data = Table.Missing_data
  type error += Canceled = Table.Canceled
  type error += Timeout = Table.Timeout
  let read t k = Table.read (Kind.proj t) k
  let read_opt t k = Table.read_opt (Kind.proj t) k
  let prefetch t ?peer ?timeout k p =
    Table.prefetch (Kind.proj t) ?peer ?timeout k p
  let fetch t ?peer ?timeout k p =
    Table.fetch (Kind.proj t) ?peer ?timeout k p
  let clear_or_cancel t k = Table.clear_or_cancel (Kind.proj t) k
  let inject t k v = Table.inject (Kind.proj t) k v
  let pending t k = Table.pending (Kind.proj t) k
  let watch t = Table.watch (Kind.proj t)
  let resolve_pending t k v = Table.resolve_pending (Kind.proj t) k v
end

module Block_header = struct
  type t = Block_header.t
  include (Make (Raw_block_header.Table) (struct
             type t = chain_db
             let proj chain = chain.block_header_db.table
           end) : Distributed_db_functors.DISTRIBUTED_DB with type t := chain_db
                                                          and type key := Block_hash.t
                                                          and type value := Block_header.t
                                                          and type param := unit)
end

module Operation_hashes =
  Make (Raw_operation_hashes.Table) (struct
    type t = chain_db
    let proj chain = chain.operation_hashes_db.table
  end)

module Operations =
  Make (Raw_operations.Table) (struct
    type t = chain_db
    let proj chain = chain.operations_db.table
  end)

module Operation = struct
  include Operation
  include (Make (Raw_operation.Table) (struct
             type t = chain_db
             let proj chain = chain.operation_db.table
           end) : Distributed_db_functors.DISTRIBUTED_DB with type t := chain_db
                                                          and type key := Operation_hash.t
                                                          and type value := Operation.t
                                                          and type param := unit)
end

module Protocol = struct
  type t = Protocol.t
  include (Make (Raw_protocol.Table) (struct
             type t = db
             let proj db = db.protocol_db.table
           end) : Distributed_db_functors.DISTRIBUTED_DB with type t := db
                                                          and type key := Protocol_hash.t
                                                          and type value := Protocol.t
                                                          and type param := unit)
end


let broadcast chain_db msg =
  P2p_peer.Table.iter
    (fun _peer_id state ->
       ignore (P2p.try_send chain_db.global_db.p2p state.conn msg))
    chain_db.active_connections

let try_send chain_db peer_id msg =
  match P2p_peer.Table.find_opt chain_db.active_connections peer_id with
  | None -> ()
  | Some conn ->
      ignore (P2p.try_send chain_db.global_db.p2p conn.conn msg : bool)

let send chain_db ?peer msg =
  match peer with
  | Some peer -> try_send chain_db peer msg
  | None -> broadcast chain_db msg

module Request = struct

  let current_head chain_db ?peer () =
    let chain_id = State.Chain.id chain_db.chain_state in
    begin match peer with
      |Some peer ->
          let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
          Peer_metadata.incr meta (Sent_request Head)
      |None -> ()
    end ;
    send chain_db ?peer @@ Get_current_head chain_id

  let current_branch chain_db ?peer () =
    let chain_id = State.Chain.id chain_db.chain_state in
    begin match peer with
      |Some peer ->
          let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
          Peer_metadata.incr meta (Sent_request Head)
      |None -> ()
    end ;
    send chain_db ?peer @@ Get_current_branch chain_id

end

module Advertise = struct

  let current_head chain_db ?peer ?(mempool = Mempool.empty) head =
    let chain_id = State.Chain.id chain_db.chain_state in
    assert (Chain_id.equal chain_id (State.Block.chain_id head)) ;
    begin match peer with
      | Some peer ->
          let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
          Peer_metadata.incr meta (Sent_advertisement Head)
      | None -> ()
    end ;
    let msg_mempool =
      Message.Current_head (chain_id, State.Block.header head, mempool) in
    if mempool = Mempool.empty then
      send chain_db ?peer msg_mempool
    else
      let msg_disable_mempool =
        Message.Current_head (chain_id, State.Block.header head, Mempool.empty) in
      let send_mempool state =
        let { Connection_metadata.disable_mempool ; _ } =
          P2p.connection_remote_metadata chain_db.global_db.p2p state.conn in
        let msg = if disable_mempool then msg_disable_mempool else msg_mempool in
        ignore @@ P2p.try_send chain_db.global_db.p2p state.conn msg
      in
      match peer with
      | Some receiver_id ->
          let state = P2p_peer.Table.find chain_db.active_connections receiver_id in
          send_mempool state
      | None ->
          List.iter (fun (_receiver_id, state) -> send_mempool state)
            (P2p_peer.Table.fold (fun k v acc -> (k,v)::acc) chain_db.active_connections [])

  let current_branch ?peer chain_db =
    let chain_id = State.Chain.id chain_db.chain_state in
    let chain_state = chain_state chain_db in
    let sender_id = my_peer_id chain_db in
    begin match peer with
      | Some peer ->
          let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
          Peer_metadata.incr meta (Sent_advertisement Branch)
      | None -> ()
    end ;

    match peer with
    | Some receiver_id ->
        let seed = {
          Block_locator.receiver_id=receiver_id; sender_id } in
        (Chain.locator chain_state seed) >>= fun locator ->
        let msg = Message.Current_branch (chain_id, locator) in
        try_send chain_db receiver_id msg;
        Lwt.return_unit
    | None ->
        Lwt_list.iter_p
          (fun (receiver_id,state) ->
             let seed = {
               Block_locator.receiver_id=receiver_id; sender_id } in
             (Chain.locator chain_state seed) >>= fun locator ->
             let msg = Message.Current_branch (chain_id, locator) in
             ignore (P2p.try_send chain_db.global_db.p2p state.conn msg);
             Lwt.return_unit
          ) (P2p_peer.Table.fold (fun k v acc -> (k,v)::acc) chain_db.active_connections [])

end
