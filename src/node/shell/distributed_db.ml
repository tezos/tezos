(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Message = Distributed_db_message
module Metadata = Distributed_db_metadata

type p2p = (Message.t, Metadata.t) P2p.net
type connection = (Message.t, Metadata.t) P2p.connection

type 'a request_param = {
  data: 'a ;
  active: unit -> P2p.Peer_id.Set.t ;
  send: P2p.Peer_id.t -> Message.t -> unit ;
}

module Make_raw
    (Hash : sig
       type t
       val name : string
       val encoding : t Data_encoding.t
       val pp : Format.formatter -> t -> unit
     end)
    (Disk_table :
       Distributed_db_functors.DISK_TABLE with type key := Hash.t)
    (Memory_table :
       Distributed_db_functors.MEMORY_TABLE with type key := Hash.t)
    (Request_message : sig
       type param
       val forge : param -> Hash.t list -> Message.t
     end)
    (Precheck : Distributed_db_functors.PRECHECK
     with type key := Hash.t
      and type value := Disk_table.value) = struct

  module Request = struct
    type param = Request_message.param request_param
    let active { active } = active ()
    let send { data ; send } gid keys =
      send gid (Request_message.forge data keys)
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

  let shutdown { scheduler } =
    Scheduler.shutdown scheduler

end

module Fake_operation_storage = struct
  type store = State.Net.t
  type value = Operation.t
  let known _ _ = Lwt.return_false
  let read _ _ = Lwt.return (Error_monad.error_exn Not_found)
  let read_opt _ _ = Lwt.return_none
  let read_exn _ _ = raise Not_found
end

module Raw_operation =
  Make_raw
    (Operation_hash)
    (Fake_operation_storage)
    (Operation_hash.Table)
    (struct
      type param = Net_id.t
      let forge net_id keys = Message.Get_operations (net_id, keys)
    end)
    (struct
      type param = unit
      type notified_value = Operation.t
      let precheck _ _ v = Some v
    end)

module Block_header_storage = struct
  type store = State.Net.t
  type value = Block_header.t
  let known = State.Block.known_valid
  let read net_state h =
    State.Block.read net_state h >>=? fun b ->
    return (State.Block.header b)
  let read_opt net_state h =
    State.Block.read_opt net_state h >>= fun b ->
    Lwt.return (Utils.map_option ~f:State.Block.header b)
  let read_exn net_state h =
    State.Block.read_exn net_state h >>= fun b ->
    Lwt.return (State.Block.header b)
end

module Raw_block_header =
  Make_raw
    (Block_hash)
    (Block_header_storage)
    (Block_hash.Table)
    (struct
      type param = Net_id.t
      let forge net_id keys = Message.Get_block_headers (net_id, keys)
    end)
    (struct
      type param = unit
      type notified_value = Block_header.t
      let precheck _ _ v = Some v
    end)

module Operation_hashes_storage = struct
  type store = State.Net.t
  type value = Operation_hash.t list
  let known net_state (h, _) = State.Block.known_valid net_state h
  let read net_state (h, i) =
    State.Block.read net_state h >>=? fun b ->
    State.Block.operation_hashes b i >>= fun (ops, _) ->
    return ops
  let read_opt net_state (h, i) =
    State.Block.read_opt net_state h >>= function
    | None -> Lwt.return_none
    | Some b ->
        State.Block.operation_hashes b i >>= fun (ops, _) ->
        Lwt.return (Some ops)
  let read_exn net_state (h, i) =
    State.Block.read_exn net_state h >>= fun b ->
    State.Block.operation_hashes b i >>= fun (ops, _) ->
    Lwt.return ops
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
      end)
      (Operation_hashes_storage)
      (Operations_table)
      (struct
        type param = Net_id.t
        let forge net_id keys =
          Message.Get_operation_hashes_for_blocks (net_id, keys)
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

  let inject_all table hash operations =
    Lwt_list.mapi_p
      (fun i ops -> Table.inject table (hash, i) ops)
      operations >>= Lwt_list.for_all_s (fun x -> Lwt.return x)

  let read_all table hash n =
    map_p (fun i -> Table.read table (hash, i)) (0 -- (n-1))

  let clear_all table hash n =
    List.iter (fun i -> Table.clear_or_cancel table (hash, i)) (0 -- (n-1))

end

module Operations_storage = struct
  type store = State.Net.t
  type value = Operation.t list
  let known net_state (h, _) = State.Block.known_valid net_state h
  let read net_state (h, i) =
    State.Block.read net_state h >>=? fun b ->
    State.Block.operations b i >>= fun (ops, _) ->
    return ops
  let read_opt net_state (h, i) =
    State.Block.read_opt net_state h >>= function
    | None -> Lwt.return_none
    | Some b ->
        State.Block.operations b i >>= fun (ops, _) ->
        Lwt.return (Some ops)
  let read_exn net_state (h, i) =
    State.Block.read_exn net_state h >>= fun b ->
    State.Block.operations b i >>= fun (ops, _) ->
    Lwt.return ops
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
      end)
      (Operations_storage)
      (Operations_table)
      (struct
        type param = Net_id.t
        let forge net_id keys =
          Message.Get_operations_for_blocks (net_id, keys)
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

  let inject_all table hash operations =
    Lwt_list.mapi_p
      (fun i ops -> Table.inject table (hash, i) ops)
      operations >>= Lwt_list.for_all_s (fun x -> Lwt.return x)

  let read_all table hash n =
    map_p (fun i -> Table.read table (hash, i)) (0 -- (n-1))

  let clear_all table hash n =
    List.iter (fun i -> Table.clear_or_cancel table (hash, i)) (0 -- (n-1))

end

module Protocol_storage = struct
  type store = State.t
  type value = Protocol.t
  let known = State.Protocol.known
  let read = State.Protocol.read
  let read_opt = State.Protocol.read_opt
  let read_exn = State.Protocol.read_exn
end

module Raw_protocol =
  Make_raw
    (Protocol_hash)
    (Protocol_storage)
    (Protocol_hash.Table)
    (struct
      type param = unit
      let forge () keys = Message.Get_protocols keys
    end)
    (struct
      type param = unit
      type notified_value = Protocol.t
      let precheck _ _ v = Some v
    end)

type callback = {
  notify_branch:
    P2p.Peer_id.t -> Block_locator.t -> unit ;
  notify_head:
    P2p.Peer_id.t -> Block_header.t -> Mempool.t -> unit ;
  disconnection: P2p.Peer_id.t -> unit ;
}

type db = {
  p2p: p2p ;
  p2p_readers: p2p_reader P2p.Peer_id.Table.t ;
  disk: State.t ;
  active_nets: net_db Net_id.Table.t ;
  protocol_db: Raw_protocol.t ;
  block_input: (Block_hash.t * Block_header.t) Watcher.input ;
  operation_input: (Operation_hash.t * Operation.t) Watcher.input ;
}

and net_db = {
  net_state: State.Net.t ;
  global_db: db ;
  operation_db: Raw_operation.t ;
  block_header_db: Raw_block_header.t ;
  operation_hashes_db: Raw_operation_hashes.t ;
  operations_db: Raw_operations.t ;
  mutable callback: callback ;
  active_peers: P2p.Peer_id.Set.t ref ;
  active_connections: p2p_reader P2p.Peer_id.Table.t ;
}

and p2p_reader = {
  gid: P2p.Peer_id.t ;
  conn: connection ;
  peer_active_nets: net_db Net_id.Table.t ;
  canceler: Lwt_utils.Canceler.t ;
  mutable worker: unit Lwt.t ;
}

let noop_callback = {
  notify_branch = begin fun _gid _locator -> () end ;
  notify_head =  begin fun _gid _block _ops -> () end ;
  disconnection = begin fun _gid -> () end ;
}

type t = db

let state { disk } = disk
let net_state { net_state } = net_state
let db { global_db } = global_db

let find_pending_block_header active_nets h =
  Net_id.Table.fold
    (fun _net_id net_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_block_header.Table.pending
             net_db.block_header_db.table h ->
           Some net_db
       | None -> None)
    active_nets
    None

let find_pending_operation active_nets h =
  Net_id.Table.fold
    (fun _net_id net_db acc ->
       match acc with
       | Some _ -> acc
       | None when Raw_operation.Table.pending
             net_db.operation_db.table h ->
           Some net_db
       | None -> None)
    active_nets
    None

module P2p_reader = struct

  let may_activate global_db state net_id f =
    match Net_id.Table.find state.peer_active_nets net_id with
    | net_db ->
        f net_db
    | exception Not_found ->
        match Net_id.Table.find global_db.active_nets net_id with
        | net_db ->
            net_db.active_peers :=
              P2p.Peer_id.Set.add state.gid !(net_db.active_peers) ;
            P2p.Peer_id.Table.add net_db.active_connections
              state.gid state ;
            Net_id.Table.add state.peer_active_nets net_id net_db ;
            f net_db
        | exception Not_found ->
            (* TODO  decrease peer score. *)
            Lwt.return_unit

  let deactivate state net_db =
    net_db.callback.disconnection state.gid ;
    net_db.active_peers :=
      P2p.Peer_id.Set.remove state.gid !(net_db.active_peers) ;
    P2p.Peer_id.Table.remove net_db.active_connections state.gid

  let may_handle state net_id f =
    match Net_id.Table.find state.peer_active_nets net_id with
    | exception Not_found ->
        (* TODO decrease peer score *)
        Lwt.return_unit
    | net_db ->
        f net_db

  let may_handle_global global_db net_id f =
    match Net_id.Table.find global_db.active_nets net_id with
    | exception Not_found ->
        Lwt.return_unit
    | net_db ->
        f net_db

  let handle_msg global_db state msg =

    let open Message in
    let module Logging =
      Logging.Make(struct let name = "node.distributed_db.p2p_reader" end) in
    let open Logging in

    lwt_debug "Read message from %a: %a"
      P2p.Peer_id.pp_short state.gid Message.pp_json msg >>= fun () ->

    match msg with

    | Get_current_branch net_id ->
        may_handle_global global_db net_id @@ fun net_db ->
        if not (Net_id.Table.mem state.peer_active_nets net_id) then
          ignore
          @@ P2p.try_send global_db.p2p state.conn
          @@ Get_current_branch net_id ;
        Chain.head net_db.net_state >>= fun head ->
        Block_locator.compute head 200 >>= fun locator ->
        ignore
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_branch (net_id, locator) ;
        Lwt.return_unit

    | Current_branch (net_id, locator) ->
        may_activate global_db state net_id @@ fun net_db ->
        let head, hist = (locator :> Block_header.t * Block_hash.t list) in
        Lwt_list.exists_p
          (State.Block.known_invalid net_db.net_state)
          (Block_header.hash head :: hist) >>= fun known_invalid ->
        if not known_invalid then
          net_db.callback.notify_branch state.gid locator ;
        (* TODO Kickban *)
        Lwt.return_unit

    | Deactivate net_id ->
        may_handle state net_id @@ fun net_db ->
        deactivate state net_db ;
        Net_id.Table.remove state.peer_active_nets net_id ;
        Lwt.return_unit

    | Get_current_head net_id ->
        may_handle state net_id @@ fun net_db ->
        Mempool.get net_db.net_state >>= fun (head, mempool) ->
        (* TODO bound the sent mempool size *)
        ignore
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_head (net_id, head, mempool) ;
        Lwt.return_unit

    | Current_head (net_id, header, mempool) ->
        may_handle state net_id @@ fun net_db ->
        let head = Block_header.hash header in
        State.Block.known_invalid net_db.net_state head >>= fun known_invalid ->
        if not known_invalid then
          net_db.callback.notify_head state.gid header mempool ;
        (* TODO Kickban *)
        Lwt.return_unit

    | Get_block_headers (net_id, hashes) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO: Blame request of unadvertised blocks ? *)
        Lwt_list.iter_p
          (fun hash ->
             State.Block.read_opt net_db.net_state hash >|= function
             | None -> ()
             | Some b ->
                 let header = State.Block.header b in
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Block_header header))
          hashes

    | Block_header block -> begin
        let hash = Block_header.hash block in
        match find_pending_block_header state.peer_active_nets hash with
        | None ->
            (* TODO some penalty. *)
            Lwt.return_unit
        | Some net_db ->
            Raw_block_header.Table.notify
              net_db.block_header_db.table state.gid hash block >>= fun () ->
            Lwt.return_unit
      end

    | Get_operations (net_id, hashes) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO: only answers for prevalidated operations *)
        Lwt_list.iter_p
          (fun hash ->
             Raw_operation.Table.read_opt
               net_db.operation_db.table hash >|= function
             | None -> ()
             | Some p ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Operation p))
          hashes

    | Operation operation -> begin
        let hash = Operation.hash operation in
        match find_pending_operation state.peer_active_nets hash with
        | None ->
            (* TODO some penalty. *)
            Lwt.return_unit
        | Some net_db ->
            Raw_operation.Table.notify
              net_db.operation_db.table state.gid hash operation >>= fun () ->
            Lwt.return_unit
      end

    | Get_protocols hashes ->
        Lwt_list.iter_p
          (fun hash ->
             State.Protocol.read_opt global_db.disk hash >|= function
             | None -> ()
             | Some p ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Protocol p))
          hashes

    | Protocol protocol ->
        let hash = Protocol.hash protocol in
        Raw_protocol.Table.notify
          global_db.protocol_db.table state.gid hash protocol >>= fun () ->
        Lwt.return_unit

    | Get_operation_hashes_for_blocks (net_id, blocks) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO: Blame request of unadvertised blocks ? *)
        Lwt_list.iter_p
          (fun (hash, ofs) ->
             State.Block.read_opt net_db.net_state hash >>= function
             | None -> Lwt.return_unit
             | Some b ->
                 State.Block.operation_hashes b ofs >>= fun (hashes, path) ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn
                   (Operation_hashes_for_block
                      (net_id, hash, ofs, hashes, path)) ;
                 Lwt.return_unit)
          blocks

    | Operation_hashes_for_block (net_id, block, ofs, ops, path) -> begin
        may_handle state net_id @@ fun net_db ->
        (* TODO early detection of non-requested list. *)
        Raw_operation_hashes.Table.notify
          net_db.operation_hashes_db.table state.gid
          (block, ofs) (ops, path) >>= fun () ->
        Lwt.return_unit
      end

    | Get_operations_for_blocks (net_id, blocks) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO: Blame request of unadvertised blocks ? *)
        Lwt_list.iter_p
          (fun (hash, ofs) ->
             State.Block.read_opt net_db.net_state hash >>= function
             | None -> Lwt.return_unit
             | Some b ->
                 State.Block.operations b ofs >>= fun (hashes, path) ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn
                   (Operations_for_block
                      (net_id, hash, ofs, hashes, path)) ;
                 Lwt.return_unit)
          blocks

    | Operations_for_block (net_id, block, ofs, ops, path) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO early detection of non-requested operations. *)
        Raw_operations.Table.notify
          net_db.operations_db.table state.gid
          (block, ofs) (ops, path) >>= fun () ->
        Lwt.return_unit

  let rec worker_loop global_db state =
    Lwt_utils.protect ~canceler:state.canceler begin fun () ->
      P2p.recv global_db.p2p state.conn
    end >>= function
    | Ok msg ->
        handle_msg global_db state msg >>= fun () ->
        worker_loop global_db state
    | Error _ ->
        Net_id.Table.iter
          (fun _ -> deactivate state)
          state.peer_active_nets ;
        P2p.Peer_id.Table.remove global_db.p2p_readers state.gid ;
        Lwt.return_unit

  let run db gid conn =
    let canceler = Lwt_utils.Canceler.create () in
    let state = {
      conn ; gid ; canceler ;
      peer_active_nets = Net_id.Table.create 17 ;
      worker = Lwt.return_unit ;
    } in
    Net_id.Table.iter (fun net_id _net_db ->
        Lwt.async begin fun () ->
          P2p.send db.p2p conn (Get_current_branch net_id)
        end)
      db.active_nets ;
    state.worker <-
      Lwt_utils.worker
        (Format.asprintf "db_network_reader.%a"
           P2p.Peer_id.pp_short gid)
        ~run:(fun () -> worker_loop db state)
        ~cancel:(fun () -> Lwt_utils.Canceler.cancel canceler) ;
    P2p.Peer_id.Table.add db.p2p_readers gid state

  let shutdown s =
    Lwt_utils.Canceler.cancel s.canceler >>= fun () ->
    s.worker

end

let active_peer_ids p2p () =
  List.fold_left
    (fun acc conn ->
       let { P2p.Connection_info.peer_id } = P2p.connection_info p2p conn in
       P2p.Peer_id.Set.add peer_id acc)
    P2p.Peer_id.Set.empty
    (P2p.connections p2p)

let raw_try_send p2p peer_id msg =
  match P2p.find_connection p2p peer_id with
  | None -> ()
  | Some conn -> ignore (P2p.try_send p2p conn msg : bool)

let create disk p2p =
  let global_request =
    { data = () ;
      active = active_peer_ids p2p ;
      send = raw_try_send p2p ;
    } in
  let protocol_db = Raw_protocol.create global_request disk in
  let active_nets = Net_id.Table.create 17 in
  let p2p_readers = P2p.Peer_id.Table.create 17 in
  let block_input = Watcher.create_input () in
  let operation_input = Watcher.create_input () in
  let db =
    { p2p ; p2p_readers ; disk ;
      active_nets ; protocol_db ;
      block_input ; operation_input } in
  P2p.on_new_connection p2p (P2p_reader.run db) ;
  P2p.iter_connections p2p (P2p_reader.run db) ;
  db

let activate ({ p2p ; active_nets } as global_db) net_state =
  let net_id = State.Net.id net_state in
  match Net_id.Table.find active_nets net_id with
  | exception Not_found ->
      let active_peers = ref P2p.Peer_id.Set.empty in
      let p2p_request =
        { data = net_id ;
          active = (fun () -> !active_peers) ;
          send = raw_try_send p2p ;
        } in
      let operation_db =
        Raw_operation.create
          ~global_input:global_db.operation_input p2p_request net_state in
      let block_header_db =
        Raw_block_header.create
          ~global_input:global_db.block_input p2p_request net_state in
      let operation_hashes_db =
        Raw_operation_hashes.create p2p_request net_state in
      let operations_db =
        Raw_operations.create p2p_request net_state in
      let net = {
        global_db ; operation_db ; block_header_db ;
        operation_hashes_db ; operations_db ;
        net_state ; callback = noop_callback ; active_peers ;
        active_connections = P2p.Peer_id.Table.create 53 ;
      } in
      P2p.iter_connections p2p (fun _peer_id conn ->
          Lwt.async begin fun () ->
            P2p.send p2p conn (Get_current_branch net_id)
          end) ;
      Net_id.Table.add active_nets net_id net ;
      net
  | net ->
      net

let set_callback net_db callback =
  net_db.callback <- callback

let deactivate net_db =
  let { active_nets ; p2p } = net_db.global_db in
  let net_id = State.Net.id net_db.net_state in
  Net_id.Table.remove active_nets net_id ;
  P2p.Peer_id.Table.iter
    (fun _peer_id reader ->
       P2p_reader.deactivate reader net_db  ;
       Lwt.async begin fun () ->
         P2p.send p2p reader.conn (Deactivate net_id)
       end)
    net_db.active_connections ;
  Raw_operation.shutdown net_db.operation_db >>= fun () ->
  Raw_block_header.shutdown net_db.block_header_db >>= fun () ->
  Lwt.return_unit >>= fun () ->
  Lwt.return_unit

let get_net { active_nets } net_id =
  try Some (Net_id.Table.find active_nets net_id)
  with Not_found -> None

let disconnect { global_db = { p2p } } peer_id =
  match P2p.find_connection p2p peer_id with
  | None -> Lwt.return_unit
  | Some conn -> P2p.disconnect p2p conn

let shutdown { p2p ; p2p_readers ; active_nets } =
  P2p.Peer_id.Table.fold
    (fun _peer_id reader acc ->
       P2p_reader.shutdown reader >>= fun () -> acc)
    p2p_readers
    Lwt.return_unit >>= fun () ->
  Net_id.Table.fold
    (fun _ net_db acc ->
       Raw_operation.shutdown net_db.operation_db >>= fun () ->
       Raw_block_header.shutdown net_db.block_header_db >>= fun () ->
       acc)
    active_nets
    Lwt.return_unit >>= fun () ->
  P2p.shutdown p2p >>= fun () ->
  Lwt.return_unit

let clear_block net_db hash n =
  Raw_operations.clear_all net_db.operations_db.table hash n ;
  Raw_operation_hashes.clear_all net_db.operation_hashes_db.table hash n ;
  Raw_block_header.Table.clear_or_cancel net_db.block_header_db.table hash

let commit_block net_db hash header operations result =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  assert (List.length operations = header.shell.validation_passes) ;
  State.Block.store net_db.net_state header operations result >>=? fun res ->
  clear_block net_db hash header.shell.validation_passes ;
  return res

let commit_invalid_block net_db hash header _err =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  State.Block.store_invalid net_db.net_state header >>=? fun res ->
  clear_block net_db hash header.shell.validation_passes ;
  return res

let inject_operation net_db h op =
  assert (Operation_hash.equal h (Operation.hash op)) ;
  Raw_operation.Table.inject net_db.operation_db.table h op >>= fun res ->
  return res

let commit_protocol db h p =
  State.Protocol.store db.disk p >>= fun res ->
  Raw_protocol.Table.clear_or_cancel db.protocol_db.table h ;
  return (res <> None)

let watch_block_header { block_input } =
  Watcher.create_stream block_input
let watch_operation { operation_input } =
  Watcher.create_stream operation_input

module Raw = struct
  let encoding = P2p.Raw.encoding Message.cfg.encoding
  let supported_versions = Message.cfg.versions
end

module type DISTRIBUTED_DB = sig
  type t
  type key
  type value
  val known: t -> key -> bool Lwt.t
  type error += Missing_data of key
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  type param
  type error += Timeout of key
  val fetch:
    t ->
    ?peer:P2p.Peer_id.t ->
    ?timeout:float ->
    key -> param -> value tzresult Lwt.t
  val prefetch:
    t ->
    ?peer:P2p.Peer_id.t ->
    ?timeout:float ->
    key -> param -> unit
  type error += Canceled of key
  val clear_or_cancel: t -> key -> unit
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper
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
  let read_exn t k = Table.read_exn (Kind.proj t) k
  let prefetch t ?peer ?timeout k p =
    Table.prefetch (Kind.proj t) ?peer ?timeout k p
  let fetch t ?peer ?timeout k p =
    Table.fetch (Kind.proj t) ?peer ?timeout k p
  let clear_or_cancel t k = Table.clear_or_cancel (Kind.proj t) k
  let inject t k v = Table.inject (Kind.proj t) k v
  let watch t = Table.watch (Kind.proj t)
end

module Block_header = struct
  type t = Block_header.t
  include (Make (Raw_block_header.Table) (struct
             type t = net_db
             let proj net = net.block_header_db.table
           end) : DISTRIBUTED_DB with type t := net_db
                                  and type key := Block_hash.t
                                  and type value := Block_header.t
                                  and type param := unit)
end

let read_block_header { disk ; active_nets } h =
  State.read_block disk h >>= function
  | Some b ->
      Lwt.return_some (State.Block.net_id b, State.Block.header b)
  | None ->
      Net_id.Table.fold
        (fun net_id net_db acc ->
           acc >>= function
           | Some _ -> acc
           | None ->
               Block_header.read_opt net_db h >>= function
               | None -> Lwt.return_none
               | Some bh -> Lwt.return_some (net_id, bh))
        active_nets
        Lwt.return_none

module Operation_hashes =
  Make (Raw_operation_hashes.Table) (struct
    type t = net_db
    let proj net = net.operation_hashes_db.table
  end)

module Operations =
  Make (Raw_operations.Table) (struct
    type t = net_db
    let proj net = net.operations_db.table
  end)

module Operation = struct
  include Operation
  include (Make (Raw_operation.Table) (struct
             type t = net_db
             let proj net = net.operation_db.table
           end) : DISTRIBUTED_DB with type t := net_db
                                  and type key := Operation_hash.t
                                  and type value := Operation.t
                                  and type param := unit)
end

module Protocol = struct
  type t = Protocol.t
  include (Make (Raw_protocol.Table) (struct
             type t = db
             let proj db = db.protocol_db.table
           end) : DISTRIBUTED_DB with type t := db
                                  and type key := Protocol_hash.t
                                  and type value := Protocol.t
                                  and type param := unit)
end


let broadcast net_db msg =
  P2p.Peer_id.Table.iter
    (fun _peer_id state ->
       ignore (P2p.try_send net_db.global_db.p2p state.conn msg))
    net_db.active_connections

let try_send net_db peer_id msg =
  try
    let conn = P2p.Peer_id.Table.find net_db.active_connections peer_id in
    ignore (P2p.try_send net_db.global_db.p2p conn.conn msg : bool)
  with Not_found -> ()

let send net_db ?peer msg =
  match peer with
  | Some peer -> try_send net_db peer msg
  | None -> broadcast net_db msg

module Request = struct

  let current_head net_db ?peer () =
    let net_id = State.Net.id net_db.net_state in
    send net_db ?peer @@ Get_current_head net_id

  let current_branch net_db ?peer () =
    let net_id = State.Net.id net_db.net_state in
    send net_db ?peer @@ Get_current_branch net_id

end

module Advertise = struct

  let current_head net_db ?peer ?(mempool = Mempool.empty) head =
    let net_id = State.Net.id net_db.net_state in
    assert (Net_id.equal net_id (State.Block.net_id head)) ;
    send net_db ?peer @@
    Current_head (net_id, State.Block.header head, mempool)

  let current_branch net_db ?peer head =
    let net_id = State.Net.id net_db.net_state in
    assert (Net_id.equal net_id (State.Block.net_id head)) ;
    Block_locator.compute head 200 >>= fun locator ->
    send net_db ?peer @@ Current_branch (net_id, locator) ;
    Lwt.return_unit

end

