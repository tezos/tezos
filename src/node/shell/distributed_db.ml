(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

  type key = Hash.t
  type value = Disk_table.value
  type param = Disk_table.store

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
  type key = Operation_hash.t
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
  type key = Block_hash.t
  type value = Block_header.t
  let known = State.Block.known_valid
  let read net_state h =
    State.Block.read net_state h >>=? fun b ->
    return (State.Block.header b)
  let read_opt net_state h =
    State.Block.read_opt net_state h >>= fun b ->
    Lwt.return (Utils.map_option State.Block.header b)
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
  type key = Block_hash.t * int
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
    List.iter (fun i -> Table.clear table (hash, i)) (0 -- (n-1))

end

module Operations_storage = struct
  type store = State.Net.t
  type key = Block_hash.t * int
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
    List.iter (fun i -> Table.clear table (hash, i)) (0 -- (n-1))

end

module Protocol_storage = struct
  type store = State.t
  type key = Protocol_hash.t
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
  notify_branch: P2p.Peer_id.t -> Block_locator.t -> unit ;
  notify_head: P2p.Peer_id.t -> Block_hash.t -> Operation_hash.t list -> unit ;
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

let state { net_state } = net_state

module P2p_reader = struct

  type t = p2p_reader

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
        Lwt_list.exists_p
          (State.Block.known_invalid net_db.net_state)
          (locator :> Block_hash.t list) >>= fun known_invalid ->
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
        Chain.head net_db.net_state >>= fun head ->
        Chain.mempool net_db.net_state >>= fun mempool ->
        ignore
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_head (net_id, State.Block.hash head,
                         Utils.list_sub mempool 200) ;
        Lwt.return_unit

    | Current_head (net_id, head, mempool) ->
        may_handle state net_id @@ fun net_db ->
        State.Block.known_invalid net_db.net_state head >>= fun known_invalid ->
        if not known_invalid then
          net_db.callback.notify_head state.gid head mempool ;
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

    | Block_header block ->
        may_handle state block.shell.net_id @@ fun net_db ->
        let hash = Block_header.hash block in
        Raw_block_header.Table.notify
          net_db.block_header_db.table state.gid hash block >>= fun () ->
        Lwt.return_unit

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

    | Operation operation ->
        may_handle state operation.shell.net_id @@ fun net_db ->
        let hash = Operation.hash operation in
        Raw_operation.Table.notify
          net_db.operation_db.table state.gid hash operation >>= fun () ->
        Lwt.return_unit

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
        let found_hash, found_ofs =
          Operation_list_list_hash.check_path
            path (Operation_list_hash.compute ops) in
        if found_ofs <> ofs then
          Lwt.return_unit
        else
          Raw_block_header.Table.read_opt
            net_db.block_header_db.table block >>= function
          | None -> Lwt.return_unit
          | Some bh ->
              if Operation_list_list_hash.compare
                  found_hash bh.shell.operations_hash <> 0 then
                Lwt.return_unit
              else
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
        let found_hash, found_ofs =
          Operation_list_list_hash.check_path
            path (Operation_list_hash.compute (List.map Operation.hash ops)) in
        if found_ofs <> ofs then
          Lwt.return_unit
        else
          Raw_block_header.Table.read_opt
            net_db.block_header_db.table block >>= function
          | None -> Lwt.return_unit
          | Some bh ->
              if Operation_list_list_hash.compare
                   found_hash bh.shell.operations_hash <> 0 then
                Lwt.return_unit
              else
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
      Lwt_utils.worker "db_network_reader"
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

let read_all_operations net_db hash n =
  Lwt_list.map_p
    (fun i ->
       Raw_operations.Table.read_opt net_db.operations_db.table (hash, i))
    (0 -- (n-1)) >>= fun operations ->
  mapi_p
    (fun i ops ->
       match ops with
       | Some ops -> return ops
       | None ->
           Raw_operation_hashes.Table.read
             net_db.operation_hashes_db.table (hash, i) >>=? fun hashes ->
           map_p (Raw_operation.Table.read net_db.operation_db.table) hashes)
    operations

let commit_block net_db hash validation_result =
  Raw_block_header.Table.read
    net_db.block_header_db.table hash >>=? fun header ->
  read_all_operations net_db
    hash header.shell.validation_passes >>=? fun operations ->
  State.Block.store
    net_db.net_state header operations validation_result >>=? fun res ->
  Raw_block_header.Table.clear net_db.block_header_db.table hash ;
  Raw_operation_hashes.clear_all
    net_db.operation_hashes_db.table hash header.shell.validation_passes ;
  Raw_operations.clear_all
    net_db.operations_db.table hash header.shell.validation_passes ;
  (* TODO: proper handling of the operations table by the prevalidator. *)
  List.iter
    (List.iter
       (fun op -> Raw_operation.Table.clear
           net_db.operation_db.table
           (Operation.hash op)))
    operations ;
  return res

let commit_invalid_block net_db hash =
  Raw_block_header.Table.read
    net_db.block_header_db.table hash >>=? fun header ->
  State.Block.store_invalid net_db.net_state header >>=? fun res ->
  Raw_block_header.Table.clear net_db.block_header_db.table hash ;
  Raw_operation_hashes.clear_all
    net_db.operation_hashes_db.table hash header.shell.validation_passes ;
  Raw_operations.clear_all
    net_db.operations_db.table hash header.shell.validation_passes ;
  return res

let inject_operation net_db h op =
  fail_unless
    (Net_id.equal op.Operation.shell.net_id (State.Net.id net_db.net_state))
    (failure "Inconsitent net_id in operation") >>=? fun () ->
  Raw_operation.Table.inject net_db.operation_db.table h op >>= fun res ->
  return res

let inject_protocol db h p =
  Raw_protocol.Table.inject db.protocol_db.table h p

let commit_protocol db h =
  Raw_protocol.Table.read db.protocol_db.table h >>=? fun p ->
  State.Protocol.store db.disk p >>= fun res ->
  Raw_protocol.Table.clear db.protocol_db.table h ;
  return (res <> None)

type operation =
  | Blob of Operation.t
  | Hash of Operation_hash.t

let resolve_operation net_db = function
  | Blob op ->
      fail_unless
        (Net_id.equal op.shell.net_id (State.Net.id net_db.net_state))
        (failure "Inconsistent net_id in operation.") >>=? fun () ->
      return (Operation.hash op, op)
  | Hash oph ->
      Raw_operation.Table.read net_db.operation_db.table oph >>=? fun op ->
      return (oph, op)

let inject_block db bytes operations =
  let hash = Block_hash.hash_bytes [bytes] in
  match Block_header.of_bytes bytes with
  | None ->
      failwith "Cannot parse block header."
  | Some block ->
      match get_net db block.shell.net_id with
      | None ->
          failwith "Unknown network."
      | Some net_db ->
          map_p
            (map_p (resolve_operation net_db))
            operations >>=? fun operations ->
          let hashes = List.map (List.map fst) operations in
          let operations = List.map (List.map snd) operations in
          let computed_hash =
            Operation_list_list_hash.compute
              (List.map Operation_list_hash.compute hashes) in
          fail_when
            (Operation_list_list_hash.compare
               computed_hash block.shell.operations_hash <> 0)
            (Exn (Failure "Incoherent operation list")) >>=? fun () ->
          Raw_block_header.Table.inject
            net_db.block_header_db.table hash block >>= function
          | false ->
              failwith "Previously injected block."
          | true ->
              Raw_operation_hashes.inject_all
                net_db.operation_hashes_db.table hash hashes >>= fun _ ->
              Raw_operations.inject_all
                net_db.operations_db.table hash operations >>= fun _ ->
              return (hash, block)

let clear_block net_db hash n =
  Raw_operations.clear_all net_db.operations_db.table hash n ;
  Raw_operation_hashes.clear_all net_db.operation_hashes_db.table hash n ;
  Raw_block_header.Table.clear net_db.block_header_db.table hash

let broadcast_head net_db head mempool =
  let msg : Message.t =
    Current_head (State.Net.id net_db.net_state, head, mempool) in
  P2p.Peer_id.Table.iter
    (fun _peer_id state ->
       ignore (P2p.try_send net_db.global_db.p2p state.conn msg))
    net_db.active_connections

let watch_block_header { block_input } =
  Watcher.create_stream block_input
let watch_operation { operation_input } =
  Watcher.create_stream operation_input
let watch_protocol { protocol_db } =
  Raw_protocol.Table.watch protocol_db.table

module Raw = struct
  type 'a t =
    | Bootstrap
    | Advertise of P2p_types.Point.t list
    | Message of 'a
    | Disconnect
  let encoding = P2p.Raw.encoding Message.cfg.encoding
  let supported_versions = Message.cfg.versions
end

module type DISTRIBUTED_DB = sig
  type t
  type key
  type value
  type param
  val known: t -> key -> bool Lwt.t
  type error += Missing_data of key
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper
  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> value Lwt.t
  val clear: t -> key -> unit
end

module Make
    (Table : Distributed_db_functors.DISTRIBUTED_DB)
    (Kind : sig
       type t
       val proj: t -> Table.t
     end) = struct
  type t = Kind.t
  type key = Table.key
  type value = Table.value
  type param = Table.param
  let known t k = Table.known (Kind.proj t) k
  type error += Missing_data = Table.Missing_data
  let read t k = Table.read (Kind.proj t) k
  let read_opt t k = Table.read_opt (Kind.proj t) k
  let read_exn t k = Table.read_exn (Kind.proj t) k
  let prefetch t ?peer k p = Table.prefetch (Kind.proj t) ?peer k p
  let fetch t ?peer k p = Table.fetch (Kind.proj t) ?peer k p
  let clear t k = Table.clear (Kind.proj t) k
  let inject t k v = Table.inject (Kind.proj t) k v
  let watch t = Table.watch (Kind.proj t)
  let clear t k = Table.clear (Kind.proj t) k
end

module Block_header =
  Make (Raw_block_header.Table) (struct
    type t = net_db
    let proj net = net.block_header_db.table
  end)

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

module Operation =
  Make (Raw_operation.Table) (struct
    type t = net_db
    let proj net = net.operation_db.table
  end)

module Protocol =
  Make (Raw_protocol.Table) (struct
    type t = db
    let proj db = db.protocol_db.table
  end)

