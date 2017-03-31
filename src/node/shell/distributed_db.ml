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
    (Hash : sig type t end)
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

module No_precheck = struct
  type param = unit
  let precheck _ _ _ = true
end

module Raw_operation =
  Make_raw
    (Operation_hash)
    (State.Operation)
    (Operation_hash.Table)
    (struct
      type param = Net_id.t
      let forge net_id keys = Message.Get_operations (net_id, keys)
    end)
    (No_precheck)

module Raw_block_header =
  Make_raw
    (Block_hash)
    (State.Block_header)
    (Block_hash.Table)
    (struct
      type param = Net_id.t
      let forge net_id keys = Message.Get_block_headers (net_id, keys)
    end)
    (No_precheck)

module Operation_list_table =
  Hashtbl.Make(struct
    type t = Block_hash.t * int
    let hash = Hashtbl.hash
    let equal (b1, i1) (b2, i2) =
      Block_hash.equal b1 b2 && i1 = i2
  end)

module Raw_operation_list =
  Make_raw
    (struct type t = Block_hash.t * int end)
    (State.Operation_list)
    (Operation_list_table)
    (struct
      type param = Net_id.t
      let forge net_id keys =
        Message.Get_operation_list (net_id, keys)
    end)
    (struct
      type param = Operation_list_list_hash.t
      let precheck (_block, expected_ofs) expected_hash (ops, path) =
        let received_hash, received_ofs =
          Operation_list_list_hash.check_path path
            (Operation_list_hash.compute ops) in
        received_ofs = expected_ofs &&
        Operation_list_list_hash.compare expected_hash received_hash = 0
    end)

module Raw_protocol =
  Make_raw
    (Protocol_hash)
    (State.Protocol)
    (Protocol_hash.Table)
    (struct
      type param = unit
      let forge () keys = Message.Get_protocols keys
    end)
    (No_precheck)

type callback = {
  notify_branch: P2p.Peer_id.t -> Block_hash.t list -> unit ;
  current_branch: int -> Block_hash.t list Lwt.t ;
  notify_head: P2p.Peer_id.t -> Block_hash.t -> Operation_hash.t list -> unit ;
  current_head: int -> (Block_hash.t * Operation_hash.t list) Lwt.t ;
  disconnection: P2p.Peer_id.t -> unit ;
}

type db = {
  p2p: p2p ;
  p2p_readers: p2p_reader P2p.Peer_id.Table.t ;
  disk: State.t ;
  active_nets: net Net_id.Table.t ;
  protocol_db: Raw_protocol.t ;
  block_input: (Block_hash.t * Store.Block_header.t) Watcher.input ;
  operation_input: (Operation_hash.t * Store.Operation.t) Watcher.input ;
}

and net = {
  net: State.Net.t ;
  global_db: db ;
  operation_db: Raw_operation.t ;
  block_header_db: Raw_block_header.t ;
  operation_list_db: Raw_operation_list.t ;
  callback: callback ;
  active_peers: P2p.Peer_id.Set.t ref ;
  active_connections: p2p_reader P2p.Peer_id.Table.t ;
}

and p2p_reader = {
  gid: P2p.Peer_id.t ;
  conn: connection ;
  peer_active_nets: net Net_id.Table.t ;
  canceler: Lwt_utils.Canceler.t ;
  mutable worker: unit Lwt.t ;
}

type t = db

let state { net } = net

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
    let open Logging.Node.Worker in

    lwt_debug "Read message from %a: %a"
      P2p.Peer_id.pp_short state.gid Message.pp_json msg >>= fun () ->

    match msg with

    | Get_current_branch net_id ->
        may_handle_global global_db net_id @@ fun net_db ->
        if not (Net_id.Table.mem state.peer_active_nets net_id) then
          ignore
          @@ P2p.try_send global_db.p2p state.conn
          @@ Get_current_branch net_id ;
        net_db.callback.current_branch 200 >>= fun locator ->
        ignore
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_branch (net_id, locator) ;
        Lwt.return_unit

    | Current_branch (net_id, locator) ->
        may_activate global_db state net_id @@ fun net_db ->
        net_db.callback.notify_branch state.gid locator ;
        Lwt.return_unit

    | Deactivate net_id ->
        may_handle state net_id @@ fun net_db ->
        deactivate state net_db ;
        Net_id.Table.remove state.peer_active_nets net_id ;
        Lwt.return_unit

    | Get_current_head net_id ->
        may_handle state net_id @@ fun net_db ->
        net_db.callback.current_head 200 >>= fun (head, mempool) ->
        ignore
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_head (net_id, head, mempool) ;
        Lwt.return_unit

    | Current_head (net_id, head, mempool) ->
        may_handle state net_id @@ fun net_db ->
        net_db.callback.notify_head state.gid head mempool ;
        Lwt.return_unit

    | Get_block_headers (net_id, hashes) ->
        may_handle state net_id @@ fun net_db ->
        (* Should we filter out invalid block ? *)
        (* Should we filter out blocks whose validity is unknown ? *)
        (* Should we blame request of unadvertised blocks ? *)
        Lwt_list.iter_p
          (fun hash ->
             Raw_block_header.Table.read
               net_db.block_header_db.table hash >|= function
             | None -> ()
             | Some p ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Block_header p))
          hashes

    | Block_header block ->
        may_handle state block.shell.net_id @@ fun net_db ->
        let hash = Store.Block_header.hash block in
        Raw_block_header.Table.notify
          net_db.block_header_db.table state.gid hash block >>= fun () ->
        Lwt.return_unit

    | Get_operations (net_id, hashes) ->
        may_handle state net_id @@ fun net_db ->
        Lwt_list.iter_p
          (fun hash ->
             Raw_operation.Table.read
               net_db.operation_db.table hash >|= function
             | None -> ()
             | Some p ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Operation p))
          hashes

    | Operation operation ->
        may_handle state operation.shell.net_id @@ fun net_db ->
        let hash = Store.Operation.hash operation in
        Raw_operation.Table.notify
          net_db.operation_db.table state.gid hash operation >>= fun () ->
        Lwt.return_unit

    | Get_protocols hashes ->
        Lwt_list.iter_p
          (fun hash ->
             Raw_protocol.Table.read
               global_db.protocol_db.table hash >|= function
             | None -> ()
             | Some p ->
                 ignore @@
                 P2p.try_send global_db.p2p state.conn (Protocol p))
          hashes

    | Protocol protocol ->
        let hash = Store.Protocol.hash protocol in
        Raw_protocol.Table.notify
          global_db.protocol_db.table state.gid hash protocol >>= fun () ->
        Lwt.return_unit

    | Get_operation_list (net_id, hashes) ->
        may_handle state net_id @@ fun net_db ->
        Lwt_list.iter_p
          (fun (block, ofs as key) ->
             Raw_operation_list.Table.read
               net_db.operation_list_db.table key >>= function
             | None -> Lwt.return_unit
             | Some (ops, path) ->
                 ignore @@
                 P2p.try_send
                   global_db.p2p state.conn
                   (Operation_list (net_id, block, ofs, ops, path)) ;
                 Lwt.return_unit)
          hashes

    | Operation_list (net_id, block, ofs, ops, path) ->
        may_handle state net_id @@ fun net_db ->
        (* TODO early detection of non-requested list. *)
        let found_hash, found_ofs =
          Operation_list_list_hash.check_path
            path (Operation_list_hash.compute ops) in
        if found_ofs <> ofs then
          Lwt.return_unit
        else
          Raw_block_header.Table.read
            net_db.block_header_db.table block >>= function
          | None -> Lwt.return_unit
          | Some bh ->
              if Operation_list_list_hash.compare
                   found_hash bh.shell.operations <> 0 then
                Lwt.return_unit
              else
                Raw_operation_list.Table.notify
                  net_db.operation_list_db.table state.gid
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

let activate ~callback ({ p2p ; active_nets } as global_db) net =
  let net_id = State.Net.id net in
  match Net_id.Table.find active_nets net_id with
  | exception Not_found ->
      let active_peers = ref P2p.Peer_id.Set.empty in
      let p2p_request =
        let net_id = State.Net.id net in
        { data = net_id ;
          active = (fun () -> !active_peers) ;
          send = raw_try_send p2p ;
        } in
      let operation_db =
        Raw_operation.create
          ~global_input:global_db.operation_input p2p_request net in
      let block_header_db =
        Raw_block_header.create
          ~global_input:global_db.block_input p2p_request net in
      let operation_list_db =
        Raw_operation_list.create p2p_request net in
      let net = {
        global_db ; operation_db ; block_header_db ; operation_list_db ;
        net ; callback ; active_peers ;
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

let deactivate net =
  let { active_nets ; p2p } = net.global_db in
  let net_id = State.Net.id net.net in
  Net_id.Table.remove active_nets net_id ;
  P2p.Peer_id.Table.iter
    (fun _peer_id reader ->
       P2p_reader.deactivate reader net  ;
       Lwt.async begin fun () ->
         P2p.send p2p reader.conn (Deactivate net_id)
       end)
    net.active_connections ;
  Raw_operation.shutdown net.operation_db >>= fun () ->
  Raw_block_header.shutdown net.block_header_db >>= fun () ->
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

module type PARAMETRIZED_DISTRIBUTED_DB =
  Distributed_db_functors.PARAMETRIZED_DISTRIBUTED_DB
module type DISTRIBUTED_DB =
  Distributed_db_functors.DISTRIBUTED_DB

module Make
    (Table : PARAMETRIZED_DISTRIBUTED_DB with type param := unit)
    (Kind : sig
       type t
       val proj: t -> Table.t
     end) = struct
  type t = Kind.t
  type key = Table.key
  type value = Table.value
  let known t k = Table.known (Kind.proj t) k
  let read t k = Table.read (Kind.proj t) k
  let read_exn t k = Table.read_exn (Kind.proj t) k
  let prefetch t ?peer k = Table.prefetch (Kind.proj t) ?peer k ()
  let fetch t ?peer k = Table.fetch (Kind.proj t) ?peer k ()
  let commit t k = Table.commit (Kind.proj t) k
  let inject t k v = Table.inject (Kind.proj t) k v
  let watch t = Table.watch (Kind.proj t)
end

module Operation =
  Make (Raw_operation.Table) (struct
    type t = net
    let proj net = net.operation_db.table
  end)

module Block_header =
  Make (Raw_block_header.Table) (struct
    type t = net
    let proj net = net.block_header_db.table
  end)

module Protocol =
  Make (Raw_protocol.Table) (struct
    type t = db
    let proj db = db.protocol_db.table
  end)

module Operation_list = struct

  type t = net
  type key = Block_hash.t * int
  type value = Operation_hash.t list
  type param = Operation_list_list_hash.t

  let proj net = net.operation_list_db.table

  module Table = Raw_operation_list.Table

  let known t k = Table.known (proj t) k
  let read t k =
    Table.read (proj t) k >>= function
    | None -> Lwt.return_none
    | Some (op, _) -> Lwt.return (Some op)
  let read_exn t k = Table.read_exn (proj t) k >|= fst
  let prefetch t ?peer k p = Table.prefetch (proj t) ?peer k p
  let fetch t ?peer k p = Table.fetch (proj t) ?peer k p >|= fst

  let rec do_read net block acc i =
    if i <= 0 then
      Lwt.return []
    else
      read_exn net (block, i-1) >>= fun ops ->
      do_read net block (ops :: acc) (i-1)

  let read_all_opt net block =
    State.Operation_list.read_count_opt
      net.net block >>= function
    | None -> Lwt.return_none
    | Some len -> do_read net block [] len >>= fun ops -> Lwt.return (Some ops)

  let read_all_exn net block =
    State.Operation_list.read_count_exn
      net.net block >>= fun len ->
    do_read net block [] len

  let rec do_commit net block i =
    if i <= 0 then
      Lwt.return_unit
    else
      Raw_operation_list.Table.commit
        net.operation_list_db.table (block, i-1) >>= fun () ->
      do_commit net block (i-1)

  let commit_all net block len =
    State.Operation_list.store_count net.net block len >>= fun () ->
    do_commit net block len

  let inject_all net block opss =
    State.Operation_list.read_count_opt net.net block >>= function
    | Some _ -> Lwt.return_false
    | None ->
        let hashes = List.map Operation_list_hash.compute opss in
        Lwt_list.mapi_p
          (fun i ops ->
             let path = Operation_list_list_hash.compute_path hashes i in
             Raw_operation_list.Table.inject
               net.operation_list_db.table
               (block, i) (ops, path))
          opss >>= fun injected ->
        Lwt.return (List.for_all (fun x -> x) injected)

end

let inject_block t bytes operations =
  let hash = Block_hash.hash_bytes [bytes] in
  match
    Data_encoding.Binary.of_bytes Store.Block_header.encoding bytes
  with
  | None ->
      failwith "Cannot parse block header."
  | Some block ->
      match get_net t block.shell.net_id with
      | None ->
          failwith "Unknown network."
      | Some net_db ->
          Block_header.known net_db hash >>= function
          | true ->
              failwith "Previously injected block."
          | false ->
              let computed_hash =
                Operation_list_list_hash.compute
                  (List.map Operation_list_hash.compute operations) in
              fail_unless
                (Operation_list_list_hash.compare
                   computed_hash block.shell.operations = 0)
                (Exn (Failure "Incoherent operation list")) >>=? fun () ->
              Raw_block_header.Table.inject
                net_db.block_header_db.table hash block >>= function
              | false ->
                  failwith "Previously injected block."
              | true ->
                  Operation_list.inject_all
                    net_db hash operations >>= fun _ ->
                  return (hash, block)

(*
let inject_operation t bytes =
  let hash = Operation_hash.hash_bytes [bytes] in
  match Data_encoding.Binary.of_bytes Store.Operation.encoding bytes with
  | None ->
      failwith "Cannot parse operations."
  | Some op ->
      match get_net t op.shell.net_id with
      | None ->
          failwith "Unknown network."
      | Some net_db ->
          Operation.known net_db hash  >>= function
          | true ->
              failwith "Previously injected block."
          | false ->
              Raw_operation.Table.inject
                net_db.operation_db.table hash op >>= function
              | false ->
                  failwith "Previously injected block."
              | true ->
                  return (hash, op)
*)

let broadcast_head net head mempool =
  let msg : Message.t =
    Current_head (State.Net.id net.net, head, mempool) in
  P2p.Peer_id.Table.iter
    (fun _peer_id state ->
       ignore (P2p.try_send net.global_db.p2p state.conn msg))
    net.active_connections

let read_block { active_nets } hash =
  Net_id.Table.fold
    (fun _net_id net acc ->
       acc >>= function
       | Some _ -> acc
       | None ->
           Block_header.read net hash >>= function
           | None -> acc
           | Some block -> Lwt.return (Some (net, block)))
    active_nets
    Lwt.return_none

let read_block_exn t hash =
  read_block t hash >>= function
  | None -> Lwt.fail Not_found
  | Some b -> Lwt.return b

let read_operation { active_nets } hash =
  Net_id.Table.fold
    (fun _net_id net acc ->
       acc >>= function
       | Some _ -> acc
       | None ->
           Operation.read net hash >>= function
           | None -> acc
           | Some block -> Lwt.return (Some (net, block)))
    active_nets
    Lwt.return_none

let read_operation_exn t hash =
  read_operation t hash >>= function
  | None -> Lwt.fail Not_found
  | Some b -> Lwt.return b

let watch_block { block_input } =
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
