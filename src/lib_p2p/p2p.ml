(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "p2p" end)

type 'meta meta_config = 'meta P2p_pool.meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
  score : 'meta -> float
}

type 'msg app_message_encoding = 'msg P2p_pool.encoding =
    Encoding : {
      tag: int ;
      encoding: 'a Data_encoding.t ;
      wrap: 'a -> 'msg ;
      unwrap: 'msg -> 'a option ;
      max_length: int option ;
    } -> 'msg app_message_encoding

type 'msg message_config = 'msg P2p_pool.message_config = {
  encoding : 'msg app_message_encoding list ;
  versions : P2p_version.t list;
}

type config = {
  listening_port : P2p_addr.port option;
  listening_addr : P2p_addr.t option;
  trusted_points : P2p_point.Id.t list ;
  peers_file : string ;
  closed_network : bool ;
  identity : P2p_identity.t ;
  proof_of_work_target : Crypto_box.target ;
}

type limits = {

  authentification_timeout : float ;

  min_connections : int ;
  expected_connections : int ;
  max_connections : int ;

  backlog : int ;
  max_incoming_connections : int ;

  max_download_speed : int option ;
  max_upload_speed : int option ;

  read_buffer_size : int ;
  read_queue_size : int option ;
  write_queue_size : int option ;
  incoming_app_message_queue_size : int option ;
  incoming_message_queue_size : int option ;
  outgoing_message_queue_size : int option ;

  known_peer_ids_history_size : int ;
  known_points_history_size : int ;
  max_known_peer_ids : (int * int) option ;
  max_known_points : (int * int) option ;

  swap_linger : float ;

  binary_chunks_size : int option ;
}

let create_scheduler limits =
  let max_upload_speed =
    Option.map limits.max_upload_speed ~f:(( * ) 1024) in
  let max_download_speed =
    Option.map limits.max_upload_speed ~f:(( * ) 1024) in
  P2p_io_scheduler.create
    ~read_buffer_size:limits.read_buffer_size
    ?max_upload_speed
    ?max_download_speed
    ?read_queue_size:limits.read_queue_size
    ?write_queue_size:limits.write_queue_size
    ()

let create_connection_pool config limits meta_cfg msg_cfg io_sched =
  let pool_cfg = {
    P2p_pool.identity = config.identity ;
    proof_of_work_target = config.proof_of_work_target ;
    listening_port = config.listening_port ;
    trusted_points = config.trusted_points ;
    peers_file = config.peers_file ;
    closed_network = config.closed_network ;
    min_connections = limits.min_connections ;
    max_connections = limits.max_connections ;
    max_incoming_connections = limits.max_incoming_connections ;
    authentification_timeout = limits.authentification_timeout ;
    incoming_app_message_queue_size = limits.incoming_app_message_queue_size ;
    incoming_message_queue_size = limits.incoming_message_queue_size ;
    outgoing_message_queue_size = limits.outgoing_message_queue_size ;
    known_peer_ids_history_size = limits.known_peer_ids_history_size ;
    known_points_history_size = limits.known_points_history_size ;
    max_known_points = limits.max_known_points ;
    max_known_peer_ids = limits.max_known_peer_ids ;
    swap_linger = limits.swap_linger ;
    binary_chunks_size = limits.binary_chunks_size ;
  }
  in
  let pool =
    P2p_pool.create pool_cfg meta_cfg msg_cfg io_sched in
  pool

let bounds ~min ~expected ~max =
  assert (min <= expected) ;
  assert (expected <= max) ;
  let step_min =
    (expected - min) / 3
  and step_max =
    (max - expected) / 3 in
  { P2p_maintenance.min_threshold = min + step_min ;
    min_target = min + 2 * step_min ;
    max_target = max - 2 * step_max ;
    max_threshold = max - step_max ;
  }

let may_create_discovery_worker _config pool =
  Some (P2p_discovery.create pool)

let create_maintenance_worker limits pool disco =
  let bounds =
    bounds
      ~min:limits.min_connections
      ~expected:limits.expected_connections
      ~max:limits.max_connections
  in
  P2p_maintenance.run
    ~connection_timeout:limits.authentification_timeout
    bounds pool disco

let may_create_welcome_worker config limits pool =
  match config.listening_port with
  | None -> Lwt.return None
  | Some port ->
      P2p_welcome.run
        ~backlog:limits.backlog pool
        ?addr:config.listening_addr
        port >>= fun w ->
      Lwt.return (Some w)

type ('msg, 'meta) connection = ('msg, 'meta) P2p_pool.connection

module Real = struct

  type ('msg, 'meta) net = {
    config: config ;
    limits: limits ;
    io_sched: P2p_io_scheduler.t ;
    pool: ('msg, 'meta) P2p_pool.t ;
    discoverer: P2p_discovery.t option ;
    maintenance: 'meta P2p_maintenance.t ;
    welcome: P2p_welcome.t option ;
  }

  let create ~config ~limits meta_cfg msg_cfg =
    let io_sched = create_scheduler limits in
    create_connection_pool
      config limits meta_cfg msg_cfg io_sched >>= fun pool ->
    let discoverer = may_create_discovery_worker config pool in
    let maintenance = create_maintenance_worker limits pool discoverer in
    may_create_welcome_worker config limits pool >>= fun welcome ->
    return {
      config ;
      limits ;
      io_sched ;
      pool ;
      discoverer ;
      maintenance ;
      welcome ;
    }

  let peer_id { config } = config.identity.peer_id

  let maintain { maintenance } () =
    P2p_maintenance.maintain maintenance

  let roll _net () = Lwt.return_unit (* TODO implement *)

  (* returns when all workers have shutted down in the opposite
     creation order. *)
  let shutdown net () =
    Lwt_utils.may ~f:P2p_welcome.shutdown net.welcome >>= fun () ->
    P2p_maintenance.shutdown net.maintenance >>= fun () ->
    Lwt_utils.may ~f:P2p_discovery.shutdown net.discoverer >>= fun () ->
    P2p_pool.destroy net.pool >>= fun () ->
    P2p_io_scheduler.shutdown ~timeout:3.0 net.io_sched

  let connections { pool } () =
    P2p_pool.Connection.fold pool
      ~init:[] ~f:(fun _peer_id c acc -> c :: acc)
  let find_connection { pool } peer_id =
    P2p_pool.Connection.find_by_peer_id pool peer_id
  let disconnect ?wait conn =
    P2p_pool.disconnect ?wait conn
  let connection_info _net conn =
    P2p_pool.Connection.info conn
  let connection_stat _net conn =
    P2p_pool.Connection.stat conn
  let global_stat { pool } () =
    P2p_pool.pool_stat pool
  let set_metadata { pool } conn meta =
    P2p_pool.Peers.set_metadata pool conn meta
  let get_metadata { pool } conn =
    P2p_pool.Peers.get_metadata pool conn

  let recv _net conn =
    P2p_pool.read conn >>=? fun msg ->
    lwt_debug "message read from %a"
      P2p_connection.Info.pp
      (P2p_pool.Connection.info conn) >>= fun () ->
    return msg

  let rec recv_any net () =
    let pipes =
      P2p_pool.Connection.fold
        net.pool ~init:[]
        ~f:begin fun _peer_id conn acc ->
          (P2p_pool.is_readable conn >>= function
            | Ok () -> Lwt.return (Some conn)
            | Error _ -> Lwt_utils.never_ending) :: acc
        end in
    Lwt.pick (
      ( P2p_pool.Pool_event.wait_new_connection net.pool >>= fun () ->
        Lwt.return_none )::
      pipes) >>= function
    | None -> recv_any net ()
    | Some conn ->
        P2p_pool.read conn >>= function
        | Ok msg ->
            lwt_debug "message read from %a"
              P2p_connection.Info.pp
              (P2p_pool.Connection.info conn) >>= fun () ->
            Lwt.return (conn, msg)
        | Error _ ->
            lwt_debug "error reading message from %a"
              P2p_connection.Info.pp
              (P2p_pool.Connection.info conn) >>= fun () ->
            Lwt_unix.yield () >>= fun () ->
            recv_any net ()

  let send _net conn m =
    P2p_pool.write conn m >>= function
    | Ok () ->
        lwt_debug "message sent to %a"
          P2p_connection.Info.pp
          (P2p_pool.Connection.info conn) >>= fun () ->
        return ()
    | Error err ->
        lwt_debug "error sending message from %a: %a"
          P2p_connection.Info.pp
          (P2p_pool.Connection.info conn)
          pp_print_error err >>= fun () ->
        Lwt.return (Error err)

  let try_send _net conn v =
    match P2p_pool.write_now conn v with
    | Ok v ->
        debug "message trysent to %a"
          P2p_connection.Info.pp
          (P2p_pool.Connection.info conn) ;
        v
    | Error err ->
        debug "error trysending message to %a@ %a"
          P2p_connection.Info.pp
          (P2p_pool.Connection.info conn)
          pp_print_error err ;
        false

  let broadcast { pool } msg =
    P2p_pool.write_all pool msg ;
    debug "message broadcasted"

  let fold_connections { pool } ~init ~f =
    P2p_pool.Connection.fold pool ~init ~f

  let iter_connections { pool } f =
    P2p_pool.Connection.fold pool
      ~init:()
      ~f:(fun gid conn () -> f gid conn)

  let on_new_connection { pool } f =
    P2p_pool.on_new_connection pool f

  let pool { pool } = pool
end

module Fake = struct

  let id = P2p_identity.generate (Crypto_box.make_target 0.)
  let empty_stat = {
    P2p_stat.total_sent = 0L ;
    total_recv = 0L ;
    current_inflow = 0 ;
    current_outflow = 0 ;
  }
  let connection_info = {
    P2p_connection.Info.incoming = false ;
    peer_id = id.peer_id ;
    id_point = (Ipaddr.V6.unspecified, None) ;
    remote_socket_port = 0 ;
    versions = [] ;
  }

end

type ('msg, 'meta) t = {
  peer_id : P2p_peer.Id.t ;
  maintain : unit -> unit Lwt.t ;
  roll : unit -> unit Lwt.t ;
  shutdown : unit -> unit Lwt.t ;
  connections : unit -> ('msg, 'meta) connection list ;
  find_connection : P2p_peer.Id.t -> ('msg, 'meta) connection option ;
  disconnect : ?wait:bool -> ('msg, 'meta) connection -> unit Lwt.t ;
  connection_info : ('msg, 'meta) connection -> P2p_connection.Info.t ;
  connection_stat : ('msg, 'meta) connection -> P2p_stat.t ;
  global_stat : unit -> P2p_stat.t ;
  get_metadata : P2p_peer.Id.t -> 'meta ;
  set_metadata : P2p_peer.Id.t -> 'meta -> unit ;
  recv : ('msg, 'meta) connection -> 'msg tzresult Lwt.t ;
  recv_any : unit -> (('msg, 'meta) connection * 'msg) Lwt.t ;
  send : ('msg, 'meta) connection -> 'msg -> unit tzresult Lwt.t ;
  try_send : ('msg, 'meta) connection -> 'msg -> bool ;
  broadcast : 'msg -> unit ;
  pool : ('msg, 'meta) P2p_pool.t option ;
  fold_connections :
    'a. init:'a -> f:(P2p_peer.Id.t -> ('msg, 'meta) connection -> 'a -> 'a) -> 'a ;
  iter_connections : (P2p_peer.Id.t -> ('msg, 'meta) connection -> unit) -> unit ;
  on_new_connection : (P2p_peer.Id.t -> ('msg, 'meta) connection -> unit) -> unit ;
}
type ('msg, 'meta) net = ('msg, 'meta) t

let check_limits =
  let fail_1 v orig =
    if not (v <= 0.) then return ()
    else
      Error_monad.failwith "value of option %S cannot be negative or null@."
        orig
  in
  let fail_2 v orig =
    if not (v < 0) then return ()
    else
      Error_monad.failwith "value of option %S cannot be negative@." orig
  in
  fun c ->
    fail_1 c.authentification_timeout
      "authentification-timeout" >>=? fun () ->
    fail_2 c.min_connections
      "min-connections" >>=? fun () ->
    fail_2 c.expected_connections
      "expected-connections" >>=? fun () ->
    fail_2 c.max_connections
      "max-connections" >>=? fun () ->
    fail_2 c.max_incoming_connections
      "max-incoming-connections" >>=? fun () ->
    fail_2 c.read_buffer_size
      "read-buffer-size" >>=? fun () ->
    fail_2 c.known_peer_ids_history_size
      "known-peer-ids-history-size" >>=? fun () ->
    fail_2 c.known_points_history_size
      "known-points-history-size" >>=? fun () ->
    fail_1 c.swap_linger
      "swap-linger" >>=? fun () ->
    begin
      match c.binary_chunks_size with
      | None -> return ()
      | Some size -> P2p_socket.check_binary_chunks_size size
    end >>=? fun () ->
    return ()

let create ~config ~limits meta_cfg msg_cfg =
  check_limits limits >>=? fun () ->
  Real.create ~config ~limits meta_cfg msg_cfg  >>=? fun net ->
  return {
    peer_id = Real.peer_id net ;
    maintain = Real.maintain net ;
    roll = Real.roll net  ;
    shutdown = Real.shutdown net  ;
    connections = Real.connections net  ;
    find_connection = Real.find_connection net ;
    disconnect = Real.disconnect ;
    connection_info = Real.connection_info net  ;
    connection_stat = Real.connection_stat net ;
    global_stat = Real.global_stat net ;
    get_metadata = Real.get_metadata net ;
    set_metadata = Real.set_metadata net ;
    recv = Real.recv net ;
    recv_any = Real.recv_any net ;
    send = Real.send net ;
    try_send = Real.try_send net ;
    broadcast = Real.broadcast net ;
    pool = Some net.pool ;
    fold_connections = (fun ~init ~f -> Real.fold_connections net ~init ~f) ;
    iter_connections = Real.iter_connections net ;
    on_new_connection = Real.on_new_connection net ;
  }

let faked_network meta_config = {
  peer_id = Fake.id.peer_id ;
  maintain = Lwt.return ;
  roll = Lwt.return ;
  shutdown = Lwt.return ;
  connections = (fun () -> []) ;
  find_connection = (fun _ -> None) ;
  disconnect = (fun ?wait:_ _ -> Lwt.return_unit) ;
  connection_info = (fun _ -> Fake.connection_info) ;
  connection_stat = (fun _ -> Fake.empty_stat) ;
  global_stat = (fun () -> Fake.empty_stat) ;
  get_metadata = (fun _ -> meta_config.initial) ;
  set_metadata = (fun _ _ -> ()) ;
  recv = (fun _ -> Lwt_utils.never_ending) ;
  recv_any = (fun () -> Lwt_utils.never_ending) ;
  send = (fun _ _ -> fail P2p_pool.Connection_closed) ;
  try_send = (fun _ _ -> false) ;
  fold_connections = (fun ~init ~f:_ -> init) ;
  iter_connections = (fun _f -> ()) ;
  on_new_connection = (fun _f -> ()) ;
  broadcast = ignore ;
  pool = None
}

let peer_id net = net.peer_id
let maintain net = net.maintain ()
let roll net = net.roll ()
let shutdown net = net.shutdown ()
let connections net = net.connections ()
let disconnect net = net.disconnect
let find_connection net = net.find_connection
let connection_info net = net.connection_info
let connection_stat net = net.connection_stat
let global_stat net = net.global_stat ()
let get_metadata net = net.get_metadata
let set_metadata net = net.set_metadata
let recv net = net.recv
let recv_any net = net.recv_any ()
let send net = net.send
let try_send net = net.try_send
let broadcast net = net.broadcast
let fold_connections net = net.fold_connections
let iter_connections net = net.iter_connections
let on_new_connection net = net.on_new_connection

module Raw = struct
  type 'a t = 'a P2p_pool.Message.t =
    | Bootstrap
    | Advertise of P2p_point.Id.t list
    | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
    | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
    | Message of 'a
    | Disconnect
  let encoding = P2p_pool.Message.encoding
end

module RPC = struct

  let stat net =
    match net.pool with
    | None -> P2p_stat.empty
    | Some pool -> P2p_pool.pool_stat pool

  let watch net =
    match net.pool with
    | None -> Lwt_watcher.create_fake_stream ()
    | Some pool -> P2p_pool.watch pool

  let connect net point timeout =
    match net.pool with
    | None -> failwith "fake net"
    | Some pool ->
        P2p_pool.connect ~timeout pool point >>|? ignore

  module Connection = struct
    let info net peer_id =
      match net.pool with
      | None -> None
      | Some pool ->
          Option.map
            (P2p_pool.Connection.find_by_peer_id pool peer_id)
            ~f:P2p_pool.Connection.info

    let kick net peer_id wait =
      match net.pool with
      | None -> Lwt.return_unit
      | Some pool ->
          match P2p_pool.Connection.find_by_peer_id pool peer_id with
          | None -> Lwt.return_unit
          | Some conn -> P2p_pool.disconnect ~wait conn

    let list net =
      match net.pool with
      | None -> []
      | Some pool ->
          P2p_pool.Connection.fold
            pool ~init:[]
            ~f:begin fun _peer_id c acc ->
              P2p_pool.Connection.info c :: acc
            end

    let count net =
      match net.pool with
      | None -> 0
      | Some pool -> P2p_pool.active_connections pool
  end

  module Point = struct

    open P2p_point.Info
    open P2p_point.State

    let info_of_point_info i =
      let state = match P2p_point.Pool_state.get i with
        | Requested _ -> Requested
        | Accepted { current_peer_id ; _ } -> Accepted current_peer_id
        | Running { current_peer_id ; _ } -> Running current_peer_id
        | Disconnected -> Disconnected in
      P2p_point.Pool_info.{
        trusted = trusted i ;
        state ;
        greylisted_until = greylisted_until i ;
        last_failed_connection = last_failed_connection i ;
        last_rejected_connection = last_rejected_connection i ;
        last_established_connection = last_established_connection i ;
        last_disconnection = last_disconnection i ;
        last_seen = last_seen i ;
        last_miss = last_miss i ;
      }

    let info net point =
      match net.pool with
      | None -> None
      | Some pool ->
          Option.map
            (P2p_pool.Points.info pool point)
            ~f:info_of_point_info

    let events ?(max=max_int) ?(rev=false) net point =
      match net.pool with
      | None -> []
      | Some pool ->
          Option.unopt_map
            (P2p_pool.Points.info pool point)
            ~default:[]
            ~f:begin fun pi ->
              let evts =
                P2p_point.Pool_event.fold
                  pi ~init:[] ~f:(fun a e -> e :: a) in
              (if rev then List.rev_sub else List.sub) evts max
            end

    let watch net point =
      match net.pool with
      | None -> raise Not_found
      | Some pool ->
          match P2p_pool.Points.info pool point with
          | None -> raise Not_found
          | Some pi -> P2p_point.Pool_event.watch pi

    let list ?(restrict=[]) net =
      match net.pool with
      | None -> []
      | Some pool ->
          P2p_pool.Points.fold_known
            pool ~init:[]
            ~f:begin fun point i a ->
              let info = info_of_point_info i in
              match restrict with
              | [] -> (point, info) :: a
              | _ when List.mem info.state restrict -> (point, info) :: a
              | _ -> a
            end

  end

  module Peer_id = struct

    open P2p_peer.Info
    open P2p_peer.State

    let info_of_peer_info pool i =
      let state, id_point = match P2p_peer.Pool_state.get i with
        | Accepted { current_point } -> Accepted, Some current_point
        | Running { current_point } -> Running, Some current_point
        | Disconnected -> Disconnected, None
      in
      let peer_id = P2p_peer.Pool_info.peer_id i in
      let score = P2p_pool.Peers.get_score pool peer_id in
      let stat =
        match P2p_pool.Connection.find_by_peer_id pool peer_id with
        | None -> P2p_stat.empty
        | Some conn -> P2p_pool.Connection.stat conn
      in P2p_peer.Pool_info.{
          score ;
          trusted = trusted i ;
          state ;
          id_point ;
          stat ;
          last_failed_connection = last_failed_connection i ;
          last_rejected_connection = last_rejected_connection i ;
          last_established_connection = last_established_connection i ;
          last_disconnection = last_disconnection i ;
          last_seen = last_seen i ;
          last_miss = last_miss i ;
        }

    let info net peer_id =
      match net.pool with
      | None -> None
      | Some pool -> begin
          match P2p_pool.Peers.info pool peer_id with
          | Some info -> Some (info_of_peer_info pool info)
          | None -> None
        end

    let events ?(max=max_int) ?(rev=false) net peer_id =
      match net.pool with
      | None -> []
      | Some pool ->
          Option.unopt_map
            (P2p_pool.Peers.info pool peer_id)
            ~default:[]
            ~f:begin fun gi ->
              let evts = P2p_peer.Pool_event.fold gi
                  ~init:[] ~f:(fun a e -> e :: a) in
              (if rev then List.rev_sub else List.sub) evts max
            end

    let watch net peer_id =
      match net.pool with
      | None -> raise Not_found
      | Some pool ->
          match P2p_pool.Peers.info pool peer_id with
          | None -> raise Not_found
          | Some gi -> P2p_peer.Pool_event.watch gi

    let list ?(restrict=[]) net =
      match net.pool with
      | None -> []
      | Some pool ->
          P2p_pool.Peers.fold_known pool
            ~init:[]
            ~f:begin fun peer_id i a ->
              let info = info_of_peer_info pool i in
              match restrict with
              | [] -> (peer_id, info) :: a
              | _ when List.mem info.state restrict -> (peer_id, info) :: a
              | _ -> a
            end

  end

end
