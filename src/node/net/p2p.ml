(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include P2p_types

type 'meta meta_config = 'meta P2p_connection_pool.meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
}

type 'msg app_message_encoding = 'msg P2p_connection_pool.encoding =
    Encoding : {
      tag: int ;
      encoding: 'a Data_encoding.t ;
      wrap: 'a -> 'msg ;
      unwrap: 'msg -> 'a option ;
      max_length: int option ;
    } -> 'msg app_message_encoding

type 'msg message_config = 'msg P2p_connection_pool.message_config = {
  encoding : 'msg app_message_encoding list ;
  versions : Version.t list;
}

type config = {
  listening_port : port option;
  listening_addr : addr option;
  trusted_points : Point.t list ;
  peers_file : string ;
  closed_network : bool ;
  identity : Identity.t ;
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

}

let create_scheduler limits =
  let max_upload_speed =
    map_option limits.max_upload_speed ~f:(( * ) 1024) in
  let max_download_speed =
    map_option limits.max_upload_speed ~f:(( * ) 1024) in
  P2p_io_scheduler.create
    ~read_buffer_size:limits.read_buffer_size
    ?max_upload_speed
    ?max_download_speed
    ?read_queue_size:limits.read_queue_size
    ?write_queue_size:limits.write_queue_size
    ()

let create_connection_pool config limits meta_cfg msg_cfg io_sched =
  let pool_cfg = {
    P2p_connection_pool.identity = config.identity ;
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
  }
  in
  let pool =
    P2p_connection_pool.create pool_cfg meta_cfg msg_cfg io_sched in
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
      limits.min_connections
      limits.expected_connections
      limits.max_connections
  in
  P2p_maintenance.run
    ~connection_timeout:limits.authentification_timeout bounds pool disco

let may_create_welcome_worker config limits pool =
  match config.listening_port with
  | None -> Lwt.return None
  | Some port ->
      P2p_welcome.run
        ~backlog:limits.backlog pool
        ?addr:config.listening_addr
        port >>= fun w ->
      Lwt.return (Some w)

type ('msg, 'meta) connection = ('msg, 'meta) P2p_connection_pool.connection

module Real = struct

  type ('msg, 'meta) net = {
    config: config ;
    limits: limits ;
    io_sched: P2p_io_scheduler.t ;
    pool: ('msg, 'meta) P2p_connection_pool.t ;
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
    Lwt.return {
      config ;
      limits ;
      io_sched ;
      pool ;
      discoverer ;
      maintenance ;
      welcome ;
    }

  let gid { config } = config.identity.gid

  let maintain { maintenance } () =
    P2p_maintenance.maintain maintenance

  let roll _net () = Lwt.return_unit (* TODO implement *)

  (* returns when all workers have shutted down in the opposite
     creation order. *)
  let shutdown net () =
    Lwt_utils.may ~f:P2p_welcome.shutdown net.welcome >>= fun () ->
    P2p_maintenance.shutdown net.maintenance >>= fun () ->
    Lwt_utils.may ~f:P2p_discovery.shutdown net.discoverer >>= fun () ->
    P2p_connection_pool.destroy net.pool >>= fun () ->
    P2p_io_scheduler.shutdown net.io_sched

  let connections { pool } () =
    P2p_connection_pool.fold_connections pool
      ~init:[] ~f:(fun _gid c acc -> c :: acc)
  let find_connection { pool } gid =
    P2p_connection_pool.Gids.find_connection pool gid
  let connection_info _net conn =
    P2p_connection_pool.connection_info conn
  let connection_stat _net conn =
    P2p_connection_pool.connection_stat conn
  let global_stat { pool } () =
    P2p_connection_pool.pool_stat pool
  let set_metadata { pool } conn meta =
    P2p_connection_pool.Gids.set_metadata pool conn meta
  let get_metadata { pool } conn =
    P2p_connection_pool.Gids.get_metadata pool conn

  let rec recv _net conn =
    P2p_connection_pool.read conn

  let rec recv_any net () =
    let pipes =
      P2p_connection_pool.fold_connections
        net.pool ~init:[] ~f:begin fun _gid conn acc ->
        (P2p_connection_pool.is_readable conn >>= function
          | Ok () -> Lwt.return conn
          | Error _ -> Lwt_utils.never_ending) :: acc
      end in
    Lwt.pick pipes >>= fun conn ->
    P2p_connection_pool.read conn >>= function
    | Ok msg ->
        Lwt.return (conn, msg)
    | Error _ ->
        Lwt_unix.yield () >>= fun () ->
        recv_any net ()

  let send _net c m =
    P2p_connection_pool.write c m >>= function
    | Ok () -> Lwt.return_unit
    | Error _ -> Lwt.fail End_of_file (* temporary *)

  let try_send _net c v =
    match P2p_connection_pool.write_now c v with
    | Ok v -> v
    | Error _ -> false

  let broadcast { pool } msg = P2p_connection_pool.write_all pool msg

end

module Fake = struct

  let id = Identity.generate (Crypto_box.make_target 0.)
  let empty_stat = {
    Stat.total_sent = 0 ;
    total_recv = 0 ;
    current_inflow = 0 ;
    current_outflow = 0 ;
  }
  let connection_info = {
    Connection_info.incoming = false ;
    gid = id.gid ;
    id_point = (Ipaddr.V6.unspecified, None) ;
    remote_socket_port = 0 ;
    versions = [] ;
  }

end

type ('msg, 'meta) t = {
  gid : Gid.t ;
  maintain : unit -> unit Lwt.t ;
  roll : unit -> unit Lwt.t ;
  shutdown : unit -> unit Lwt.t ;
  connections : unit -> ('msg, 'meta) connection list ;
  find_connection : Gid.t -> ('msg, 'meta) connection option ;
  connection_info : ('msg, 'meta) connection -> Connection_info.t ;
  connection_stat : ('msg, 'meta) connection -> Stat.t ;
  global_stat : unit -> Stat.t ;
  get_metadata : Gid.t -> 'meta option ;
  set_metadata : Gid.t -> 'meta -> unit ;
  recv : ('msg, 'meta) connection -> 'msg tzresult Lwt.t ;
  recv_any : unit -> (('msg, 'meta) connection * 'msg) Lwt.t ;
  send : ('msg, 'meta) connection -> 'msg -> unit Lwt.t ;
  try_send : ('msg, 'meta) connection -> 'msg -> bool ;
  broadcast : 'msg -> unit ;
}
type ('msg, 'meta) net = ('msg, 'meta) t

let create ~config ~limits meta_cfg msg_cfg =
  Real.create ~config ~limits meta_cfg msg_cfg  >>= fun net ->
  Lwt.return {
    gid = Real.gid net ;
    maintain = Real.maintain net ;
    roll = Real.roll net  ;
    shutdown = Real.shutdown net  ;
    connections = Real.connections net  ;
    find_connection = Real.find_connection net ;
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
  }

let faked_network = {
  gid = Fake.id.gid ;
  maintain = Lwt.return ;
  roll = Lwt.return ;
  shutdown = Lwt.return ;
  connections = (fun () -> []) ;
  find_connection = (fun _ -> None) ;
  connection_info = (fun _ -> Fake.connection_info) ;
  connection_stat = (fun _ -> Fake.empty_stat) ;
  global_stat = (fun () -> Fake.empty_stat) ;
  get_metadata = (fun _ -> None) ;
  set_metadata = (fun _ _ -> ()) ;
  recv = (fun _ -> Lwt_utils.never_ending) ;
  recv_any = (fun () -> Lwt_utils.never_ending) ;
  send = (fun _ _ -> Lwt_utils.never_ending) ;
  try_send = (fun _ _ -> false) ;
  broadcast = ignore ;
}

let gid net = net.gid
let maintain net = net.maintain ()
let roll net = net.roll ()
let shutdown net = net.shutdown ()
let connections net = net.connections ()
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

module Raw = struct
  type 'a t = 'a P2p_connection_pool.Message.t =
    | Bootstrap
    | Advertise of P2p_types.Point.t list
    | Message of 'a
    | Disconnect
  let encoding = P2p_connection_pool.Message.encoding
end
