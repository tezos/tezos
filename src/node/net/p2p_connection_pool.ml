(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO check version negotiation *)

(* TODO Test cancelation of a (pending) connection *)

(* TODO do not recompute list_known_points at each requests...  but
        only once in a while, e.g. every minutes or when a point
        or the associated gid is blacklisted. *)

(* TODO allow to track "requested gids" when we reconnect to a point. *)

open P2p_types
open P2p_connection_pool_types

include Logging.Make (struct let name = "p2p.connection-pool" end)

type 'msg encoding = Encoding : {
    tag: int ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg encoding

module Message = struct

  type 'msg t =
    | Bootstrap
    | Advertise of Point.t list
    | Message of 'msg
    | Disconnect

  let encoding msg_encoding =
    let open Data_encoding in
    union ~tag_size:`Uint16
      ([ case ~tag:0x01 null
           (function Disconnect -> Some () | _ -> None)
           (fun () -> Disconnect);
         case ~tag:0x02 null
           (function Bootstrap -> Some () | _ -> None)
           (fun () -> Bootstrap);
         case ~tag:0x03 (Variable.list Point.encoding)
           (function Advertise points -> Some points | _ -> None)
           (fun points -> Advertise points);
       ] @
       ListLabels.map msg_encoding
         ~f:(function Encoding { tag ; encoding ; wrap ; unwrap } ->
             case ~tag encoding
               (function Message msg -> unwrap msg | _ -> None)
               (fun msg -> Message (wrap msg))))

end


module Answerer = struct

  type 'msg callback = {
    bootstrap: unit -> Point.t list Lwt.t ;
    advertise: Point.t list -> unit Lwt.t ;
    message: int -> 'msg -> unit Lwt.t ;
  }

  type 'msg t = {
    canceler: Canceler.t ;
    conn: 'msg Message.t P2p_connection.t ;
    callback: 'msg callback ;
    mutable worker: unit Lwt.t ;
  }

  let rec worker_loop st =
    Lwt_unix.yield () >>= fun () ->
    Lwt_utils.protect ~canceler:st.canceler begin fun () ->
      P2p_connection.read st.conn
    end >>= function
    | Ok (_, Bootstrap) -> begin
        st.callback.bootstrap () >>= function
        | [] ->
            worker_loop st
        | points ->
            match P2p_connection.write_now st.conn (Advertise points) with
            | Ok _sent ->
                (* if not sent then ?? TODO count dropped message ?? *)
                worker_loop st
            | Error _ ->
                Canceler.cancel st.canceler >>= fun () ->
                Lwt.return_unit
      end
    | Ok (_, Advertise points) ->
        st.callback.advertise points >>= fun () ->
        worker_loop st
    | Ok (size, Message msg) ->
        st.callback.message size msg >>= fun () ->
        worker_loop st
    | Ok (_, Disconnect) | Error [P2p_io_scheduler.Connection_closed] ->
        Canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [Lwt_utils.Canceled] ->
        Lwt.return_unit
    | Error err ->
        lwt_log_error "@[Answerer unexpected error:@ %a@]"
          Error_monad.pp_print_error err >>= fun () ->
        Canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit

  let run conn canceler callback =
    let st = {
      canceler ; conn ; callback ;
      worker = Lwt.return_unit ;
    } in
    st.worker <-
      Lwt_utils.worker "answerer"
        (fun () -> worker_loop st)
        (fun () -> Canceler.cancel canceler) ;
    st

  let shutdown st =
    Canceler.cancel st.canceler >>= fun () ->
    st.worker

end

type config = {

  identity : Identity.t ;
  proof_of_work_target : Crypto_box.target ;

  trusted_points : Point.t list ;
  peers_file : string ;
  closed_network : bool ;

  listening_port : port option ;
  min_connections : int ;
  max_connections : int ;
  max_incoming_connections : int ;
  authentification_timeout : float ;

  incoming_app_message_queue_size : int option ;
  incoming_message_queue_size : int option ;
  outgoing_message_queue_size : int option ;

}

type 'meta meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
}

type 'msg message_config = {
  encoding : 'msg encoding list ;
  versions : P2p_types.Version.t list;
}

type ('msg, 'meta) t = {
  config : config ;
  meta_config : 'meta meta_config ;
  message_config : 'msg message_config ;
  my_id_points : unit Point.Table.t ;
  known_gids : (('msg, 'meta) connection, 'meta) Gid_info.t Gid.Table.t ;
  connected_gids : (('msg, 'meta) connection, 'meta) Gid_info.t Gid.Table.t ;
  known_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  connected_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  incoming : Canceler.t Point.Table.t ;
  io_sched : P2p_io_scheduler.t ;
  encoding : 'msg Message.t Data_encoding.t ;
  events : events ;
}

and events = {
  too_few_connections : unit Lwt_condition.t ;
  too_many_connections : unit Lwt_condition.t ;
  new_point : unit Lwt_condition.t ;
}

and ('msg, 'meta) connection = {
  canceler : Canceler.t ;
  messages : (int * 'msg) Lwt_pipe.t ;
  conn : 'msg Message.t P2p_connection.t ;
  gid_info : (('msg, 'meta) connection, 'meta) Gid_info.t ;
  point_info : ('msg, 'meta) connection Point_info.t option ;
  answerer : 'msg Answerer.t ;
  mutable wait_close : bool ;
}

type ('msg, 'meta) pool = ('msg, 'meta) t

let register_point pool ?trusted (addr, port as point) =
  match Point.Table.find pool.known_points point with
  | exception Not_found ->
      let pi = Point_info.create ?trusted addr port in
      Point.Table.add pool.known_points point pi ;
      pi
  | pi -> pi

let register_peer pool gid =
  match Gid.Table.find pool.known_gids gid with
  | exception Not_found ->
      Lwt_condition.broadcast pool.events.new_point () ;
      let peer = Gid_info.create gid ~metadata:pool.meta_config.initial in
      Gid.Table.add pool.known_gids gid peer ;
      peer
  | peer -> peer

let register_new_point pool _gid point =
  if not (Point.Table.mem pool.my_id_points point) then
    ignore (register_point pool point)

let register_new_points pool gid points =
  List.iter (register_new_point pool gid) points ;
  Lwt.return_unit

let compare_known_point_info p1 p2 =
  (* The most-recently disconnected peers are greater. *)
  (* Then come long-standing connected peers. *)
  let disconnected1 = Point_info.State.is_disconnected p1
  and disconnected2 = Point_info.State.is_disconnected p2 in
  let compare_last_seen p1 p2 =
    match Point_info.last_seen p1, Point_info.last_seen p2 with
    | None, None -> Random.int 2 * 2 - 1 (* HACK... *)
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some (_, time1), Some (_, time2) ->
        match compare time1 time2 with
        | 0 -> Random.int 2 * 2 - 1 (* HACK... *)
        | x -> x in
  match disconnected1, disconnected2 with
  | false, false -> compare_last_seen p1 p2
  | false, true -> -1
  | true, false -> 1
  | true, true -> compare_last_seen p2 p1

let list_known_points pool _gid () =
  let knowns =
    Point.Table.fold (fun _ pi acc -> pi :: acc) pool.known_points [] in
  let best_knowns =
    Utils.take_n ~compare:compare_known_point_info 50 knowns in
  Lwt.return (List.map Point_info.point best_knowns)

let active_connections pool = Gid.Table.length pool.connected_gids

let create_connection pool conn id_point pi gi =
  let gid = Gid_info.gid gi in
  let canceler = Canceler.create () in
  let size =
    map_option pool.config.incoming_app_message_queue_size
      ~f:(fun qs -> qs, fun (size, _) -> (Sys.word_size / 8) * 11 + size) in
  let messages = Lwt_pipe.create ?size () in
  let callback =
    { Answerer.message =
        (fun size msg -> Lwt_pipe.push messages (size, msg)) ;
      advertise = register_new_points pool gid ;
      bootstrap = list_known_points pool gid ;
    } in
  let answerer = Answerer.run conn canceler callback in
  let conn =
    { conn ; point_info = pi ; gid_info = gi ;
      messages ; canceler ; answerer ; wait_close = false } in
  iter_option pi ~f:begin fun pi ->
    Point_info.State.set_running pi gid conn ;
    Point.Table.add pool.connected_points (Point_info.point pi) pi ;
  end ;
  Gid_info.State.set_running gi id_point conn ;
  Gid.Table.add pool.connected_gids gid gi ;
  Canceler.on_cancel canceler begin fun () ->
    lwt_debug "Disconnect: %a (%a)"
      Gid.pp gid Id_point.pp id_point >>= fun () ->
    iter_option ~f:Point_info.State.set_disconnected pi;
    Gid_info.State.set_disconnected gi ;
    iter_option pi ~f:begin fun pi ->
      Point.Table.remove pool.connected_points (Point_info.point pi) ;
    end ;
    Gid.Table.remove pool.connected_gids gid ;
    if pool.config.max_connections <= active_connections pool then
      Lwt_condition.broadcast pool.events.too_many_connections () ;
    P2p_connection.close ~wait:conn.wait_close conn.conn
  end ;
  if active_connections pool < pool.config.min_connections then
    Lwt_condition.broadcast pool.events.too_few_connections () ;
  conn

let disconnect ?(wait = false) conn =
  conn.wait_close <- wait ;
  Canceler.cancel conn.canceler >>= fun () ->
  conn.answerer.worker

type error += Rejected of Gid.t
type error += Unexpected_point_state
type error += Unexpected_gid_state

let may_register_my_id_point pool = function
  | [P2p_connection.Myself (addr, Some port)] ->
      Point.Table.add pool.my_id_points (addr, port) () ;
      Point.Table.remove pool.known_points (addr, port)
  | _ -> ()

let authenticate pool ?pi canceler fd point =
  let incoming = pi = None in
  lwt_debug "authenticate: %a%s"
    Point.pp point
    (if incoming then " incoming" else "") >>= fun () ->
  Lwt_utils.protect ~canceler begin fun () ->
    P2p_connection.authenticate
      ~proof_of_work_target:pool.config.proof_of_work_target
      ~incoming (P2p_io_scheduler.register pool.io_sched fd) point
      ?listening_port:pool.config.listening_port
      pool.config.identity pool.message_config.versions
  end ~on_error: begin fun err ->
    (* TODO do something when the error is Not_enough_proof_of_work ?? *)
    lwt_debug "authenticate: %a%s -> failed %a"
      Point.pp point
      (if incoming then " incoming" else "")
      pp_print_error err >>= fun () ->
    may_register_my_id_point pool err ;
    if incoming then
      Point.Table.remove pool.incoming point
    else
      iter_option Point_info.State.set_disconnected pi ;
    Lwt.return (Error err)
  end >>=? fun (info, auth_fd) ->
  lwt_debug "authenticate: %a -> auth %a"
    Point.pp point
    Connection_info.pp info >>= fun () ->
  let remote_pi =
    match info.id_point with
    | addr, Some port
      when not (Point.Table.mem pool.my_id_points (addr, port)) ->
        Some (register_point pool (addr, port))
    | _ -> None in
  let connection_pi =
    match pi, remote_pi with
    | None, None -> None
    | Some _ as pi, _ | _, (Some _ as pi) -> pi in
  let gi = register_peer pool info.gid in
  let acceptable_point =
    unopt_map connection_pi
      ~default:(not pool.config.closed_network)
      ~f:begin fun connection_pi ->
      match Point_info.State.get connection_pi with
      | Requested _ -> not incoming
      | Disconnected ->
          not pool.config.closed_network
          || Point_info.trusted connection_pi
      | Accepted _ | Running _ -> false
    end
  in
  let acceptable_gid =
    match Gid_info.State.get gi with
    | Accepted _ ->
        (* TODO: in some circumstances cancel and accept... *)
        false
    | Running _ -> false
    | Disconnected -> true
  in
  if incoming then Point.Table.remove pool.incoming point ;
  if not acceptable_gid || not acceptable_point then begin
    lwt_debug "authenticate: %a -> kick %a point: %B gid: %B"
      Point.pp point
      Connection_info.pp info
      acceptable_point acceptable_gid >>= fun () ->
    P2p_connection.kick auth_fd >>= fun () ->
    if not incoming then begin
      iter_option ~f:Point_info.State.set_disconnected pi ;
      (* FIXME Gid_info.State.set_disconnected ~requested:true gi ; *)
    end ;
    fail (Rejected info.gid)
  end else begin
    iter_option connection_pi
      ~f:(fun pi -> Point_info.State.set_accepted pi info.gid canceler) ;
    Gid_info.State.set_accepted gi info.id_point canceler ;
    lwt_debug "authenticate: %a -> accept %a"
      Point.pp point
      Connection_info.pp info >>= fun () ->
    Lwt_utils.protect ~canceler begin fun () ->
      P2p_connection.accept
        ?incoming_message_queue_size:pool.config.incoming_message_queue_size
        ?outgoing_message_queue_size:pool.config.outgoing_message_queue_size
        auth_fd pool.encoding >>= fun conn ->
      lwt_debug "authenticate: %a -> Connected %a"
        Point.pp point
        Connection_info.pp info >>= fun () ->
      Lwt.return conn
    end ~on_error: begin fun err ->
      lwt_debug "authenticate: %a -> rejected %a"
        Point.pp point
        Connection_info.pp info >>= fun () ->
      iter_option connection_pi ~f:Point_info.State.set_disconnected;
      Gid_info.State.set_disconnected gi ;
      Lwt.return (Error err)
    end >>=? fun conn ->
    let id_point =
      match info.id_point, map_option Point_info.point pi with
      | (addr, _), Some (_, port) -> addr, Some port
      | id_point, None ->  id_point in
    return (create_connection pool conn id_point connection_pi gi)
  end

type error += Pending_connection
type error += Connected
type error += Connection_closed = P2p_io_scheduler.Connection_closed
type error += Connection_refused
type error += Closed_network

let fail_unless_disconnected_point pi =
  match Point_info.State.get pi with
  | Disconnected -> return ()
  | Requested _ | Accepted _ -> fail Pending_connection
  | Running _ -> fail Connected

let fail_unless_disconnected_gid gi =
  match Gid_info.State.get gi with
  | Disconnected -> return ()
  | Accepted _ -> fail Pending_connection
  | Running _ -> fail Connected

let raw_connect canceler pool point =
  let pi = register_point pool point in
  let addr, port as point = Point_info.point pi in
  fail_unless
    (not pool.config.closed_network || Point_info.trusted pi)
    Closed_network >>=? fun () ->
  fail_unless_disconnected_point pi >>=? fun () ->
  Point_info.State.set_requested pi canceler ;
  let fd = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  let uaddr =
    Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  lwt_debug "connect: %a" Point.pp point >>= fun () ->
  Lwt_utils.protect ~canceler begin fun () ->
    Lwt_unix.connect fd uaddr >>= fun () ->
    return ()
  end ~on_error: begin fun err ->
    lwt_debug "connect: %a -> disconnect" Point.pp point >>= fun () ->
    Point_info.State.set_disconnected pi ;
    match err with
    | [Exn (Unix.Unix_error (Unix.ECONNREFUSED, _, _))] ->
        fail Connection_refused
    | err -> Lwt.return (Error err)
  end >>=? fun () ->
  lwt_debug "connect: %a -> authenticate" Point.pp point >>= fun () ->
  authenticate pool ~pi canceler fd point

type error += Too_many_connections

let connect ~timeout pool point =
  fail_unless
    (active_connections pool <= pool.config.max_connections)
    Too_many_connections >>=? fun () ->
  let canceler = Canceler.create () in
  Lwt_utils.with_timeout ~canceler timeout begin fun canceler ->
    raw_connect canceler pool point
  end

let accept pool fd point =
  if pool.config.max_incoming_connections <= Point.Table.length pool.incoming
  || pool.config.max_connections <= active_connections pool then
    Lwt.async (fun () -> Lwt_utils.safe_close fd)
  else
    let canceler = Canceler.create () in
    Point.Table.add pool.incoming point canceler ;
    Lwt.async begin fun () ->
      Lwt_utils.with_timeout
        ~canceler pool.config.authentification_timeout
        (fun canceler -> authenticate pool canceler fd point)
    end


(***************************************************************************)

let read { messages } =
  Lwt.catch
    (fun () -> Lwt_pipe.pop messages >>= fun ( _, msg) -> return msg)
    (fun _ (* Closed *) -> fail P2p_io_scheduler.Connection_closed)

let is_readable { messages } =
  Lwt.catch
    (fun () -> Lwt_pipe.values_available messages >>= return)
    (fun _ (* Closed *) -> fail P2p_io_scheduler.Connection_closed)

let write { conn } msg =
  P2p_connection.write conn (Message msg)

let write_sync { conn } msg =
  P2p_connection.write_sync conn (Message msg)

let write_now { conn } msg =
  P2p_connection.write_now conn (Message msg)

let write_all pool msg =
  Gid.Table.iter
    (fun _gid gi ->
       match Gid_info.State.get gi with
       | Running { data = conn } ->
           ignore (write_now conn msg : bool tzresult )
       | _ -> ())
    pool.connected_gids

let broadcast_bootstrap_msg pool =
  Gid.Table.iter
    (fun _gid gi ->
       match Gid_info.State.get gi with
       | Running { data = { conn } } ->
           ignore (P2p_connection.write_now conn Bootstrap : bool tzresult )
       | _ -> ())
    pool.connected_gids


(***************************************************************************)

module Gids = struct

  type ('msg, 'meta) info = (('msg, 'meta) connection, 'meta) Gid_info.t

  let info { known_gids } point =
    try Some (Gid.Table.find known_gids point)
    with Not_found -> None

  let get_metadata pool gid =
    try Some (Gid_info.metadata (Gid.Table.find pool.known_gids gid))
    with Not_found -> None

  let set_metadata pool gid data =
    Gid_info.set_metadata (register_peer pool gid) data

  let get_trusted pool gid =
    try Gid_info.trusted (Gid.Table.find pool.known_gids gid)
    with Not_found -> false

  let set_trusted pool gid =
    try Gid_info.set_trusted (register_peer pool gid)
    with Not_found -> ()

  let unset_trusted pool gid =
    try Gid_info.unset_trusted (Gid.Table.find pool.known_gids gid)
    with Not_found -> ()

  let find_connection pool gid =
    apply_option
      (info pool gid)
      ~f:(fun p ->
          match Gid_info.State.get p with
          | Running { data } -> Some data
          | _ -> None)

  let fold_known pool ~init ~f =
    Gid.Table.fold f pool.known_gids init
  let fold_connected pool ~init ~f =
    Gid.Table.fold f pool.connected_gids init

end

let fold_connections  pool ~init ~f =
  Gids.fold_connected pool ~init ~f:begin fun gid gi acc ->
    match Gid_info.State.get gi with
    | Running { data } -> f gid data acc
    | _ -> acc
  end

module Points = struct

  type ('msg, 'meta) info = ('msg, 'meta) connection Point_info.t

  let info { known_points } point =
    try Some (Point.Table.find known_points point)
    with Not_found -> None

  let get_trusted pool gid =
    try Point_info.trusted (Point.Table.find pool.known_points gid)
    with Not_found -> false

  let set_trusted pool gid =
    try Point_info.set_trusted (register_point pool gid)
    with Not_found -> ()

  let unset_trusted pool gid =
    try Point_info.unset_trusted (Point.Table.find pool.known_points gid)
    with Not_found -> ()

  let find_connection pool point =
    apply_option
      (info pool point)
      ~f:(fun p ->
          match Point_info.State.get p with
          | Running { data } -> Some data
          | _ -> None)

  let fold_known pool ~init ~f =
  Point.Table.fold f pool.known_points init

  let fold_connected  pool ~init ~f =
    Point.Table.fold f pool.connected_points init

end

module Events = struct
  let too_few_connections pool =
    Lwt_condition.wait pool.events.too_few_connections
  let too_many_connections pool =
    Lwt_condition.wait pool.events.too_many_connections
  let new_point pool =
    Lwt_condition.wait pool.events.new_point
end


let connection_stat { conn } =
  P2p_connection.stat conn

let pool_stat { io_sched } =
  P2p_io_scheduler.global_stat io_sched

let connection_info { conn } =
  P2p_connection.info conn

(***************************************************************************)

let create config meta_config message_config io_sched =
  let events = {
    too_few_connections = Lwt_condition.create () ;
    too_many_connections = Lwt_condition.create () ;
    new_point = Lwt_condition.create () ;
  } in
  let pool = {
    config ; meta_config ; message_config ;
    my_id_points = Point.Table.create 7 ;
    known_gids = Gid.Table.create 53 ;
    connected_gids = Gid.Table.create 53 ;
    known_points = Point.Table.create 53 ;
    connected_points = Point.Table.create 53 ;
    incoming = Point.Table.create 53 ;
    io_sched ;
    encoding = Message.encoding message_config.encoding ;
    events ;
  } in
  List.iter (Points.set_trusted pool) config.trusted_points ;
  Gid_info.File.load config.peers_file meta_config.encoding >>= function
  | Ok gids ->
      List.iter
        (fun gi -> Gid.Table.add pool.known_gids (Gid_info.gid gi) gi)
        gids ;
      Lwt.return pool
  | Error err ->
      log_error "@[Failed to parsed peers file:@ %a@]"
        pp_print_error err ;
      Lwt.return pool

let destroy pool =
  Point.Table.fold (fun _point pi acc ->
      match Point_info.State.get pi with
      | Requested { cancel } | Accepted { cancel } ->
          Canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn } ->
          disconnect conn >>= fun () -> acc
      | Disconnected ->  acc)
    pool.known_points @@
  Gid.Table.fold (fun _gid gi acc ->
      match Gid_info.State.get gi with
      | Accepted { cancel } ->
          Canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn } ->
          disconnect conn >>= fun () -> acc
      | Disconnected ->  acc)
    pool.known_gids @@
  Point.Table.fold (fun _point canceler acc ->
      Canceler.cancel canceler >>= fun () -> acc)
    pool.incoming Lwt.return_unit
