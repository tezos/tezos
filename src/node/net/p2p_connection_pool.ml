(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO Test cancelation of a (pending) connection *)

(* TODO do not recompute list_known_points at each requests...  but
        only once in a while, e.g. every minutes or when a point
        or the associated peer_id is blacklisted. *)

(* TODO allow to track "requested peer_ids" when we reconnect to a point. *)

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

module LogEvent = struct
  type t =
    | Too_few_connections
    | Too_many_connections
    | New_point of Point.t
    | New_peer of Peer_id.t
    | Incoming_connection of Point.t
    | Outgoing_connection of Point.t
    | Authentication_failed of Point.t
    | Accepting_request of Point.t * Id_point.t * Peer_id.t
    | Rejecting_request of Point.t * Id_point.t * Peer_id.t
    | Request_rejected of Point.t * (Id_point.t * Peer_id.t) option
    | Connection_established of Id_point.t * Peer_id.t
    | Disconnection of Peer_id.t
    | External_disconnection of Peer_id.t

    | Gc_points
    | Gc_peer_ids

  let encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv (fun x -> (), x) (fun ((), x) -> x)
        (merge_objs
           (obj1 (req "event" (constant name))) obj) in
    union ~tag_size:`Uint8 [
      case ~tag:0 (branch_encoding "too_few_connections" empty)
        (function Too_few_connections -> Some () | _ -> None)
        (fun () -> Too_few_connections) ;
      case ~tag:1 (branch_encoding "too_many_connections" empty)
        (function Too_many_connections -> Some () | _ -> None)
        (fun () -> Too_many_connections) ;
      case ~tag:2 (branch_encoding "new_point"
                     (obj1 (req "point" Point.encoding)))
        (function New_point p -> Some p | _ -> None)
        (fun p -> New_point p) ;
      case ~tag:3 (branch_encoding "new_peer"
                     (obj1 (req "peer_id" Peer_id.encoding)))
        (function New_peer p -> Some p | _ -> None)
        (fun p -> New_peer p) ;
      case ~tag:4 (branch_encoding "incoming_connection"
                     (obj1 (req "point" Point.encoding)))
        (function Incoming_connection p -> Some p | _ -> None)
        (fun p -> Incoming_connection p) ;
      case ~tag:5 (branch_encoding "outgoing_connection"
                     (obj1 (req "point" Point.encoding)))
        (function Outgoing_connection p -> Some p | _ -> None)
        (fun p -> Outgoing_connection p) ;
      case ~tag:6 (branch_encoding "authentication_failed"
                     (obj1 (req "point" Point.encoding)))
        (function Authentication_failed p -> Some p | _ -> None)
        (fun p -> Authentication_failed p) ;
      case ~tag:7 (branch_encoding "accepting_request"
                     (obj3
                        (req "point" Point.encoding)
                        (req "id_point" Id_point.encoding)
                        (req "peer_id" Peer_id.encoding)))
        (function Accepting_request (p, id_p, g) -> Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Accepting_request (p, id_p, g)) ;
      case ~tag:8 (branch_encoding "rejecting_request"
                     (obj3
                        (req "point" Point.encoding)
                        (req "id_point" Id_point.encoding)
                        (req "peer_id" Peer_id.encoding)))
        (function Rejecting_request (p, id_p, g) -> Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Rejecting_request (p, id_p, g)) ;
      case ~tag:9 (branch_encoding "request_rejected"
                     (obj2
                        (req "point" Point.encoding)
                        (opt "identity" (tup2 Id_point.encoding Peer_id.encoding))))
        (function Request_rejected (p, id) -> Some (p, id) | _ -> None)
        (fun (p, id) -> Request_rejected (p, id)) ;
      case ~tag:10 (branch_encoding "connection_established"
                      (obj2
                         (req "id_point" Id_point.encoding)
                         (req "peer_id" Peer_id.encoding)))
        (function Connection_established (id_p, g) -> Some (id_p, g) | _ -> None)
        (fun (id_p, g) -> Connection_established (id_p, g)) ;
      case ~tag:11 (branch_encoding "disconnection"
                      (obj1 (req "peer_id" Peer_id.encoding)))
        (function Disconnection g -> Some g | _ -> None)
        (fun g -> Disconnection g) ;
      case ~tag:12 (branch_encoding "external_disconnection"
                      (obj1 (req "peer_id" Peer_id.encoding)))
        (function External_disconnection g -> Some g | _ -> None)
        (fun g -> External_disconnection g) ;
      case ~tag:13 (branch_encoding "gc_points" empty)
        (function Gc_points -> Some () | _ -> None)
        (fun () -> Gc_points) ;
      case ~tag:14 (branch_encoding "gc_peer_ids" empty)
        (function Gc_peer_ids -> Some () | _ -> None)
        (fun () -> Gc_peer_ids) ;
    ]

  let log watcher event = Watcher.notify watcher event

  let too_few_connections watcher = log watcher Too_few_connections
  let too_many_connections watcher = log watcher Too_many_connections
  let new_point watcher ~point = log watcher (New_point point)
  let new_peer watcher ~peer_id = log watcher (New_peer peer_id)
  let incoming_connection watcher ~point = log watcher (Incoming_connection point)
  let outgoing_connection  watcher ~point = log watcher (Outgoing_connection point)
  let authentication_failed watcher ~point = log watcher (Authentication_failed point)
  let accepting_request watcher ~id_point ~point ~peer_id =
    log watcher (Accepting_request (point, id_point, peer_id))
  let rejecting_request watcher ~id_point ~point ~peer_id =
    log watcher (Rejecting_request (point, id_point, peer_id))
  let request_rejected watcher ?credentials ~point =
    log watcher (Request_rejected (point, credentials))
  let connection_established watcher ~id_point ~peer_id =
    log watcher (Connection_established (id_point, peer_id))
  let disconnection watcher ~is_external ~peer_id =
    log watcher (if is_external then External_disconnection peer_id
              else Disconnection peer_id)
  let gc_points watcher = log watcher Gc_points
  let gc_peer_ids watcher = log watcher Gc_peer_ids
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

  known_peer_ids_history_size : int ;
  known_points_history_size : int ;
  max_known_points : (int * int) option ; (* max, gc target *)
  max_known_peer_ids : (int * int) option ; (* max, gc target *)
}

type 'meta meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
  score : 'meta -> float;
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
  known_peer_ids : (('msg, 'meta) connection, 'meta) Peer_info.t Peer_id.Table.t ;
  connected_peer_ids : (('msg, 'meta) connection, 'meta) Peer_info.t Peer_id.Table.t ;
  known_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  connected_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  incoming : Canceler.t Point.Table.t ;
  io_sched : P2p_io_scheduler.t ;
  encoding : 'msg Message.t Data_encoding.t ;
  events : events ;
  watcher : LogEvent.t Watcher.input ;
}


and events = {
  too_few_connections : unit Lwt_condition.t ;
  too_many_connections : unit Lwt_condition.t ;
  new_peer : unit Lwt_condition.t ;
  new_connection : unit Lwt_condition.t ;
}

and ('msg, 'meta) connection = {
  canceler : Canceler.t ;
  messages : (int * 'msg) Lwt_pipe.t ;
  conn : 'msg Message.t P2p_connection.t ;
  peer_info : (('msg, 'meta) connection, 'meta) Peer_info.t ;
  point_info : ('msg, 'meta) connection Point_info.t option ;
  answerer : 'msg Answerer.t ;
  mutable wait_close : bool ;
}

type ('msg, 'meta) pool = ('msg, 'meta) t

module PoolEvent = struct
  let wait_too_few_connections pool =
    Lwt_condition.wait pool.events.too_few_connections
  let wait_too_many_connections pool =
    Lwt_condition.wait pool.events.too_many_connections
  let wait_new_peer pool =
    Lwt_condition.wait pool.events.new_peer
  let wait_new_connection pool =
    Lwt_condition.wait pool.events.new_connection
end

let watch { watcher } = Watcher.create_stream watcher

module GcPointSet = Utils.Bounded(struct
    type t = Time.t * Point.t
    let compare (x, _) (y, _) = - (Time.compare x y)
  end)

let gc_points ({ config = { max_known_points } ; known_points } as pool) =
  match max_known_points with
  | None -> ()
  | Some (_, target) ->
      let now = Time.now () in (* TODO: maybe time of discovery? *)
      let table = GcPointSet.create target in
      Point.Table.iter (fun p pi ->
          if Point_info.State.is_disconnected pi then
            let time =
              match Point_info.last_miss pi with
              | None -> now
              | Some t -> t in
            GcPointSet.insert (time, p) table
        ) known_points ;
      let to_remove = GcPointSet.get table in
      ListLabels.iter to_remove ~f:begin fun (_, p) ->
        Point.Table.remove known_points p
      end ;
      LogEvent.gc_points pool.watcher

let register_point pool ?trusted (addr, port as point) =
  match Point.Table.find pool.known_points point with
  | exception Not_found ->
      let pi = Point_info.create ?trusted addr port in
      iter_option pool.config.max_known_points ~f:begin fun (max, _) ->
        if Point.Table.length pool.known_points >= max then gc_points pool
      end ;
      Point.Table.add pool.known_points point pi ;
      LogEvent.new_point pool.watcher point ;
      pi
  | pi -> pi


(* Bounded table used to garbage collect peer_id infos when needed. The
   strategy used is to remove the info of the peer_id with the lowest
   score first. In case of equality, the info of the most recent added
   peer_id is removed. The rationale behind this choice is that in the
   case of a flood attack, the newly added infos will probably belong
   to peer_ids with the same (low) score and removing the most recent ones
   ensure that older (and probably legit) peer_id infos are kept. *)
module GcPeer_idSet = Utils.Bounded(struct
    type t = float * Time.t * Peer_id.t
    let compare (s, t, _) (s', t', _) =
      let score_cmp = Pervasives.compare s s' in
      if score_cmp = 0 then Time.compare t t' else - score_cmp
  end)

let gc_peer_ids ({ meta_config = { score } ;
              config = { max_known_peer_ids } ;
              known_peer_ids ; } as pool) =
  match max_known_peer_ids with
  | None -> ()
  | Some (_, target) ->
      let table = GcPeer_idSet.create target in
      Peer_id.Table.iter (fun peer_id peer_info ->
          let created = Peer_info.created peer_info in
          let score = score @@ Peer_info.metadata peer_info in
          GcPeer_idSet.insert (score, created, peer_id) table
        ) known_peer_ids ;
      let to_remove = GcPeer_idSet.get table in
      ListLabels.iter to_remove ~f:begin fun (_, _, peer_id) ->
        Peer_id.Table.remove known_peer_ids peer_id
      end ;
      LogEvent.gc_peer_ids pool.watcher

let register_peer pool peer_id =
  match Peer_id.Table.find pool.known_peer_ids peer_id with
  | exception Not_found ->
      Lwt_condition.broadcast pool.events.new_peer () ;
      let peer = Peer_info.create peer_id ~metadata:pool.meta_config.initial in
      iter_option pool.config.max_known_peer_ids ~f:begin fun (max, _) ->
        if Peer_id.Table.length pool.known_peer_ids >= max then gc_peer_ids pool
      end ;
      Peer_id.Table.add pool.known_peer_ids peer_id peer ;
      LogEvent.new_peer pool.watcher peer_id ;
      peer
  | peer -> peer

let register_new_point pool _peer_id point =
  if not (Point.Table.mem pool.my_id_points point) then
    ignore (register_point pool point)

let register_new_points pool peer_id points =
  List.iter (register_new_point pool peer_id) points ;
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

let list_known_points pool _peer_id () =
  let knowns =
    Point.Table.fold (fun _ pi acc -> pi :: acc) pool.known_points [] in
  let best_knowns =
    Utils.take_n ~compare:compare_known_point_info 50 knowns in
  Lwt.return (List.map Point_info.point best_knowns)

let active_connections pool = Peer_id.Table.length pool.connected_peer_ids

let create_connection pool conn id_point pi gi _version =
  let peer_id = Peer_info.peer_id gi in
  let canceler = Canceler.create () in
  let size =
    map_option pool.config.incoming_app_message_queue_size
      ~f:(fun qs -> qs, fun (size, _) -> (Sys.word_size / 8) * 11 + size) in
  let messages = Lwt_pipe.create ?size () in
  let callback =
    { Answerer.message =
        (fun size msg -> Lwt_pipe.push messages (size, msg)) ;
      advertise = register_new_points pool peer_id ;
      bootstrap = list_known_points pool peer_id ;
    } in
  let answerer = Answerer.run conn canceler callback in
  let conn =
    { conn ; point_info = pi ; peer_info = gi ;
      messages ; canceler ; answerer ; wait_close = false } in
  iter_option pi ~f:begin fun pi ->
    let point = Point_info.point pi in
    Point_info.State.set_running pi peer_id conn ;
    Point.Table.add pool.connected_points point pi ;
  end ;
  LogEvent.connection_established pool.watcher ~id_point ~peer_id ;
  Peer_info.State.set_running gi id_point conn ;
  Peer_id.Table.add pool.connected_peer_ids peer_id gi ;
  Lwt_condition.broadcast pool.events.new_connection () ;
  Canceler.on_cancel canceler begin fun () ->
    lwt_debug "Disconnect: %a (%a)"
      Peer_id.pp peer_id Id_point.pp id_point >>= fun () ->
    iter_option ~f:Point_info.State.set_disconnected pi;
    LogEvent.disconnection pool.watcher ~is_external:false ~peer_id ;
    Peer_info.State.set_disconnected gi ;
    iter_option pi ~f:begin fun pi ->
      Point.Table.remove pool.connected_points (Point_info.point pi) ;
    end ;
    Peer_id.Table.remove pool.connected_peer_ids peer_id ;
    if pool.config.max_connections <= active_connections pool then begin
      Lwt_condition.broadcast pool.events.too_many_connections () ;
      LogEvent.too_many_connections pool.watcher ;
    end ;
    P2p_connection.close ~wait:conn.wait_close conn.conn
  end ;
  if active_connections pool < pool.config.min_connections then begin
    Lwt_condition.broadcast pool.events.too_few_connections () ;
    LogEvent.too_few_connections pool.watcher ;
  end ;
  conn

let disconnect ?(wait = false) conn =
  conn.wait_close <- wait ;
  Canceler.cancel conn.canceler >>= fun () ->
  conn.answerer.worker

type error += Rejected of Peer_id.t
type error += Unexpected_point_state
type error += Unexpected_peer_id_state

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
    (* Authentication incorrect! *)
    (* TODO do something when the error is Not_enough_proof_of_work ?? *)
    lwt_debug "authenticate: %a%s -> failed %a"
      Point.pp point
      (if incoming then " incoming" else "")
      pp_print_error err >>= fun () ->
    may_register_my_id_point pool err ;
    LogEvent.authentication_failed pool.watcher ~point ;
    if incoming then
      Point.Table.remove pool.incoming point
    else
      iter_option Point_info.State.set_disconnected pi ;
    Lwt.return (Error err)
  end >>=? fun (info, auth_fd) ->
  (* Authentication correct! *)
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
  let gi = register_peer pool info.peer_id in
  let acceptable_versions =
    Version.common info.versions pool.message_config.versions
  in
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
  let acceptable_peer_id =
    match Peer_info.State.get gi with
    | Accepted _ ->
        (* TODO: in some circumstances cancel and accept... *)
        false
    | Running _ -> false
    | Disconnected -> true
  in
  if incoming then
    Point.Table.remove pool.incoming point ;
  match acceptable_versions with
  | Some version when acceptable_peer_id && acceptable_point -> begin
      LogEvent.accepting_request pool.watcher
        ~id_point:info.id_point ~point ~peer_id:info.peer_id ;
      iter_option connection_pi
        ~f:(fun pi -> Point_info.State.set_accepted pi info.peer_id canceler) ;
      Peer_info.State.set_accepted gi info.id_point canceler ;
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
        if incoming then
          LogEvent.request_rejected pool.watcher
            ~credentials:(info.id_point, info.peer_id) ~point ;
        lwt_debug "authenticate: %a -> rejected %a"
          Point.pp point
          Connection_info.pp info >>= fun () ->
        iter_option connection_pi ~f:Point_info.State.set_disconnected;
        Peer_info.State.set_disconnected gi ;
        Lwt.return (Error err)
      end >>=? fun conn ->
      let id_point =
        match info.id_point, map_option Point_info.point pi with
        | (addr, _), Some (_, port) -> addr, Some port
        | id_point, None ->  id_point in
      return (create_connection pool conn id_point connection_pi gi version)
    end
  | _ -> begin
      LogEvent.rejecting_request pool.watcher
        ~id_point:info.id_point ~point ~peer_id:info.peer_id ;
      lwt_debug "authenticate: %a -> kick %a point: %B peer_id: %B"
        Point.pp point
        Connection_info.pp info
        acceptable_point acceptable_peer_id >>= fun () ->
      P2p_connection.kick auth_fd >>= fun () ->
      if not incoming then begin
        iter_option ~f:Point_info.State.set_disconnected pi ;
        (* FIXME Peer_info.State.set_disconnected ~requested:true gi ; *)
      end ;
      fail (Rejected info.peer_id)
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

let fail_unless_disconnected_peer_id gi =
  match Peer_info.State.get gi with
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
    LogEvent.outgoing_connection pool.watcher ~point ;
    Lwt_unix.connect fd uaddr >>= fun () ->
    return ()
  end ~on_error: begin fun err ->
    lwt_debug "connect: %a -> disconnect" Point.pp point >>= fun () ->
    Point_info.State.set_disconnected pi ;
    Lwt_utils.safe_close fd >>= fun () ->
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
  LogEvent.incoming_connection pool.watcher ~point ;
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

let read { messages ; conn } =
  Lwt.catch
    (fun () -> Lwt_pipe.pop messages >>= fun (s, msg) ->
      lwt_debug "%d bytes message popped from queue %a\027[0m"
        s Connection_info.pp (P2p_connection.info conn) >>= fun () ->
      return msg)
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
  Peer_id.Table.iter
    (fun _peer_id gi ->
       match Peer_info.State.get gi with
       | Running { data = conn } ->
           ignore (write_now conn msg : bool tzresult )
       | _ -> ())
    pool.connected_peer_ids

let broadcast_bootstrap_msg pool =
  Peer_id.Table.iter
    (fun _peer_id gi ->
       match Peer_info.State.get gi with
       | Running { data = { conn } } ->
           ignore (P2p_connection.write_now conn Bootstrap : bool tzresult )
       | _ -> ())
    pool.connected_peer_ids


(***************************************************************************)

module Peer_ids = struct

  type ('msg, 'meta) info = (('msg, 'meta) connection, 'meta) Peer_info.t

  let info { known_peer_ids } point =
    try Some (Peer_id.Table.find known_peer_ids point)
    with Not_found -> None

  let get_metadata pool peer_id =
    try Some (Peer_info.metadata (Peer_id.Table.find pool.known_peer_ids peer_id))
    with Not_found -> None

  let get_score pool peer_id =
    try Some (pool.meta_config.score @@ Peer_info.metadata (Peer_id.Table.find pool.known_peer_ids peer_id))
    with Not_found -> None

  let set_metadata pool peer_id data =
    Peer_info.set_metadata (register_peer pool peer_id) data

  let get_trusted pool peer_id =
    try Peer_info.trusted (Peer_id.Table.find pool.known_peer_ids peer_id)
    with Not_found -> false

  let set_trusted pool peer_id =
    try Peer_info.set_trusted (register_peer pool peer_id)
    with Not_found -> ()

  let unset_trusted pool peer_id =
    try Peer_info.unset_trusted (Peer_id.Table.find pool.known_peer_ids peer_id)
    with Not_found -> ()

  let find_connection pool peer_id =
    apply_option
      (info pool peer_id)
      ~f:(fun p ->
          match Peer_info.State.get p with
          | Running { data } -> Some data
          | _ -> None)

  let fold_known pool ~init ~f =
    Peer_id.Table.fold f pool.known_peer_ids init
  let fold_connected pool ~init ~f =
    Peer_id.Table.fold f pool.connected_peer_ids init

end

let fold_connections  pool ~init ~f =
  Peer_ids.fold_connected pool ~init ~f:begin fun peer_id gi acc ->
    match Peer_info.State.get gi with
    | Running { data } -> f peer_id data acc
    | _ -> acc
  end

module Points = struct

  type ('msg, 'meta) info = ('msg, 'meta) connection Point_info.t

  let info { known_points } point =
    try Some (Point.Table.find known_points point)
    with Not_found -> None

  let get_trusted pool peer_id =
    try Point_info.trusted (Point.Table.find pool.known_points peer_id)
    with Not_found -> false

  let set_trusted pool peer_id =
    try Point_info.set_trusted (register_point pool peer_id)
    with Not_found -> ()

  let unset_trusted pool peer_id =
    try Point_info.unset_trusted (Point.Table.find pool.known_points peer_id)
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

let connection_stat { conn } =
  P2p_connection.stat conn

let pool_stat { io_sched } =
  P2p_io_scheduler.global_stat io_sched

let score { meta_config = { score }} meta = score meta

let connection_info { conn } =
  P2p_connection.info conn

(***************************************************************************)

let create config meta_config message_config io_sched =
  let events = {
    too_few_connections = Lwt_condition.create () ;
    too_many_connections = Lwt_condition.create () ;
    new_peer = Lwt_condition.create () ;
    new_connection = Lwt_condition.create () ;
  } in
  let pool = {
    config ; meta_config ; message_config ;
    my_id_points = Point.Table.create 7 ;
    known_peer_ids = Peer_id.Table.create 53 ;
    connected_peer_ids = Peer_id.Table.create 53 ;
    known_points = Point.Table.create 53 ;
    connected_points = Point.Table.create 53 ;
    incoming = Point.Table.create 53 ;
    io_sched ;
    encoding = Message.encoding message_config.encoding ;
    events ;
    watcher = Watcher.create_input () ;
  } in
  List.iter (Points.set_trusted pool) config.trusted_points ;
  Peer_info.File.load config.peers_file meta_config.encoding >>= function
  | Ok peer_ids ->
      List.iter
        (fun gi -> Peer_id.Table.add pool.known_peer_ids (Peer_info.peer_id gi) gi)
        peer_ids ;
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
  Peer_id.Table.fold (fun _peer_id gi acc ->
      match Peer_info.State.get gi with
      | Accepted { cancel } ->
          Canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn } ->
          disconnect conn >>= fun () -> acc
      | Disconnected ->  acc)
    pool.known_peer_ids @@
  Point.Table.fold (fun _point canceler acc ->
      Canceler.cancel canceler >>= fun () -> acc)
    pool.incoming Lwt.return_unit
