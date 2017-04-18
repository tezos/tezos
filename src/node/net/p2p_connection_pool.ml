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
    | Swap_request of Point.t * Peer_id.t
    | Swap_ack of Point.t * Peer_id.t
    | Message of 'msg
    | Disconnect

  let encoding msg_encoding =
    let open Data_encoding in
    dynamic_size @@
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
         case ~tag:0x04 (tup2 Point.encoding Peer_id.encoding)
           (function
             | Swap_request (point, peer_id) -> Some (point, peer_id)
             | _ -> None)
           (fun (point, peer_id) -> Swap_request (point, peer_id)) ;
         case ~tag:0x05 (tup2 Point.encoding Peer_id.encoding)
           (function
             | Swap_ack (point, peer_id) -> Some (point, peer_id)
             | _ -> None)
           (fun (point, peer_id) -> Swap_ack (point, peer_id)) ;
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
    swap_request: Point.t -> Peer_id.t -> unit Lwt.t ;
    swap_ack: Point.t -> Peer_id.t -> unit Lwt.t ;
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
    | Ok (_, Swap_request (point, peer)) ->
        st.callback.swap_request point peer >>= fun () ->
        worker_loop st
    | Ok (_, Swap_ack (point, peer)) ->
        st.callback.swap_ack point peer >>= fun () ->
        worker_loop st
    | Ok (size, Message msg) ->
        st.callback.message size msg >>= fun () ->
        worker_loop st
    | Ok (_, Disconnect) | Error [P2p_io_scheduler.Connection_closed] ->
        Canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [P2p_connection.Decoding_error] ->
        (* TODO: Penalize peer... *)
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

module Log_event = struct

  type t =

    | Too_few_connections
    | Too_many_connections

    | New_point of Point.t
    | New_peer of Peer_id.t

    | Gc_points
    | Gc_peer_ids

    | Incoming_connection of Point.t
    | Outgoing_connection of Point.t
    | Authentication_failed of Point.t
    | Accepting_request of Point.t * Id_point.t * Peer_id.t
    | Rejecting_request of Point.t * Id_point.t * Peer_id.t
    | Request_rejected of Point.t * (Id_point.t * Peer_id.t) option
    | Connection_established of Id_point.t * Peer_id.t

    | Swap_request_received of { source : Peer_id.t }
    | Swap_ack_received of { source : Peer_id.t }
    | Swap_request_sent of { source : Peer_id.t }
    | Swap_ack_sent of { source : Peer_id.t }
    | Swap_request_ignored of { source : Peer_id.t }
    | Swap_success of { source : Peer_id.t }
    | Swap_failure of { source : Peer_id.t }

    | Disconnection of Peer_id.t
    | External_disconnection of Peer_id.t

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
        (function Accepting_request (p, id_p, g) ->
           Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Accepting_request (p, id_p, g)) ;
      case ~tag:8 (branch_encoding "rejecting_request"
                     (obj3
                        (req "point" Point.encoding)
                        (req "id_point" Id_point.encoding)
                        (req "peer_id" Peer_id.encoding)))
        (function Rejecting_request (p, id_p, g) ->
           Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Rejecting_request (p, id_p, g)) ;
      case ~tag:9 (branch_encoding "request_rejected"
                     (obj2
                        (req "point" Point.encoding)
                        (opt "identity"
                           (tup2 Id_point.encoding Peer_id.encoding))))
        (function Request_rejected (p, id) -> Some (p, id) | _ -> None)
        (fun (p, id) -> Request_rejected (p, id)) ;
      case ~tag:10 (branch_encoding "connection_established"
                      (obj2
                         (req "id_point" Id_point.encoding)
                         (req "peer_id" Peer_id.encoding)))
        (function Connection_established (id_p, g) ->
           Some (id_p, g) | _ -> None)
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
      case ~tag:15 (branch_encoding "swap_request_received"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_request_received { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_received { source }) ;
      case ~tag:16 (branch_encoding "swap_ack_received"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_ack_received { source } -> Some source
          | _ -> None)
        (fun source -> Swap_ack_received { source }) ;
      case ~tag:17 (branch_encoding "swap_request_sent"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_request_sent { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_sent { source }) ;
      case ~tag:18 (branch_encoding "swap_ack_sent"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_ack_sent { source } -> Some source
          | _ -> None)
        (fun source -> Swap_ack_sent { source }) ;
      case ~tag:19 (branch_encoding "swap_request_ignored"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_request_ignored { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_ignored { source }) ;
      case ~tag:20 (branch_encoding "swap_success"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_success { source } -> Some source
          | _ -> None)
        (fun source -> Swap_success { source }) ;
      case ~tag:21 (branch_encoding "swap_failure"
                      (obj1 (req "source" Peer_id.encoding)))
        (function
          | Swap_failure { source } -> Some source
          | _ -> None)
        (fun source -> Swap_failure { source }) ;
    ]

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

  swap_linger : float ;

  binary_chunks_size : int option ;
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
  known_peer_ids :
    (('msg, 'meta) connection, 'meta) Peer_info.t Peer_id.Table.t ;
  connected_peer_ids :
    (('msg, 'meta) connection, 'meta) Peer_info.t Peer_id.Table.t ;
  known_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  connected_points : ('msg, 'meta) connection Point_info.t Point.Table.t ;
  incoming : Canceler.t Point.Table.t ;
  io_sched : P2p_io_scheduler.t ;
  encoding : 'msg Message.t Data_encoding.t ;
  events : events ;
  watcher : Log_event.t Watcher.input ;
  mutable new_connection_hook :
    (Peer_id.t -> ('msg, 'meta) connection -> unit) list ;
  mutable latest_accepted_swap : Time.t ;
  mutable latest_succesfull_swap : Time.t  ;
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
  answerer : 'msg Answerer.t Lazy.t ;
  mutable last_sent_swap_request : (Time.t * Peer_id.t) option ;
  mutable wait_close : bool ;
}

type ('msg, 'meta) pool = ('msg, 'meta) t

module Pool_event = struct
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
let log { watcher } event = Watcher.notify watcher event

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
      Point.Table.iter (fun p point_info ->
          if Point_info.State.is_disconnected point_info then
            let time =
              match Point_info.last_miss point_info with
              | None -> now
              | Some t -> t in
            GcPointSet.insert (time, p) table
        ) known_points ;
      let to_remove = GcPointSet.get table in
      ListLabels.iter to_remove ~f:begin fun (_, p) ->
        Point.Table.remove known_points p
      end ;
      log pool Gc_points

let register_point pool ?trusted _source_peer_id (addr, port as point) =
  match Point.Table.find pool.known_points point with
  | exception Not_found ->
      let point_info = Point_info.create ?trusted addr port in
      iter_option pool.config.max_known_points ~f:begin fun (max, _) ->
        if Point.Table.length pool.known_points >= max then gc_points pool
      end ;
      Point.Table.add pool.known_points point point_info ;
      log pool (New_point point) ;
      point_info
  | point_info -> point_info

let may_register_my_id_point pool = function
  | [P2p_connection.Myself (addr, Some port)] ->
      Point.Table.add pool.my_id_points (addr, port) () ;
      Point.Table.remove pool.known_points (addr, port)
  | _ -> ()


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
      log pool Gc_peer_ids

let register_peer pool peer_id =
  match Peer_id.Table.find pool.known_peer_ids peer_id with
  | exception Not_found ->
      Lwt_condition.broadcast pool.events.new_peer () ;
      let peer = Peer_info.create peer_id ~metadata:pool.meta_config.initial in
      iter_option pool.config.max_known_peer_ids ~f:begin fun (max, _) ->
        if Peer_id.Table.length pool.known_peer_ids >= max then gc_peer_ids pool
      end ;
      Peer_id.Table.add pool.known_peer_ids peer_id peer ;
      log pool (New_peer peer_id) ;
      peer
  | peer -> peer


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

let raw_write_sync { conn } buf =
  P2p_connection.raw_write_sync conn buf

let write_now { conn } msg =
  P2p_connection.write_now conn (Message msg)

let write_all pool msg =
  Peer_id.Table.iter
    (fun _peer_id peer_info ->
       match Peer_info.State.get peer_info with
       | Running { data = conn } ->
           ignore (write_now conn msg : bool tzresult )
       | _ -> ())
    pool.connected_peer_ids

let broadcast_bootstrap_msg pool =
  Peer_id.Table.iter
    (fun _peer_id peer_info ->
       match Peer_info.State.get peer_info with
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
    try Peer_info.metadata (Peer_id.Table.find pool.known_peer_ids peer_id)
    with Not_found -> pool.meta_config.initial

  let get_score pool peer_id =
    pool.meta_config.score (get_metadata pool peer_id)

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

  let fold_known pool ~init ~f =
    Peer_id.Table.fold f pool.known_peer_ids init

  let fold_connected pool ~init ~f =
    Peer_id.Table.fold f pool.connected_peer_ids init

end

module Points = struct

  type ('msg, 'meta) info = ('msg, 'meta) connection Point_info.t

  let info { known_points } point =
    try Some (Point.Table.find known_points point)
    with Not_found -> None

  let get_trusted pool point =
    try Point_info.trusted (Point.Table.find pool.known_points point)
    with Not_found -> false

  let set_trusted pool point =
    try
      Point_info.set_trusted
        (register_point pool pool.config.identity.peer_id point)
    with Not_found -> ()

  let unset_trusted pool peer_id =
    try Point_info.unset_trusted (Point.Table.find pool.known_points peer_id)
    with Not_found -> ()

  let fold_known pool ~init ~f =
    Point.Table.fold f pool.known_points init

  let fold_connected  pool ~init ~f =
    Point.Table.fold f pool.connected_points init

end

module Connection = struct

  let fold pool ~init ~f =
    Peer_ids.fold_connected pool ~init ~f:begin fun peer_id peer_info acc ->
      match Peer_info.State.get peer_info with
      | Running { data } -> f peer_id data acc
      | _ -> acc
    end

  let list pool =
    fold pool ~init:[] ~f:(fun peer_id c acc -> (peer_id, c) :: acc)

  let random ?different_than pool =
    let candidates =
      fold pool ~init:[] ~f:begin fun _peer conn acc ->
        match different_than with
        | Some excluded_conn
          when P2p_connection.equal conn.conn excluded_conn.conn -> acc
        | Some _ | None -> conn :: acc
      end in
    match candidates with
    | [] -> None
    | _ :: _ ->
        Some (List.nth candidates (Random.int @@ List.length candidates))

  let random_lowid ?different_than pool =
    let candidates =
      fold pool ~init:[] ~f:begin fun _peer conn acc ->
        match different_than with
        | Some excluded_conn
          when P2p_connection.equal conn.conn excluded_conn.conn -> acc
        | Some _ | None ->
            let ci = P2p_connection.info conn.conn in
            match ci.id_point with
            | _, None -> acc
            | addr, Some port -> ((addr, port), ci.peer_id, conn) :: acc
      end in
    match candidates with
    | [] -> None
    | _ :: _ ->
        Some (List.nth candidates (Random.int @@ List.length candidates))

  let stat { conn } =
    P2p_connection.stat conn

  let score { meta_config = { score }} meta = score meta

  let info { conn } =
    P2p_connection.info conn

  let find_by_peer_id pool peer_id =
    apply_option
      (Peer_ids.info pool peer_id)
      ~f:(fun p ->
          match Peer_info.State.get p with
          | Running { data } -> Some data
          | _ -> None)

  let find_by_point pool point =
    apply_option
      (Points.info pool point)
      ~f:(fun p ->
          match Point_info.State.get p with
          | Running { data } -> Some data
          | _ -> None)

end

let pool_stat { io_sched } =
  P2p_io_scheduler.global_stat io_sched


(***************************************************************************)

type error += Rejected of Peer_id.t
type error += Unexpected_point_state
type error += Unexpected_peer_id_state
type error += Pending_connection
type error += Connected
type error += Connection_closed = P2p_io_scheduler.Connection_closed
type error += Connection_refused
type error += Closed_network
type error += Too_many_connections

let fail_unless_disconnected_point point_info =
  match Point_info.State.get point_info with
  | Disconnected -> return ()
  | Requested _ | Accepted _ -> fail Pending_connection
  | Running _ -> fail Connected

let fail_unless_disconnected_peer_id peer_info =
  match Peer_info.State.get peer_info with
  | Disconnected -> return ()
  | Accepted _ -> fail Pending_connection
  | Running _ -> fail Connected

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

let rec connect ~timeout pool point =
  fail_unless
    (active_connections pool <= pool.config.max_connections)
    Too_many_connections >>=? fun () ->
  let canceler = Canceler.create () in
  Lwt_utils.with_timeout ~canceler timeout begin fun canceler ->
    let point_info =
      register_point pool pool.config.identity.peer_id point in
    let addr, port as point = Point_info.point point_info in
    fail_unless
      (not pool.config.closed_network || Point_info.trusted point_info)
      Closed_network >>=? fun () ->
    fail_unless_disconnected_point point_info >>=? fun () ->
    Point_info.State.set_requested point_info canceler ;
    let fd = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
    let uaddr =
      Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    lwt_debug "connect: %a" Point.pp point >>= fun () ->
    Lwt_utils.protect ~canceler begin fun () ->
      log pool (Outgoing_connection point) ;
      Lwt_unix.connect fd uaddr >>= fun () ->
      return ()
    end ~on_error: begin fun err ->
      lwt_debug "connect: %a -> disconnect" Point.pp point >>= fun () ->
      Point_info.State.set_disconnected point_info ;
      Lwt_utils.safe_close fd >>= fun () ->
      match err with
      | [Exn (Unix.Unix_error (Unix.ECONNREFUSED, _, _))] ->
          fail Connection_refused
      | err -> Lwt.return (Error err)
    end >>=? fun () ->
    lwt_debug "connect: %a -> authenticate" Point.pp point >>= fun () ->
    authenticate pool ~point_info canceler fd point
  end

and authenticate pool ?point_info canceler fd point =
  let incoming = point_info = None in
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
    lwt_debug "@[authenticate: %a%s -> failed@ %a@]"
      Point.pp point
      (if incoming then " incoming" else "")
      pp_print_error err >>= fun () ->
    may_register_my_id_point pool err ;
    log pool (Authentication_failed point) ;
    if incoming then
      Point.Table.remove pool.incoming point
    else
      iter_option Point_info.State.set_disconnected point_info ;
    Lwt.return (Error err)
  end >>=? fun (info, auth_fd) ->
  (* Authentication correct! *)
  lwt_debug "authenticate: %a -> auth %a"
    Point.pp point
    Connection_info.pp info >>= fun () ->
  let remote_point_info =
    match info.id_point with
    | addr, Some port
      when not (Point.Table.mem pool.my_id_points (addr, port)) ->
        Some (register_point pool info.peer_id (addr, port))
    | _ -> None in
  let connection_point_info =
    match point_info, remote_point_info with
    | None, None -> None
    | Some _ as point_info, _ | _, (Some _ as point_info) -> point_info in
  let peer_info = register_peer pool info.peer_id in
  let acceptable_versions =
    Version.common info.versions pool.message_config.versions
  in
  let acceptable_point =
    unopt_map connection_point_info
      ~default:(not pool.config.closed_network)
      ~f:begin fun connection_point_info ->
      match Point_info.State.get connection_point_info with
      | Requested _ -> not incoming
      | Disconnected ->
          not pool.config.closed_network
          || Point_info.trusted connection_point_info
      | Accepted _ | Running _ -> false
    end
  in
  let acceptable_peer_id =
    match Peer_info.State.get peer_info with
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
      log pool (Accepting_request (point, info.id_point, info.peer_id)) ;
      iter_option connection_point_info
        ~f:(fun point_info ->
            Point_info.State.set_accepted point_info info.peer_id canceler) ;
      Peer_info.State.set_accepted peer_info info.id_point canceler ;
      lwt_debug "authenticate: %a -> accept %a"
        Point.pp point
        Connection_info.pp info >>= fun () ->
      Lwt_utils.protect ~canceler begin fun () ->
        P2p_connection.accept
          ?incoming_message_queue_size:pool.config.incoming_message_queue_size
          ?outgoing_message_queue_size:pool.config.outgoing_message_queue_size
          ?binary_chunks_size:pool.config.binary_chunks_size
          auth_fd pool.encoding >>= fun conn ->
        lwt_debug "authenticate: %a -> Connected %a"
          Point.pp point
          Connection_info.pp info >>= fun () ->
        Lwt.return conn
      end ~on_error: begin fun err ->
        if incoming then
          log pool
            (Request_rejected (point, Some (info.id_point, info.peer_id))) ;
        lwt_debug "authenticate: %a -> rejected %a"
          Point.pp point
          Connection_info.pp info >>= fun () ->
        iter_option connection_point_info
          ~f:Point_info.State.set_disconnected ;
        Peer_info.State.set_disconnected peer_info ;
        Lwt.return (Error err)
      end >>=? fun conn ->
      let id_point =
        match info.id_point, map_option Point_info.point point_info with
        | (addr, _), Some (_, port) -> addr, Some port
        | id_point, None ->  id_point in
      return
        (create_connection
           pool conn
           id_point connection_point_info peer_info version)
    end
  | _ -> begin
      log pool (Rejecting_request (point, info.id_point, info.peer_id)) ;
      lwt_debug "authenticate: %a -> kick %a point: %B peer_id: %B"
        Point.pp point
        Connection_info.pp info
        acceptable_point acceptable_peer_id >>= fun () ->
      P2p_connection.kick auth_fd >>= fun () ->
      if not incoming then begin
        iter_option ~f:Point_info.State.set_disconnected point_info ;
        (* FIXME Peer_info.State.set_disconnected ~requested:true peer_info ; *)
      end ;
      fail (Rejected info.peer_id)
    end

and create_connection pool p2p_conn id_point point_info peer_info _version =
  let peer_id = Peer_info.peer_id peer_info in
  let canceler = Canceler.create () in
  let size =
    map_option pool.config.incoming_app_message_queue_size
      ~f:(fun qs -> qs, fun (size, _) -> (Sys.word_size / 8) * 11 + size) in
  let messages = Lwt_pipe.create ?size () in
  let rec callback =
    { Answerer.message =
        (fun size msg -> Lwt_pipe.push messages (size, msg)) ;
      advertise =
        (fun points -> register_new_points pool conn points ) ;
      bootstrap =
        (fun () -> list_known_points pool conn () ) ;
      swap_request =
        (fun point peer_id -> swap_request pool conn point peer_id ) ;
      swap_ack =
        (fun point peer_id -> swap_ack pool conn point peer_id ) ;
    }
  and answerer = lazy (Answerer.run p2p_conn canceler callback)
  and conn =
    { conn = p2p_conn ; point_info ; peer_info ;
      messages ; canceler ; answerer ; wait_close = false ;
      last_sent_swap_request = None } in
  ignore (Lazy.force answerer) ;
  iter_option point_info ~f:begin fun point_info ->
    let point = Point_info.point point_info in
    Point_info.State.set_running point_info peer_id conn ;
    Point.Table.add pool.connected_points point point_info ;
  end ;
  log pool (Connection_established (id_point, peer_id)) ;
  Peer_info.State.set_running peer_info id_point conn ;
  Peer_id.Table.add pool.connected_peer_ids peer_id peer_info ;
  Lwt_condition.broadcast pool.events.new_connection () ;
  Canceler.on_cancel canceler begin fun () ->
    lwt_debug "Disconnect: %a (%a)"
      Peer_id.pp peer_id Id_point.pp id_point >>= fun () ->
    iter_option ~f:Point_info.State.set_disconnected point_info ;
    log pool (Disconnection peer_id) ;
    Peer_info.State.set_disconnected peer_info ;
    iter_option point_info ~f:begin fun point_info ->
      Point.Table.remove pool.connected_points (Point_info.point point_info) ;
    end ;
    Peer_id.Table.remove pool.connected_peer_ids peer_id ;
    if pool.config.max_connections <= active_connections pool then begin
      Lwt_condition.broadcast pool.events.too_many_connections () ;
      log pool Too_many_connections ;
    end ;
    Lwt_pipe.close messages ;
    P2p_connection.close ~wait:conn.wait_close conn.conn
  end ;
  List.iter (fun f -> f peer_id conn) pool.new_connection_hook ;
  if active_connections pool < pool.config.min_connections then begin
    Lwt_condition.broadcast pool.events.too_few_connections () ;
    log pool Too_few_connections ;
  end ;
  conn

and disconnect ?(wait = false) conn =
  conn.wait_close <- wait ;
  Answerer.shutdown (Lazy.force conn.answerer)

and register_new_points pool conn =
  let source_peer_id = Peer_info.peer_id conn.peer_info in
  fun points ->
    List.iter (register_new_point pool source_peer_id) points ;
    Lwt.return_unit

and register_new_point pool _source_peer_id point =
  if not (Point.Table.mem pool.my_id_points point) then
    ignore (register_point pool _source_peer_id point)

and list_known_points pool _conn () =
  let knowns =
    Point.Table.fold
      (fun _ point_info acc -> point_info :: acc)
      pool.known_points [] in
  let best_knowns =
    Utils.take_n ~compare:compare_known_point_info 50 knowns in
  Lwt.return (List.map Point_info.point best_knowns)

and active_connections pool = Peer_id.Table.length pool.connected_peer_ids

and swap_request pool conn new_point _new_peer_id =
  let source_peer_id = Peer_info.peer_id conn.peer_info in
  log pool (Swap_request_received { source = source_peer_id }) ;
  lwt_log_info
    "Swap request received from %a" Peer_id.pp source_peer_id >>= fun () ->
  (* Ignore if already connected to peer or already swapped less
     than <swap_linger> seconds ago. *)
  let now = Time.now () in
  let span_since_last_swap =
    Int64.to_int @@
    Time.diff now
      (Time.max pool.latest_succesfull_swap pool.latest_accepted_swap) in
  let new_point_info = register_point pool source_peer_id new_point in
  if span_since_last_swap < int_of_float pool.config.swap_linger
     || not (Point_info.State.is_disconnected new_point_info) then begin
    log pool (Swap_request_ignored { source = source_peer_id }) ;
    lwt_log_info "Ignoring swap request from %a" Peer_id.pp source_peer_id
  end else begin
    match Connection.random_lowid pool with
    | None ->
        lwt_log_info
          "No swap candidate for %a" Peer_id.pp source_peer_id
    | Some (proposed_point, proposed_peer_id, _proposed_conn) ->
        match P2p_connection.write_now
                conn.conn (Swap_ack (proposed_point, proposed_peer_id)) with
        | Ok true ->
            log pool (Swap_ack_sent { source = source_peer_id }) ;
            swap pool conn proposed_peer_id new_point >>= fun () ->
            Lwt.return_unit
        | Ok false ->
            log pool (Swap_request_received { source = source_peer_id }) ;
            Lwt.return_unit
        | Error _ ->
            log pool (Swap_request_received { source = source_peer_id }) ;
            Lwt.return_unit
  end

and swap_ack pool conn new_point _new_peer_id =
  let source_peer_id = Peer_info.peer_id conn.peer_info in
  log pool (Swap_ack_received { source = source_peer_id }) ;
  lwt_log_info
    "Swap ack received from %a" Peer_id.pp source_peer_id >>= fun () ->
  match conn.last_sent_swap_request with
  | None -> Lwt.return_unit (* ignore *)
  | Some (_time, proposed_peer_id) ->
      match Connection.find_by_peer_id pool proposed_peer_id with
      | None ->
          swap pool conn proposed_peer_id new_point >>= fun () ->
          Lwt.return_unit
      | Some _ ->
          Lwt.return_unit

and swap pool conn current_peer_id new_point =
  let source_peer_id = Peer_info.peer_id conn.peer_info in
  pool.latest_accepted_swap <- Time.now () ;
  connect ~timeout:10. pool new_point >>= function
  | Ok _new_conn -> begin
      pool.latest_succesfull_swap <- Time.now () ;
      log pool (Swap_success { source = source_peer_id }) ;
      lwt_log_info "Swap to %a succeeded" Point.pp new_point >>= fun () ->
      match Connection.find_by_peer_id pool current_peer_id with
      | None -> Lwt.return_unit
      | Some conn ->
          disconnect conn >>= fun () ->
          Lwt.return_unit
    end
  | Error err -> begin
      pool.latest_accepted_swap <- pool.latest_succesfull_swap ;
      log pool (Swap_failure { source = source_peer_id }) ;
      lwt_log_error "Swap to %a failed: %a"
        Point.pp new_point pp_print_error err
    end

let accept pool fd point =
  log pool (Incoming_connection point) ;
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

let send_swap_request pool =
  match Connection.random pool with
  | None -> ()
  | Some recipient ->
      let recipient_peer_id = (Connection.info recipient).peer_id in
      match Connection.random_lowid ~different_than:recipient pool with
      | None -> ()
      | Some (proposed_point, proposed_peer_id, _proposed_conn) ->
          log pool (Swap_request_sent { source = recipient_peer_id }) ;
          recipient.last_sent_swap_request <-
            Some (Time.now (), proposed_peer_id) ;
          ignore (P2p_connection.write_now recipient.conn
                    (Swap_request (proposed_point, proposed_peer_id)))

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
    new_connection_hook = [] ;
    latest_accepted_swap = Time.epoch ;
    latest_succesfull_swap = Time.epoch ;
  } in
  List.iter (Points.set_trusted pool) config.trusted_points ;
  Peer_info.File.load config.peers_file meta_config.encoding >>= function
  | Ok peer_ids ->
      List.iter
        (fun peer_info ->
           let peer_id = Peer_info.peer_id peer_info in
           Peer_id.Table.add pool.known_peer_ids peer_id peer_info)
        peer_ids ;
      Lwt.return pool
  | Error err ->
      log_error "@[Failed to parsed peers file:@ %a@]"
        pp_print_error err ;
      Lwt.return pool

let destroy pool =
  Point.Table.fold (fun _point point_info acc ->
      match Point_info.State.get point_info with
      | Requested { cancel } | Accepted { cancel } ->
          Canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn } ->
          disconnect conn >>= fun () -> acc
      | Disconnected ->  acc)
    pool.known_points @@
  Peer_id.Table.fold (fun _peer_id peer_info acc ->
      match Peer_info.State.get peer_info with
      | Accepted { cancel } ->
          Canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn } ->
          disconnect conn >>= fun () -> acc
      | Disconnected ->  acc)
    pool.known_peer_ids @@
  Point.Table.fold (fun _point canceler acc ->
      Canceler.cancel canceler >>= fun () -> acc)
    pool.incoming Lwt.return_unit

let on_new_connection pool f =
  pool.new_connection_hook <- f :: pool.new_connection_hook
