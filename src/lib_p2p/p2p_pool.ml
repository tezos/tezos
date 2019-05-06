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

(* TODO Test cancellation of a (pending) connection *)

(* TODO do not recompute list_known_points at each requests... but
        only once in a while, e.g. every minutes or when a point
        or the associated peer_id is blacklisted. *)

(* TODO allow to track "requested peer_ids" when we reconnect to a point. *)

include Internal_event.Legacy_logging.Make (struct let name = "p2p.connection-pool" end)

type 'msg encoding = Encoding : {
    tag: int ;
    title: string ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg encoding

module Message = struct

  type 'msg t =
    | Bootstrap
    | Advertise of P2p_point.Id.t list
    | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
    | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
    | Message of 'msg
    | Disconnect

  let encoding msg_encoding =
    let open Data_encoding in
    dynamic_size @@
    union ~tag_size:`Uint16
      ([ case (Tag 0x01) ~title:"Disconnect"
           (obj1 (req "kind" (constant "Disconnect")))
           (function Disconnect -> Some () | _ -> None)
           (fun () -> Disconnect);
         case (Tag 0x02) ~title:"Bootstrap"
           (obj1 (req "kind" (constant "Bootstrap")))
           (function Bootstrap -> Some () | _ -> None)
           (fun () -> Bootstrap);
         case (Tag 0x03) ~title:"Advertise"
           (obj2
              (req "id" (Variable.list P2p_point.Id.encoding))
              (req "kind" (constant "Advertise")))
           (function Advertise points -> Some (points, ()) | _ -> None)
           (fun (points, ()) -> Advertise points);
         case (Tag 0x04) ~title:"Swap_request"
           (obj3
              (req "point" P2p_point.Id.encoding)
              (req "peer_id" P2p_peer.Id.encoding)
              (req "kind" (constant "Swap_request")))
           (function
             | Swap_request (point, peer_id) -> Some (point, peer_id, ())
             | _ -> None)
           (fun (point, peer_id, ()) -> Swap_request (point, peer_id)) ;
         case (Tag 0x05)
           ~title:"Swap_ack"
           (obj3
              (req "point" P2p_point.Id.encoding)
              (req "peer_id" P2p_peer.Id.encoding)
              (req "kind" (constant "Swap_ack")))
           (function
             | Swap_ack (point, peer_id) -> Some (point, peer_id, ())
             | _ -> None)
           (fun (point, peer_id, ()) -> Swap_ack (point, peer_id)) ;
       ] @
       ListLabels.map msg_encoding
         ~f:(function Encoding { tag ; title ; encoding ; wrap ; unwrap ; max_length = _ (* ?? *) } ->
             Data_encoding.case (Tag tag)
               ~title
               encoding
               (function Message msg -> unwrap msg | _ -> None)
               (fun msg -> Message (wrap msg))))

end


module Answerer = struct

  type 'msg callback = {
    bootstrap: unit -> P2p_point.Id.t list Lwt.t ;
    advertise: P2p_point.Id.t list -> unit Lwt.t ;
    message: int -> 'msg -> unit Lwt.t ;
    swap_request: P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t ;
    swap_ack: P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t ;
  }

  type ('msg, 'meta) t = {
    canceler: Lwt_canceler.t ;
    conn: ('msg Message.t, 'meta) P2p_socket.t ;
    callback: 'msg callback ;
    mutable worker: unit Lwt.t ;
  }

  let rec worker_loop st =
    Lwt_unix.yield () >>= fun () ->
    protect ~canceler:st.canceler begin fun () ->
      P2p_socket.read st.conn
    end >>= function
    | Ok (_, Bootstrap) -> begin
        (* st.callback.bootstrap will return an empty list if the node
           is in private mode *)
        st.callback.bootstrap () >>= function
        | [] ->
            worker_loop st
        | points ->
            match P2p_socket.write_now st.conn (Advertise points) with
            | Ok _sent ->
                (* if not sent then ?? TODO count dropped message ?? *)
                worker_loop st
            | Error _ ->
                Lwt_canceler.cancel st.canceler >>= fun () ->
                Lwt.return_unit
      end
    | Ok (_, Advertise points) ->
        (* st.callback.advertise will ignore the points if the node is
           in private mode *)
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
    | Ok (_, Disconnect) | Error [P2p_errors.Connection_closed] ->
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [P2p_errors.Decoding_error] ->
        (* TODO: Penalize peer... *)
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit
    | Error [ Canceled ] ->
        Lwt.return_unit
    | Error err ->
        lwt_log_error "@[Answerer unexpected error:@ %a@]"
          Error_monad.pp_print_error err >>= fun () ->
        Lwt_canceler.cancel st.canceler >>= fun () ->
        Lwt.return_unit

  let run conn canceler callback =
    let st = {
      canceler ; conn ; callback ;
      worker = Lwt.return_unit ;
    } in
    st.worker <-
      Lwt_utils.worker "answerer"
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Lwt_canceler.cancel canceler) ;
    st

  let shutdown st =
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker

end

type config = {

  identity : P2p_identity.t ;
  proof_of_work_target : Crypto_box.target ;

  trusted_points : P2p_point.Id.t list ;
  peers_file : string ;
  private_mode : bool ;

  listening_port : P2p_addr.port option ;
  min_connections : int ;
  max_connections : int ;
  max_incoming_connections : int ;
  connection_timeout : Time.System.Span.t ;
  authentication_timeout : Time.System.Span.t ;

  incoming_app_message_queue_size : int option ;
  incoming_message_queue_size : int option ;
  outgoing_message_queue_size : int option ;

  known_peer_ids_history_size : int ;
  known_points_history_size : int ;
  max_known_points : (int * int) option ; (* max, gc target *)
  max_known_peer_ids : (int * int) option ; (* max, gc target *)

  swap_linger : Time.System.Span.t ;

  binary_chunks_size : int option ;
}

type 'peer_meta peer_meta_config = {
  peer_meta_encoding : 'peer_meta Data_encoding.t ;
  peer_meta_initial : unit -> 'peer_meta ;
  score : 'peer_meta -> float ;
}

type 'msg message_config = {
  encoding : 'msg encoding list ;
  chain_name : Distributed_db_version.name ;
  distributed_db_versions : Distributed_db_version.t list ;
}

type ('msg, 'peer_meta, 'conn_meta) t = {
  config : config ;
  announced_version : Network_version.t ;
  custom_p2p_versions : P2p_version.t list ;
  peer_meta_config : 'peer_meta peer_meta_config ;
  conn_meta_config : 'conn_meta P2p_socket.metadata_config ;
  message_config : 'msg message_config ;
  my_id_points : unit P2p_point.Table.t ;
  known_peer_ids :
    (('msg, 'peer_meta, 'conn_meta) connection,
     'peer_meta,
     'conn_meta) P2p_peer_state.Info.t P2p_peer.Table.t ;
  connected_peer_ids :
    (('msg, 'peer_meta, 'conn_meta) connection,
     'peer_meta,
     'conn_meta) P2p_peer_state.Info.t P2p_peer.Table.t ;
  known_points :
    ('msg, 'peer_meta, 'conn_meta) connection P2p_point_state.Info.t P2p_point.Table.t ;
  connected_points :
    ('msg, 'peer_meta, 'conn_meta) connection P2p_point_state.Info.t P2p_point.Table.t ;
  incoming : Lwt_canceler.t P2p_point.Table.t ;
  io_sched : P2p_io_scheduler.t ;
  encoding : 'msg Message.t Data_encoding.t ;
  events : events ;
  watcher : P2p_connection.Pool_event.t Lwt_watcher.input ;
  acl : P2p_acl.t ;
  mutable new_connection_hook :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) list ;
  mutable latest_accepted_swap : Time.System.t ;
  mutable latest_succesfull_swap : Time.System.t  ;
}

and events = {
  too_few_connections : unit Lwt_condition.t ;
  too_many_connections : unit Lwt_condition.t ;
  new_peer : unit Lwt_condition.t ;
  new_point : unit Lwt_condition.t ;
  new_connection : unit Lwt_condition.t ;
}

and ('msg, 'peer_meta, 'conn_meta) connection = {
  canceler : Lwt_canceler.t ;
  messages : (int * 'msg) Lwt_pipe.t ;
  conn : ('msg Message.t, 'conn_meta) P2p_socket.t ;
  peer_info :
    (('msg, 'peer_meta, 'conn_meta) connection, 'peer_meta, 'conn_meta) P2p_peer_state.Info.t ;
  point_info :
    ('msg, 'peer_meta, 'conn_meta) connection P2p_point_state.Info.t option ;
  negotiated_version : Network_version.t ;
  answerer : ('msg, 'conn_meta) Answerer.t Lazy.t ;
  mutable last_sent_swap_request : (Time.System.t * P2p_peer.Id.t) option ;
  mutable wait_close : bool ;
}

type ('msg, 'peer_meta, 'conn_meta) pool = ('msg, 'peer_meta, 'conn_meta) t

module Pool_event = struct
  let wait_too_few_connections pool =
    Lwt_condition.wait pool.events.too_few_connections
  let wait_too_many_connections pool =
    Lwt_condition.wait pool.events.too_many_connections
  let wait_new_peer pool =
    Lwt_condition.wait pool.events.new_peer
  let wait_new_point pool =
    Lwt_condition.wait pool.events.new_point
  let wait_new_connection pool =
    Lwt_condition.wait pool.events.new_connection
end

let watch { watcher ; _ } = Lwt_watcher.create_stream watcher
let log { watcher ; _ } event = Lwt_watcher.notify watcher event
let private_node_warn fmt =
  Format.kasprintf (fun s -> lwt_warn "[private node] %s" s) fmt

module Gc_point_set = List.Bounded(struct
    type t = Time.System.t * P2p_point.Id.t
    let compare (x, _) (y, _) = - (Time.System.compare x y)
  end)

let gc_points ({ config = { max_known_points ; _ } ; known_points ; _ } as pool) =
  match max_known_points with
  | None -> ()
  | Some (_, target) ->
      let current_size = P2p_point.Table.length known_points in
      if current_size > target then
        let to_remove_target = current_size - target in
        let now = Systime_os.now () in (* TODO: maybe time of discovery? *)
        let table = Gc_point_set.create to_remove_target in
        P2p_point.Table.iter (fun p point_info ->
            if P2p_point_state.is_disconnected point_info then
              let time =
                match P2p_point_state.Info.last_miss point_info with
                | None -> now
                | Some t -> t in
              Gc_point_set.insert (time, p) table
          ) known_points ;
        let to_remove = Gc_point_set.get table in
        ListLabels.iter to_remove ~f:begin fun (_, p) ->
          P2p_point.Table.remove known_points p
        end ;
        log pool Gc_points

let register_point pool ?trusted _source_peer_id (addr, port as point) =
  match P2p_point.Table.find_opt pool.known_points point with
  | None ->
      let point_info = P2p_point_state.Info.create ?trusted addr port in
      Option.iter pool.config.max_known_points ~f:begin fun (max, _) ->
        if P2p_point.Table.length pool.known_points >= max then gc_points pool
      end ;
      P2p_point.Table.add pool.known_points point point_info ;
      Lwt_condition.broadcast pool.events.new_point () ;
      log pool (New_point point) ;
      point_info
  | Some point_info ->
      begin
        match trusted with
        | Some true -> P2p_point_state.Info.set_trusted point_info ;
        | _ -> ()
      end ;
      point_info

let may_register_my_id_point pool = function
  | [P2p_errors.Myself (addr, Some port)] ->
      P2p_point.Table.add pool.my_id_points (addr, port) () ;
      P2p_point.Table.remove pool.known_points (addr, port)
  | _ -> ()


(* Bounded table used to garbage collect peer_id infos when needed. The
   strategy used is to remove the info of the peer_id with the lowest
   score first. In case of equality, the info of the most recent added
   peer_id is removed. The rationale behind this choice is that in the
   case of a flood attack, the newly added infos will probably belong
   to peer_ids with the same (low) score and removing the most recent ones
   ensure that older (and probably legit) peer_id infos are kept. *)
module Gc_peer_set = List.Bounded(struct
    type t = float * Time.System.t * P2p_peer.Id.t
    let compare (s, t, _) (s', t', _) =
      let score_cmp = Pervasives.compare s s' in
      if score_cmp = 0 then Time.System.compare t t' else - score_cmp
  end)

let gc_peer_ids ({ peer_meta_config = { score ; _ } ;
                   config = { max_known_peer_ids ; _ } ;
                   known_peer_ids ; _ } as pool) =
  match max_known_peer_ids with
  | None -> ()
  | Some (_, target) ->
      let current_size = P2p_peer.Table.length known_peer_ids in
      if current_size > target then
        let to_remove_target = current_size - target in
        let table = Gc_peer_set.create to_remove_target in
        P2p_peer.Table.iter (fun peer_id peer_info ->
            let created = P2p_peer_state.Info.created peer_info in
            let score = score @@ P2p_peer_state.Info.peer_metadata peer_info in
            if P2p_peer_state.is_disconnected peer_info then
              Gc_peer_set.insert (score, created, peer_id) table
          ) known_peer_ids ;
        let to_remove = Gc_peer_set.get table in
        ListLabels.iter to_remove ~f:begin fun (_, _, peer_id) ->
          P2p_peer.Table.remove known_peer_ids peer_id
        end ;
        log pool Gc_peer_ids

let register_peer pool peer_id =
  match P2p_peer.Table.find_opt pool.known_peer_ids peer_id with
  | None ->
      Lwt_condition.broadcast pool.events.new_peer () ;
      let peer =
        P2p_peer_state.Info.create peer_id
          ~peer_metadata:(pool.peer_meta_config.peer_meta_initial ()) in
      Option.iter pool.config.max_known_peer_ids ~f:begin fun (max, _) ->
        if P2p_peer.Table.length pool.known_peer_ids >= max then gc_peer_ids pool
      end ;
      P2p_peer.Table.add pool.known_peer_ids peer_id peer ;
      log pool (New_peer peer_id) ;
      peer
  | Some peer -> peer


(***************************************************************************)

let read { messages ; conn ; _ } =
  Lwt.catch
    (fun () ->
       Lwt_pipe.pop messages >>= fun (s, msg) ->
       lwt_debug "%d bytes message popped from queue %a\027[0m"
         s P2p_peer.Id.pp (P2p_socket.info conn).peer_id >>= fun () ->
       return msg)
    (fun _ (* Closed *) -> fail P2p_errors.Connection_closed)

let is_readable { messages ; _ } =
  Lwt.catch
    (fun () -> Lwt_pipe.values_available messages >>= return)
    (fun _ (* Closed *) -> fail P2p_errors.Connection_closed)

let write { conn ; _ } msg =
  P2p_socket.write conn (Message msg)

let write_sync { conn ; _ } msg =
  P2p_socket.write_sync conn (Message msg)

let raw_write_sync { conn ; _ } buf =
  P2p_socket.raw_write_sync conn buf

let write_now { conn ; _ } msg =
  P2p_socket.write_now conn (Message msg)

let write_all pool msg =
  P2p_peer.Table.iter
    (fun _peer_id peer_info ->
       match P2p_peer_state.get peer_info with
       | Running { data = conn ; _ } ->
           ignore (write_now conn msg : bool tzresult )
       | _ -> ())
    pool.connected_peer_ids

let broadcast_bootstrap_msg pool =
  if not pool.config.private_mode then
    P2p_peer.Table.iter
      (fun _peer_id peer_info ->
         match P2p_peer_state.get peer_info with
         | Running { data = { conn ; _ } ; _ } ->
             (* should not ask private nodes for the list of their
                known peers*)
             if not (P2p_socket.private_node conn) then
               ignore (P2p_socket.write_now conn Bootstrap : bool tzresult )
         | _ -> ())
      pool.connected_peer_ids


(***************************************************************************)

(* this function duplicates bit of code from the modules below to avoid
   creating mutually recursive modules *)
let connection_of_peer_id pool peer_id =
  Option.apply
    (P2p_peer.Table.find_opt pool.known_peer_ids peer_id) ~f:begin fun p ->
    match P2p_peer_state.get p with
    | Running { data ; _ } -> Some data
    | _ -> None
  end

(* Every running connection matching the point's ip address is returned. *)
let connections_of_addr pool addr =
  P2p_point.Table.fold
    (fun (addr', _) p acc ->
       if Ipaddr.V6.compare addr addr' = 0
       then
         match P2p_point_state.get p with
         | P2p_point_state.Running { data ; _ } -> data :: acc
         | _ -> acc
       else acc
    ) pool.connected_points []

let get_addr pool peer_id =
  Option.map (connection_of_peer_id pool peer_id) ~f:begin fun ci ->
    (P2p_socket.info ci.conn).id_point
  end

module Points = struct

  type ('msg, 'peer_meta, 'conn_meta) info =
    ('msg, 'peer_meta, 'conn_meta) connection P2p_point_state.Info.t

  let info { known_points ; _ } point =
    P2p_point.Table.find_opt known_points point

  let get_trusted pool point =
    Option.unopt_map ~default:false ~f:P2p_point_state.Info.trusted
      (P2p_point.Table.find_opt pool.known_points point)

  let set_trusted pool point =
    P2p_point_state.Info.set_trusted
      (register_point pool pool.config.identity.peer_id point)

  let unset_trusted pool point =
    Option.iter ~f:P2p_point_state.Info.unset_trusted
      (P2p_point.Table.find_opt pool.known_points point)

  let fold_known pool ~init ~f =
    P2p_point.Table.fold f pool.known_points init

  let fold_connected pool ~init ~f =
    P2p_point.Table.fold f pool.connected_points init

  let banned pool (addr, _port) =
    P2p_acl.banned_addr pool.acl addr

  let ban pool (addr, _port) =
    P2p_acl.IPBlacklist.add pool.acl addr ;
    (* Kick [addr]:* if it is in `Running` state. *)
    List.iter (fun conn ->
        conn.wait_close <- false ;
        Lwt.async (fun () -> Answerer.shutdown (Lazy.force conn.answerer))
      ) (connections_of_addr pool addr)

  let unban pool (addr, _port) =
    P2p_acl.IPBlacklist.remove pool.acl addr

  let trust pool ((addr, _port) as point) =
    P2p_acl.IPBlacklist.remove pool.acl addr ;
    set_trusted pool point

  let untrust pool point =
    unset_trusted pool point

end

module Peers = struct

  type ('msg, 'peer_meta, 'conn_meta) info =
    (('msg, 'peer_meta, 'conn_meta) connection, 'peer_meta, 'conn_meta) P2p_peer_state.Info.t

  let info { known_peer_ids ; _ } peer_id =
    try Some (P2p_peer.Table.find known_peer_ids peer_id)
    with Not_found -> None

  let get_peer_metadata pool peer_id =
    try P2p_peer_state.Info.peer_metadata (P2p_peer.Table.find pool.known_peer_ids peer_id)
    with Not_found -> pool.peer_meta_config.peer_meta_initial ()

  let get_score pool peer_id =
    pool.peer_meta_config.score (get_peer_metadata pool peer_id)

  let set_peer_metadata pool peer_id data =
    P2p_peer_state.Info.set_peer_metadata (register_peer pool peer_id) data

  let get_trusted pool peer_id =
    try P2p_peer_state.Info.trusted (P2p_peer.Table.find pool.known_peer_ids peer_id)
    with Not_found -> false

  let set_trusted pool peer_id =
    try P2p_peer_state.Info.set_trusted (register_peer pool peer_id)
    with Not_found -> ()

  let unset_trusted pool peer_id =
    try P2p_peer_state.Info.unset_trusted (P2p_peer.Table.find pool.known_peer_ids peer_id)
    with Not_found -> ()

  let fold_known pool ~init ~f =
    P2p_peer.Table.fold f pool.known_peer_ids init

  let fold_connected pool ~init ~f =
    P2p_peer.Table.fold f pool.connected_peer_ids init

  let ban pool peer =
    P2p_acl.PeerBlacklist.add pool.acl peer ;
    (* Kick [peer] if it is in `Running` state. *)
    Option.iter (connection_of_peer_id pool peer) ~f:begin fun conn ->
      conn.wait_close <- false ;
      Lwt.async (fun () -> Answerer.shutdown (Lazy.force conn.answerer))
    end

  let unban pool peer =
    P2p_acl.PeerBlacklist.remove pool.acl peer

  let trust pool peer =
    unban pool peer ;
    set_trusted pool peer

  let untrust pool peer =
    unset_trusted pool peer

  let banned pool peer =
    P2p_acl.banned_peer pool.acl peer

end

module Connection = struct

  let trusted_node conn =
    P2p_peer_state.Info.trusted conn.peer_info ||
    Option.unopt_map
      ~default:false
      ~f:P2p_point_state.Info.trusted
      conn.point_info

  let private_node conn = P2p_socket.private_node conn.conn

  let fold pool ~init ~f =
    Peers.fold_connected pool ~init ~f:begin fun peer_id peer_info acc ->
      match P2p_peer_state.get peer_info with
      | Running { data ; _ } -> f peer_id data acc
      | _ -> acc
    end

  let list pool =
    fold pool ~init:[] ~f:(fun peer_id c acc -> (peer_id, c) :: acc)

  let random ?different_than ~no_private pool =
    let candidates =
      fold pool ~init:[] ~f:begin fun _peer conn acc ->
        if no_private && (private_node conn) then
          acc
        else
          match different_than with
          | Some excluded_conn
            when P2p_socket.equal conn.conn excluded_conn.conn -> acc
          | Some _ | None -> conn :: acc
      end in
    match candidates with
    | [] -> None
    | _ :: _ ->
        Some (List.nth candidates (Random.int @@ List.length candidates))

  let random_lowid ?different_than ~no_private pool =
    let candidates =
      fold pool ~init:[] ~f:begin fun _peer conn acc ->
        if no_private && (private_node conn) then
          acc
        else
          match different_than with
          | Some excluded_conn
            when P2p_socket.equal conn.conn excluded_conn.conn -> acc
          | Some _ | None ->
              let ci = P2p_socket.info conn.conn in
              match ci.id_point with
              | _, None -> acc
              | addr, Some port -> ((addr, port), ci.peer_id, conn) :: acc
      end in
    match candidates with
    | [] -> None
    | _ :: _ ->
        Some (List.nth candidates (Random.int @@ List.length candidates))

  let stat { conn ; _ } =
    P2p_socket.stat conn

  let info { conn ; _ } =
    P2p_socket.info conn

  let local_metadata { conn ; _ } =
    P2p_socket.local_metadata conn

  let remote_metadata { conn ; _ } =
    P2p_socket.remote_metadata conn

  let find_by_peer_id pool peer_id =
    Option.apply
      (Peers.info pool peer_id)
      ~f:(fun p ->
          match P2p_peer_state.get p with
          | Running { data ; _ } -> Some data
          | _ -> None)

  let find_by_point pool point =
    Option.apply
      (Points.info pool point)
      ~f:(fun p ->
          match P2p_point_state.get p with
          | Running { data ; _ } -> Some data
          | _ -> None)

end

let greylist_addr pool addr =
  P2p_acl.IPGreylist.add pool.acl addr (Systime_os.now ())

let greylist_peer pool peer =
  Option.iter (get_addr pool peer) ~f:begin fun (addr, _port) ->
    greylist_addr pool addr ;
    P2p_acl.PeerGreylist.add pool.acl peer
  end

let acl_clear pool =
  P2p_acl.clear pool.acl

let gc_greylist ~older_than pool =
  P2p_acl.IPGreylist.remove_old ~older_than pool.acl

let pool_stat { io_sched ; _ } =
  P2p_io_scheduler.global_stat io_sched

let config { config ; _ } = config

let score { peer_meta_config = { score ; _ } ; _ } meta = score meta

(***************************************************************************)

let fail_unless_disconnected_point point_info =
  match P2p_point_state.get point_info with
  | Disconnected -> return_unit
  | Requested _ | Accepted _ -> fail P2p_errors.Pending_connection
  | Running _ -> fail P2p_errors.Connected

let compare_known_point_info p1 p2 =
  (* The most-recently disconnected peers are greater. *)
  (* Then come long-standing connected peers. *)
  let disconnected1 = P2p_point_state.is_disconnected p1
  and disconnected2 = P2p_point_state.is_disconnected p2 in
  let compare_last_seen p1 p2 =
    match P2p_point_state.Info.last_seen p1, P2p_point_state.Info.last_seen p2 with
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

let rec connect ?timeout pool point =
  fail_when (Points.banned pool point)
    (P2p_errors.Point_banned point) >>=? fun () ->
  let timeout =
    Option.unopt ~default:pool.config.connection_timeout timeout in
  fail_unless
    (active_connections pool <= pool.config.max_connections)
    P2p_errors.Too_many_connections >>=? fun () ->
  let canceler = Lwt_canceler.create () in
  with_timeout ~canceler (Systime_os.sleep timeout) begin fun canceler ->
    let point_info =
      register_point pool pool.config.identity.peer_id point in
    let addr, port as point = P2p_point_state.Info.point point_info in
    fail_unless
      (not pool.config.private_mode || P2p_point_state.Info.trusted point_info)
      P2p_errors.Private_mode >>=? fun () ->
    fail_unless_disconnected_point point_info >>=? fun () ->
    P2p_point_state.set_requested point_info canceler ;
    let fd = P2p_fd.socket PF_INET6 SOCK_STREAM 0 in
    let uaddr =
      Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    lwt_debug "connect: %a" P2p_point.Id.pp point >>= fun () ->
    protect ~canceler begin fun () ->
      log pool (Outgoing_connection point) ;
      P2p_fd.connect fd uaddr >>= fun () ->
      return_unit
    end ~on_error: begin fun err ->
      lwt_debug "connect: %a -> disconnect" P2p_point.Id.pp point >>= fun () ->
      P2p_point_state.set_disconnected point_info ;
      P2p_fd.close fd >>= fun () ->
      match err with
      | [Exn (Unix.Unix_error (Unix.ECONNREFUSED, _, _))] ->
          fail P2p_errors.Connection_refused
      | err -> Lwt.return_error err
    end >>=? fun () ->
    lwt_debug "connect: %a -> authenticate" P2p_point.Id.pp point >>= fun () ->
    authenticate pool ~point_info canceler fd point
  end

and authenticate pool ?point_info canceler fd point =
  let fd = P2p_io_scheduler.register pool.io_sched fd in
  raw_authenticate pool ?point_info canceler fd point >>= function
  | Ok connection -> return connection
  | Error _ as err ->
      P2p_io_scheduler.close fd >>=? fun () ->
      Lwt.return err

and raw_authenticate pool ?point_info canceler fd point =
  let incoming = point_info = None in
  lwt_debug "authenticate: %a%s"
    P2p_point.Id.pp point
    (if incoming then " incoming" else "") >>= fun () ->
  protect ~canceler begin fun () ->
    P2p_socket.authenticate
      ~canceler
      ~proof_of_work_target:pool.config.proof_of_work_target
      ~incoming fd point
      ?listening_port:pool.config.listening_port
      pool.config.identity pool.announced_version
      pool.conn_meta_config
  end ~on_error: begin fun err ->
    begin match err with
      | [ Canceled ] ->
          (* Currently only on time out *)
          lwt_debug "authenticate: %a%s -> canceled"
            P2p_point.Id.pp point
            (if incoming then " incoming" else "")
      | err -> begin
          (* Authentication incorrect! Temp ban the offending points/peers *)
          List.iter (function
              | P2p_errors.Not_enough_proof_of_work _
              | P2p_errors.Invalid_auth
              | P2p_errors.Decipher_error
              | P2p_errors.Invalid_message_size
              | P2p_errors.Encoding_error
              | P2p_errors.Decoding_error
              | P2p_errors.Invalid_chunks_size _ ->
                  greylist_addr pool (fst point)
              | _ -> ()
            ) err ;
          lwt_debug "@[authenticate: %a%s -> failed@ %a@]"
            P2p_point.Id.pp point
            (if incoming then " incoming" else "")
            pp_print_error err
        end
    end >>= fun () ->
    may_register_my_id_point pool err ;
    log pool (Authentication_failed point) ;
    if incoming then
      P2p_point.Table.remove pool.incoming point
    else
      Option.iter ~f:P2p_point_state.set_disconnected point_info ;
    Lwt.return_error err
  end >>=? fun (info, auth_fd) ->
  (* Authentication correct! *)
  lwt_debug "authenticate: %a -> auth %a"
    P2p_point.Id.pp point
    P2p_peer.Id.pp info.peer_id >>= fun () ->
  fail_when (Peers.banned pool info.peer_id)
    (P2p_errors.Peer_banned info.peer_id) >>=? fun () ->
  let remote_point_info =
    match info.id_point with
    | addr, Some port
      when not (P2p_point.Table.mem pool.my_id_points (addr, port)) ->
        Some (register_point pool info.peer_id (addr, port))
    | _ -> None in
  let connection_point_info =
    match point_info, remote_point_info with
    | None, None -> None
    | Some _ as point_info, _ | _, (Some _ as point_info) -> point_info in
  let peer_info = register_peer pool info.peer_id in
  let acceptable_version =
    Network_version.select
      ~chain_name:pool.message_config.chain_name
      ~distributed_db_versions:pool.message_config.distributed_db_versions
      ~p2p_versions:pool.custom_p2p_versions
      info.announced_version in
  let acceptable_point =
    Option.unopt_map connection_point_info
      ~default:(not pool.config.private_mode)
      ~f:begin fun connection_point_info ->
        match P2p_point_state.get connection_point_info with
        | Requested _ -> not incoming
        | Disconnected ->
            let unexpected =
              pool.config.private_mode
              && not (P2p_point_state.Info.trusted connection_point_info)
            in
            if unexpected then
              warn "[private node] incoming connection from untrused \
                    peer rejected!";
            not unexpected
        | Accepted _ | Running _ -> false
      end
  in
  let acceptable_peer_id =
    match P2p_peer_state.get peer_info with
    | Accepted _ ->
        (* TODO: in some circumstances cancel and accept... *)
        false
    | Running _ -> false
    | Disconnected -> true in
  (* To Verify : the thread must ? not be interrupted between
     point removal from incoming and point registration into
     active connection to prevent flooding attack.
     incoming_connections + active_connection must reflect/dominate
     the actual number of ongoing connections.
     On the other hand, if we wait too long for Ack, we will reject
     incoming connections, thus giving an entry point for dos attack
     by giving late Nack.
  *)
  if incoming then
    P2p_point.Table.remove pool.incoming point ;
  match acceptable_version with
  | Some version when acceptable_peer_id && acceptable_point -> begin
      log pool (Accepting_request (point, info.id_point, info.peer_id)) ;
      Option.iter connection_point_info
        ~f:(fun point_info ->
            P2p_point_state.set_accepted point_info info.peer_id canceler) ;
      P2p_peer_state.set_accepted peer_info info.id_point canceler ;
      lwt_debug "authenticate: %a -> accept %a"
        P2p_point.Id.pp point
        P2p_peer.Id.pp info.peer_id >>= fun () ->
      protect ~canceler begin fun () ->
        P2p_socket.accept
          ?incoming_message_queue_size:pool.config.incoming_message_queue_size
          ?outgoing_message_queue_size:pool.config.outgoing_message_queue_size
          ?binary_chunks_size:pool.config.binary_chunks_size
          ~canceler
          auth_fd pool.encoding >>=? fun conn ->
        lwt_debug "authenticate: %a -> Connected %a"
          P2p_point.Id.pp point
          P2p_peer.Id.pp info.peer_id >>= fun () ->
        return conn
      end ~on_error: begin fun err ->
        if incoming then
          log pool
            (Request_rejected (point, Some (info.id_point, info.peer_id))) ;
        lwt_debug "authenticate: %a -> rejected %a"
          P2p_point.Id.pp point
          P2p_peer.Id.pp info.peer_id >>= fun () ->
        Option.iter connection_point_info
          ~f:P2p_point_state.set_disconnected ;
        P2p_peer_state.set_disconnected peer_info ;
        Lwt.return_error err
      end >>=? fun conn ->
      let id_point =
        match info.id_point, Option.map ~f:P2p_point_state.Info.point point_info with
        | (addr, _), Some (_, port) -> addr, Some port
        | id_point, None -> id_point in
      return
        (create_connection
           pool conn
           id_point connection_point_info peer_info version)
    end
  | _ -> begin
      log pool (Rejecting_request (point, info.id_point, info.peer_id)) ;
      lwt_debug "authenticate: %a -> kick %a point: %B peer_id: %B"
        P2p_point.Id.pp point
        P2p_peer.Id.pp info.peer_id
        acceptable_point acceptable_peer_id >>= fun () ->
      P2p_socket.kick auth_fd >>= fun () ->
      if not incoming then begin
        Option.iter ~f:P2p_point_state.set_disconnected point_info ;
        (* FIXME P2p_peer_state.set_disconnected ~requested:true peer_info ; *)
      end ;
      match acceptable_version with
      | None -> begin
          lwt_debug "No common protocol@.\
                     (chains: local %a - remote %a)@.\
                     (db_versions: local [%a] - remote %a)@.\
                     (p2p_versions: local [%a] - remote %a)"
            Distributed_db_version.pp_name pool.message_config.chain_name
            Distributed_db_version.pp_name info.announced_version.chain_name
            (Format.pp_print_list Distributed_db_version.pp) pool.message_config.distributed_db_versions
            Distributed_db_version.pp info.announced_version.distributed_db_version
            (Format.pp_print_list P2p_version.pp) pool.custom_p2p_versions
            P2p_version.pp info.announced_version.p2p_version
          >>= fun () ->
          fail (P2p_errors.Rejected_no_common_protocol
                  { announced = info.announced_version })
        end
      | Some _ -> fail (P2p_errors.Rejected info.peer_id)
    end

and create_connection pool p2p_conn id_point point_info peer_info negotiated_version =
  let peer_id = P2p_peer_state.Info.peer_id peer_info in
  let canceler = Lwt_canceler.create () in
  let size =
    Option.map pool.config.incoming_app_message_queue_size
      ~f:(fun qs -> qs, fun (size, _) ->
          (Sys.word_size / 8) * 11 + size + Lwt_pipe.push_overhead) in
  let messages = Lwt_pipe.create ?size () in

  let rec callback_default =
    { Answerer.message =
        (fun size msg -> Lwt_pipe.push messages (size, msg)) ;
      advertise =
        (fun points -> register_new_points pool conn points ) ;
      bootstrap =
        (fun () -> list_known_points ~ignore_private:true pool conn) ;
      swap_request =
        (fun point peer_id -> swap_request pool conn point peer_id ) ;
      swap_ack =
        (fun point peer_id -> swap_ack pool conn point peer_id ) ;
    }

  (* when the node is in private mode: deactivate advertising,
     peers_swap and sending list of peers in callback *)
  and callback_private =
    { Answerer.message =
        (fun size msg -> Lwt_pipe.push messages (size, msg)) ;
      advertise =
        (fun _points ->
           private_node_warn
             "Received new peers addresses from %a"
             P2p_peer.Id.pp peer_id >>= fun () ->
           Lwt.return_unit
        ) ;
      bootstrap =
        (fun () ->
           private_node_warn
             "Receive requests for peers addresses from %a"
             P2p_peer.Id.pp peer_id >>= fun () ->
           Lwt.return_nil
        ) ;
      swap_request =
        (fun _point _peer_id ->
           private_node_warn
             "Received swap requests from %a"
             P2p_peer.Id.pp peer_id >>= fun () ->
           Lwt.return_unit
        ) ;
      swap_ack =
        (fun _point _peer_id ->
           private_node_warn
             "Received swap ack from %a"
             P2p_peer.Id.pp peer_id >>= fun () ->
           Lwt.return_unit
        ) ;
    }

  and answerer =
    lazy (
      Answerer.run p2p_conn canceler @@
      if pool.config.private_mode then callback_private else callback_default
    )

  and conn =
    { conn = p2p_conn ; point_info ; peer_info ;
      messages ; canceler ; answerer ; wait_close = false ;
      last_sent_swap_request = None ;
      negotiated_version } in
  ignore (Lazy.force answerer) ;
  let conn_meta = P2p_socket.remote_metadata p2p_conn in
  Option.iter point_info ~f:begin fun point_info ->
    let point = P2p_point_state.Info.point point_info in
    P2p_point_state.set_running
      ~known_private:(pool.conn_meta_config.private_node conn_meta)
      point_info peer_id conn;
    P2p_point.Table.add pool.connected_points point point_info ;
  end ;
  log pool (Connection_established (id_point, peer_id)) ;
  P2p_peer_state.set_running peer_info id_point conn conn_meta ;
  P2p_peer.Table.add pool.connected_peer_ids peer_id peer_info ;
  Lwt_condition.broadcast pool.events.new_connection () ;
  Lwt_canceler.on_cancel canceler begin fun () ->
    lwt_debug "Disconnect: %a (%a)"
      P2p_peer.Id.pp peer_id P2p_connection.Id.pp id_point >>= fun () ->
    Option.iter ~f:P2p_point_state.set_disconnected point_info ;
    log pool (Disconnection peer_id) ;
    P2p_peer_state.set_disconnected peer_info ;
    Option.iter point_info ~f:begin fun point_info ->
      P2p_point.Table.remove pool.connected_points (P2p_point_state.Info.point point_info) ;
    end ;
    P2p_peer.Table.remove pool.connected_peer_ids peer_id ;
    if pool.config.max_connections <= active_connections pool then begin
      Lwt_condition.broadcast pool.events.too_many_connections () ;
      log pool Too_many_connections ;
    end ;
    Lwt_pipe.close messages ;
    P2p_socket.close ~wait:conn.wait_close conn.conn
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

and register_new_points ?trusted pool conn =
  let source_peer_id = P2p_peer_state.Info.peer_id conn.peer_info in
  fun points ->
    List.iter (register_new_point ?trusted pool source_peer_id) points ;
    Lwt.return_unit

and register_new_point ?trusted pool source_peer_id point =
  if not (P2p_point.Table.mem pool.my_id_points point) then
    ignore (register_point ?trusted pool source_peer_id point)

and list_known_points ?(ignore_private = false) pool conn =
  if Connection.private_node conn then
    private_node_warn "Private peer (%a) asked other peers addresses"
      P2p_peer.Id.pp (P2p_peer_state.Info.peer_id conn.peer_info) >>= fun () ->
    Lwt.return_nil
  else
    let knowns =
      P2p_point.Table.fold
        (fun _ point_info acc ->
           if ignore_private &&
              not (P2p_point_state.Info.known_public point_info) then acc
           else point_info :: acc)
        pool.known_points [] in
    let best_knowns =
      List.take_n ~compare:compare_known_point_info 50 knowns in
    Lwt.return (List.map P2p_point_state.Info.point best_knowns)

and active_connections pool = P2p_peer.Table.length pool.connected_peer_ids

and swap_request pool conn new_point _new_peer_id =
  let source_peer_id = P2p_peer_state.Info.peer_id conn.peer_info in
  log pool (Swap_request_received { source = source_peer_id }) ;
  lwt_log_info
    "Swap request received from %a" P2p_peer.Id.pp source_peer_id >>= fun () ->
  (* Ignore if already connected to peer or already swapped less
     than <swap_linger> seconds ago. *)
  let span_since_last_swap =
    Ptime.diff
      (Systime_os.now ())
      (Time.System.max pool.latest_succesfull_swap pool.latest_accepted_swap) in
  let new_point_info = register_point pool source_peer_id new_point in
  if Ptime.Span.compare span_since_last_swap pool.config.swap_linger < 0
  || not (P2p_point_state.is_disconnected new_point_info) then begin
    log pool (Swap_request_ignored { source = source_peer_id }) ;
    lwt_log_info "Ignoring swap request from %a" P2p_peer.Id.pp source_peer_id
  end else begin
    match Connection.random_lowid pool ~no_private:true with
    | None ->
        lwt_log_info
          "No swap candidate for %a" P2p_peer.Id.pp source_peer_id
    | Some (proposed_point, proposed_peer_id, _proposed_conn) ->
        match P2p_socket.write_now
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
  let source_peer_id = P2p_peer_state.Info.peer_id conn.peer_info in
  log pool (Swap_ack_received { source = source_peer_id }) ;
  lwt_log_info
    "Swap ack received from %a" P2p_peer.Id.pp source_peer_id >>= fun () ->
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
  let source_peer_id = P2p_peer_state.Info.peer_id conn.peer_info in
  pool.latest_accepted_swap <- Systime_os.now () ;
  connect pool new_point >>= function
  | Ok _new_conn -> begin
      pool.latest_succesfull_swap <- Systime_os.now () ;
      log pool (Swap_success { source = source_peer_id }) ;
      lwt_log_info "Swap to %a succeeded" P2p_point.Id.pp new_point >>= fun () ->
      match Connection.find_by_peer_id pool current_peer_id with
      | None -> Lwt.return_unit
      | Some conn ->
          disconnect conn >>= fun () ->
          Lwt.return_unit
    end
  | Error err -> begin
      pool.latest_accepted_swap <- pool.latest_succesfull_swap ;
      log pool (Swap_failure { source = source_peer_id }) ;
      match err with
      | [ Timeout ] ->
          lwt_debug "Swap to %a was interrupted: %a"
            P2p_point.Id.pp new_point pp_print_error err
      | _ ->
          lwt_log_error "Swap to %a failed: %a"
            P2p_point.Id.pp new_point pp_print_error err
    end

let accept pool fd point =
  log pool (Incoming_connection point) ;
  let max_active_conns =
    if Random.bool () then
      (* randomly allow one additional incoming connection *)
      pool.config.max_connections + 1
    else
      pool.config.max_connections in
  if pool.config.max_incoming_connections <= P2p_point.Table.length pool.incoming
  || max_active_conns <= active_connections pool
  (* silently ignore banned points *)
  || (P2p_acl.banned_addr pool.acl (fst point)) then
    Lwt.async (fun () -> P2p_fd.close fd)
  else
    let canceler = Lwt_canceler.create () in
    P2p_point.Table.add pool.incoming point canceler ;
    Lwt.async begin fun () ->
      with_timeout
        ~canceler (Systime_os.sleep pool.config.authentication_timeout)
        (fun canceler -> authenticate pool canceler fd point)
    end

let send_swap_request pool =
  match Connection.random ~no_private:true pool with
  | Some recipient when not pool.config.private_mode -> begin
      let recipient_peer_id = (Connection.info recipient).peer_id in
      match
        Connection.random_lowid
          ~different_than:recipient
          ~no_private:true pool
      with
      | None -> ()
      | Some (proposed_point, proposed_peer_id, _proposed_conn) ->
          log pool (Swap_request_sent { source = recipient_peer_id }) ;
          recipient.last_sent_swap_request <-
            Some (Systime_os.now (), proposed_peer_id) ;
          ignore (P2p_socket.write_now recipient.conn
                    (Swap_request (proposed_point, proposed_peer_id)))
    end
  | Some _ | None -> ()

(***************************************************************************)

let create
    ?(p2p_versions = P2p_version.supported)
    config peer_meta_config conn_meta_config message_config io_sched =
  let events = {
    too_few_connections = Lwt_condition.create () ;
    too_many_connections = Lwt_condition.create () ;
    new_peer = Lwt_condition.create () ;
    new_point = Lwt_condition.create () ;
    new_connection = Lwt_condition.create () ;
  } in
  let pool = {
    config ; peer_meta_config ; conn_meta_config ; message_config ;
    announced_version =
      Network_version.announced
        ~chain_name: message_config.chain_name
        ~distributed_db_versions: message_config.distributed_db_versions
        ~p2p_versions ;
    custom_p2p_versions = p2p_versions ;
    my_id_points = P2p_point.Table.create 7 ;
    known_peer_ids = P2p_peer.Table.create 53 ;
    connected_peer_ids = P2p_peer.Table.create 53 ;
    known_points = P2p_point.Table.create 53 ;
    connected_points = P2p_point.Table.create 53 ;
    incoming = P2p_point.Table.create 53 ;
    io_sched ;
    encoding = Message.encoding message_config.encoding ;
    events ;
    watcher = Lwt_watcher.create_input () ;
    acl = P2p_acl.create 1023;
    new_connection_hook = [] ;
    latest_accepted_swap = Ptime.epoch ;
    latest_succesfull_swap = Ptime.epoch ;
  } in
  List.iter (Points.set_trusted pool) config.trusted_points ;
  P2p_peer_state.Info.File.load
    config.peers_file
    peer_meta_config.peer_meta_encoding >>= function
  | Ok peer_ids ->
      List.iter
        (fun peer_info ->
           let peer_id = P2p_peer_state.Info.peer_id peer_info in
           P2p_peer.Table.add pool.known_peer_ids peer_id peer_info)
        peer_ids ;
      Lwt.return pool
  | Error err ->
      log_error "@[Failed to parse peers file:@ %a@]"
        pp_print_error err ;
      Lwt.return pool

let destroy ({ config ; peer_meta_config ; _ } as pool) =
  lwt_log_info "Saving metadata in %s" config.peers_file >>= fun () ->
  begin
    P2p_peer_state.Info.File.save
      config.peers_file
      peer_meta_config.peer_meta_encoding
      (P2p_peer.Table.fold (fun _ a b -> a::b) pool.known_peer_ids []) >>= function
    | Error err ->
        log_error "@[Failed to save peers file:@ %a@]"
          pp_print_error err;
        Lwt.return_unit
    | Ok ()-> Lwt.return_unit
  end >>= fun () ->
  P2p_point.Table.fold (fun _point point_info acc ->
      match P2p_point_state.get point_info with
      | Requested { cancel } | Accepted { cancel ; _ } ->
          Lwt_canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn ; _ } ->
          disconnect conn >>= fun () -> acc
      | Disconnected -> acc)
    pool.known_points @@
  P2p_peer.Table.fold (fun _peer_id peer_info acc ->
      match P2p_peer_state.get peer_info with
      | Accepted { cancel ; _ } ->
          Lwt_canceler.cancel cancel >>= fun () -> acc
      | Running { data = conn ; _ } ->
          disconnect conn >>= fun () -> acc
      | Disconnected -> acc)
    pool.known_peer_ids @@
  P2p_point.Table.fold (fun _point canceler acc ->
      Lwt_canceler.cancel canceler >>= fun () -> acc)
    pool.incoming Lwt.return_unit

let on_new_connection pool f =
  pool.new_connection_hook <- f :: pool.new_connection_hook

