(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module LU = Lwt_unix
module LC = Lwt_condition
open Lwt
open Lwt_utils
open Netbits
open Logging.Net

let pp_gid ppf gid =
  Format.pp_print_string ppf (Hex_encode.hex_encode gid)

(* public types *)
type addr = Ipaddr.t
type port = int
type version = string * int * int
type limits = {
  max_packet_size : int ;
  peer_answer_timeout : float ;
  expected_connections : int ;
  min_connections : int ;
  max_connections : int ;
  blacklist_time : float ;
}
type config = {
  incoming_port : port option ;
  discovery_port : port option ;
  supported_versions : version list ;
  known_peers : (addr * port) list ;
  peers_file : string ;
  closed_network : bool ;
}

(* the common version for a pair of peers, if any, is the maximum one,
   in lexicographic order *)
let common_version la lb =
  let la = List.sort (fun l r -> compare r l) la in
  let lb = List.sort (fun l r -> compare r l) lb in
  let rec find = function
    | [], _ | _, [] -> None
    | ((a :: ta) as la), ((b :: tb) as lb) ->
        if a = b then Some a
        else if a < b then find (ta, lb)
        else find (la, tb)
  in find (la, lb)

(* The global net identificator. *)
type gid = string

(* A net point (address x port). *)
type point = addr * port

(* Low-level network protocol packets (internal). The protocol is
   completely symmetrical and asynchronous. First both peers must
   present their credentials with a [Connect] packet, then any
   combination of the other packets can be received at any time. An
   exception is the [Disconnect] message, which should mark the end of
   transmission (and needs not being replied). The [Unkown] packet is
   not a real kind of packet, it means that something indecypherable
   was transmitted. *)
type packet =
  | Connect of gid * Crypto_box.public_key * int option * version list
  | Disconnect
  | Advertise of (addr * port) list
  | Message of Netbits.frame
  | Ping
  | Pong
  | Bootstrap
  | Unknown of Netbits.frame

(* read a packet from a TCP socket *)
let recv_packet
  : LU.file_descr -> int -> packet Lwt.t
  = fun socket limit ->
    Netbits.read socket limit >>= function
    | None ->
        return Disconnect
    | Some frame ->
        let decode_versions msg frame cb =
          let rec decode_versions acc = function
            | F [ B name ; S maj ; S min ] :: rest ->
                decode_versions ((MBytes.to_string name, maj, min) :: acc) rest
            | [] -> cb (List.rev acc)
            | _ -> return (Unknown msg)
          in decode_versions [] frame
        in
        match frame with
        | [ S 1 ] -> return Disconnect
        | [ S 2 ] -> return Ping
        | [ S 12 ] -> return Pong
        | [ S 3 ] -> return Bootstrap
        | [ S 4 ; B gid ; B public_key ; S port ; F rest ] as msg ->
            decode_versions msg rest @@ fun versions ->
            return (Connect (MBytes.to_string gid, Crypto_box.to_public_key public_key, Some port, versions))
        | [ S 4 ; B gid ; B public_key ; F rest ] as msg ->
            decode_versions msg rest @@ fun versions ->
            return (Connect (MBytes.to_string gid, Crypto_box.to_public_key public_key, None, versions))
        | [ S 5 ; F rest ] as msg ->
            let rec decode_peers acc = function
              | F [ B addr ; S port ] :: rest -> begin
                  match Ipaddr.of_string @@ MBytes.to_string addr with
                  | Some addr ->
                      decode_peers ((addr, port) :: acc) rest
                  | None ->
                      decode_peers acc rest
                end
              | [] -> Advertise (List.rev acc)
              | _ -> Unknown msg
            in return (decode_peers [] rest)
        | [ S 6 ; F rest ] -> return (Message rest)
        | msg -> return (Unknown msg)

(* send a packet over a TCP socket *)
let send_packet
  : LU.file_descr -> packet -> bool Lwt.t
  = fun socket packet ->
    let frame = match packet with
      | Unknown _ -> assert false (* should never happen *)
      | Disconnect -> [ S 1 ]
      | Ping -> [ S 2 ]
      | Pong -> [ S 12 ]
      | Bootstrap -> [ S 3 ]
      | Connect (gid, public_key, port, versions) ->
          let rec encode = function
            | (name, maj, min) :: tl ->
                let rest = encode tl in
                F [ B (MBytes.of_string name) ; S maj ; S min ] :: rest
            | [] -> []
          in
          [ S 4 ; B (MBytes.of_string gid) ; B (Crypto_box.of_public_key public_key) ]
          @ (match port with | Some port -> [ S port ] | None -> [])
          @ [ F (encode versions) ]
      | Advertise peers ->
          let rec encode = function
            | (addr, port) :: tl ->
                let rest = encode tl in
                F [ B (MBytes.of_string @@ Ipaddr.to_string addr) ; S port ] :: rest
            | [] -> []
          in [ S 5 ; F (encode peers) ]
      | Message message -> [ S 6 ; F message ] in
    Netbits.write socket frame

(* A net handler, as a record-encoded object, abstract from the
   outside world. Hidden Lwt workers are associated to a net at its
   creation and can be killed using the shutdown callback. *)
type net = {
  recv_from : unit -> (peer * Netbits.frame) Lwt.t ;
  send_to : peer * Netbits.frame -> unit Lwt.t ;
  push : peer * Netbits.frame -> unit ;
  broadcast : Netbits.frame -> unit ;
  blacklist : ?duration:float -> addr -> unit ;
  whitelist : peer -> unit ;
  maintain : unit -> unit Lwt.t ;
  roll : unit -> unit Lwt.t ;
  shutdown : unit -> unit Lwt.t ;
  peers : unit -> peer list ;
  peer_info : peer -> addr * port * version ;
}

(* A peer handle, as a record-encoded object, abstract from the
   outside world. A hidden Lwt worker is associated to a peer at its
   creation and is killed using the disconnect callback by net
   workers (on shutdown of during maintenance). *)
and peer = {
  gid : gid ;
  public_key : Crypto_box.public_key ;
  point : point ;
  listening_port : port option ;
  version : version ;
  last_seen : unit -> float ;
  disconnect : unit -> unit Lwt.t;
  send : packet -> unit Lwt.t ;
}

(* The (internal) type of network events, those dispatched from peer
   workers to the net and others internal to net workers. *)
and event =
  | Disconnected of peer
  | Bootstrap of peer
  | Recv of peer * Netbits.frame
  | Peers of point list
  | Contact of point * LU.file_descr
  | Connected of peer
  | Shutdown

(* Run-time point-or-gid indexed storage, one point is bound to at
   most one gid, which is the invariant we want to keep both for the
   connected peers table and the known peers one *)
module GidMap = Map.Make (struct type t = gid let compare = compare end)
module GidSet = Set.Make (struct type t = gid let compare = compare end)
module PointMap = Map.Make (struct type t = point let compare = compare end)
module PointSet = Set.Make (struct type t = point let compare = compare end)
module PeerMap : sig
  type 'a t
  val empty : 'a t
  val by_point : point -> 'a t -> 'a
  val by_gid : gid -> 'a t -> 'a
  val gid_by_point : point -> 'a t -> gid option
  val point_by_gid : gid -> 'a t -> point
  val mem_by_point : point -> 'a t -> bool
  val mem_by_gid : gid -> 'a t -> bool
  val remove_by_point : point -> 'a t -> 'a t
  val remove_by_gid : gid -> 'a t -> 'a t
  val update : point -> ?gid : gid -> 'a -> 'a t -> 'a t
  val fold : (point -> gid option -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (point -> gid option -> 'a -> unit) -> 'a t -> unit
  val bindings : 'a t -> (point * gid option * 'a) list
  val cardinal : 'a t -> int
end = struct
  type 'a t =
    { by_point : (gid option * 'a) PointMap.t ;
      by_gid : (point * 'a) GidMap.t }

  let empty =
    { by_point = PointMap.empty ;
      by_gid = GidMap.empty }

  let by_point point { by_point } =
    let (_, v) = PointMap.find point by_point in v

  let by_gid gid { by_gid } =
    let (_, v) = GidMap.find gid by_gid in v

  let gid_by_point point { by_point } =
    let (gid, _) = PointMap.find point by_point in gid

  let point_by_gid gid { by_gid } =
    let (point, _) = GidMap.find gid by_gid in point

  let mem_by_point point { by_point } =
    PointMap.mem point by_point

  let mem_by_gid gid { by_gid } =
    GidMap.mem gid by_gid

  let remove_by_point point ({ by_point ; by_gid } as map) =
    try
      let (gid, _) = PointMap.find point by_point in
      { by_point = PointMap.remove point by_point ;
        by_gid = match gid with
          | None -> by_gid
          | Some gid -> GidMap.remove gid by_gid }
    with Not_found -> map

  let remove_by_gid gid ({ by_point ; by_gid } as map) =
    try
      let (point, _) = GidMap.find gid by_gid in
      { by_point = PointMap.remove point by_point ;
        by_gid = GidMap.remove gid by_gid }
    with Not_found -> map

  let update point ?gid v map =
    let { by_point ; by_gid } =
      let map = remove_by_point point map in
      match gid with Some gid -> remove_by_gid gid map | None -> map in
    { by_point = PointMap.add point (gid, v) by_point ;
      by_gid = match gid with Some gid -> GidMap.add gid (point, v) by_gid
                            | None -> by_gid }

  let fold f { by_point } init =
    PointMap.fold
      (fun point (gid, v) r -> f point gid v r) by_point init

  let iter f { by_point } =
    PointMap.iter
      (fun point (gid, v) -> f point gid v) by_point

  let cardinal { by_point } =
    PointMap.cardinal by_point

  let bindings map =
    fold (fun point gid v l -> (point, gid, v) :: l) map []
end

(* Builds a peer and launches its associated worker. Takes a push
   function for communicating with the main worker using events
   (including the one sent when the connection is alive). Returns a
   canceler. *)
let connect_to_peer config limits my_gid my_public_key socket (addr, port) push white_listed =
  (* a non exception-based cancelation mechanism *)
  let cancelation, cancel, on_cancel = canceler () in
  (* a cancelable reception *)
  let recv () =
    pick [ recv_packet socket limits.max_packet_size ;
           (cancelation () >>= fun () -> return Disconnect) ] in
  (* First step: send and receive credentials, makes no difference
     whether we're trying to connect to a peer or checking an incoming
     connection, both parties must first present themselves. *)
  let rec connect () =
    send_packet socket (Connect (my_gid,
                                 my_public_key,
                                 config.incoming_port,
                                 config.supported_versions)) >>= fun _ ->
    pick [ (LU.sleep limits.peer_answer_timeout >>= fun () -> return Disconnect) ;
           recv () ] >>= function
    | Connect (gid, public_key , listening_port, versions) ->
        debug "(%a) connection requested from %a @ %a:%d"
          pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
        begin match common_version config.supported_versions versions with
          | None ->
              debug "(%a) connection rejected (incompatible versions) from %a:%d"
                pp_gid my_gid Ipaddr.pp_hum addr port ;
              cancel ()
          | Some version ->
              if config.closed_network then
                match listening_port with
                | Some port when white_listed (addr, port) ->
                    connected version gid public_key listening_port
                | Some port ->
                    debug "(%a) connection rejected (out of the closed network) from %a:%d"
                      pp_gid my_gid Ipaddr.pp_hum addr port ;
                    cancel ()
                | None ->
                    debug "(%a) connection rejected (out of the closed network) from %a:unknown"
                      pp_gid my_gid Ipaddr.pp_hum addr ;
                    cancel ()
              else
                connected version gid public_key listening_port
        end
    | Advertise peers ->
        (* alternatively, one can refuse a connection but reply with
           some peers, so we accept this info *)
        debug "(%a) new peers received from %a:%d"
          pp_gid my_gid Ipaddr.pp_hum addr port ;
        push (Peers peers) ;
        cancel ()
    | Disconnect ->
        debug "(%a) connection rejected (closed by peer or timeout) from %a:%d"
          pp_gid my_gid Ipaddr.pp_hum addr port ;
        cancel ()
    | _ ->
        debug "(%a) connection rejected (bad connection request) from %a:%d"
          pp_gid my_gid Ipaddr.pp_hum addr port ;
        cancel ()
  (* Them we can build the net object and launch the worker. *)
  and connected version gid public_key listening_port =
    (* net object state *)
    let last = ref (Unix.gettimeofday ()) in
    (* net object callbaks *)
    let last_seen () = !last in
    let disconnect () = cancel () in
    let send p = send_packet socket p >>= fun _ -> return () in
    (* net object construction *)
    let peer = { gid ; public_key ; point = (addr, port) ; listening_port ;
                 version ; last_seen ; disconnect ; send } in
    (* The packet reception loop. *)
    let rec receiver () =
      recv () >>= fun packet ->
      last := Unix.gettimeofday () ;
      match packet with
      | Connect _
      | Unknown _ ->
          debug "(%a) disconnected (bad request) %a @ %a:%d"
            pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
          cancel ()
      | Disconnect ->
          debug "(%a) disconnected (by peer) %a @ %a:%d"
            pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
          cancel ()
      | Bootstrap -> push (Bootstrap peer) ; receiver ()
      | Advertise peers -> push (Peers peers) ; receiver ()
      | Ping -> send_packet socket Pong >>= fun _ -> receiver ()
      | Pong -> receiver ()
      | Message msg ->
          push (Recv (peer, msg)) ; receiver ()
    in
    (* The polling loop *)
    let rec pulse_monitor ping =
      pick [ (cancelation () >>= fun () -> return false) ;
             (LU.sleep limits.peer_answer_timeout >>= fun () -> return true)]
      >>= fun continue ->
      if continue then
        match ping with
        | Some tping ->
            if !last -. tping < 0. then begin
              debug "(%a) disconnected (timeout exceeded) %a @ %a:%d"
                pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
              cancel ()
            end else
              pulse_monitor None
        | None ->
            let now = Unix.gettimeofday () in
            if now -. !last < limits.peer_answer_timeout then
              pulse_monitor None
            else
              send_packet socket Ping >>= fun _ ->
              pulse_monitor (Some (Unix.gettimeofday ()))
      else return ()
    in
    (* Events for the main worker *)
    push (Connected peer) ;
    on_cancel (fun () -> push (Disconnected peer) ; return ()) ;
    (* Launch both workers *)
    join [ pulse_monitor None ; receiver () ]
  in
  on_cancel (fun () ->
      send_packet socket Disconnect >>= fun _ ->
      LU.close socket >>= fun _ ->
      return ()) ;
  let worker_name =
    Format.asprintf
      "(%a) connection handler for %a:%d"
      pp_gid my_gid Ipaddr.pp_hum addr port in
  ignore (worker ~safe:true worker_name ~run:connect ~cancel) ;
  (* return the canceler *)
  cancel


(* JSON format for on-disk peers cache file *)
let addr_encoding =
  let open Data_encoding in
  splitted
    ~json:
      (conv Ipaddr.to_string (Data_encoding.Json.wrap_error Ipaddr.of_string_exn) string)
    ~binary:
      (union ~tag_size:`Int8
         [ case ~tag:4
             (Fixed.string 4)
             (fun ip -> Utils.map_option Ipaddr.V4.to_bytes (Ipaddr.to_v4 ip) )
             (fun b -> Ipaddr.(V4 (V4.of_bytes_exn b))) ;
           case ~tag:6
             (Fixed.string 32)
             (fun ip -> Some (Ipaddr.V6.to_bytes (Ipaddr.to_v6 ip)))
             (fun b -> Ipaddr.(V6 (V6.of_bytes_exn b))) ;
         ])

let public_key_encoding =
  let open Data_encoding in
    conv
      (MBytes.to_string << Crypto_box.of_public_key)
      (Crypto_box.to_public_key << MBytes.of_string)
      string

let secret_key_encoding =
  let open Data_encoding in
    conv
      (MBytes.to_string << Crypto_box.of_secret_key)
      (Crypto_box.to_secret_key << MBytes.of_string)
      string

let peers_file_encoding =
  let open Data_encoding in
  obj4
    (req "gid" string)
    (req "public_key" public_key_encoding)
    (req "secret_key" secret_key_encoding)
    (req "peers"
       (obj3
          (req "known"
             (list (obj3
                       (req "addr" addr_encoding)
                       (req "port" int31)
                       (opt "infos"
                          (obj3
                             (req "connections" int31)
                             (req "lastSeen" float)
                             (req "gid" string))))))
          (req "blacklisted"
             (list (obj2
                       (req "addr" addr_encoding)
                       (req "until" float))))
          (req "whitelisted"
             (list (obj2
                       (req "addr" addr_encoding)
                       (req "port" int31))))))

(* Info on peers maintained between connections *)
type source =
  { unreachable_since : float option;
    connections : (int * float) option ;
    white_listed : bool }

(* Ad hoc comparison on sources such as good source < bad source *)
let compare_sources s1 s2 =
  match s1.white_listed, s2.white_listed with
  | true, false -> -1 | false, true -> 1
  | _, _ ->
      match s1.unreachable_since, s2.unreachable_since with
      | None, Some _ -> -1 | Some _, None -> 1
      | _, _ ->
          match s1.connections, s2.connections with
          | Some _, None -> -1 | None, Some _ -> 1 | None, None -> 0
          | Some (n1, t1), Some (n2, t2) ->
              if n1 = n2 then compare t2 t1
              else compare n2 n1

(* A store for blacklisted addresses (we ban any peer on a blacklisted
   address, which is the policy that seems to make the most sense) *)
module BlackList = Map.Make (struct type t = addr let compare = compare end)

(* A good random string so it is probably unique on the network *)
let fresh_gid () =
  Bytes.to_string @@ Sodium.Random.Bytes.generate 16


(* The (fixed size) broadcast frame. *)
let discovery_message gid port =
  Netbits.([ B (MBytes.of_string "DISCO") ; B (MBytes.of_string gid) ; S port ])

(* Broadcast frame verifier. *)
let answerable_discovery_message message my_gid when_ok when_not =
  match message with
  | Some [ B magic ; B gid ; S port ] ->
      if MBytes.to_string magic = "DISCO" && MBytes.to_string gid <> my_gid then
        when_ok gid port
      else when_not ()
  | _ -> when_not ()

let string_of_unix_exn = function
  | Unix.Unix_error (err, fn, _) -> "in " ^ fn ^ ", " ^ Unix.error_message err
  | exn -> Printexc.to_string exn

(* Launch an answer machine for the discovery mechanism, takes a
   callback to fill the answers and returns a canceler function *)
let discovery_answerer my_gid disco_port cancelation callback =
  (* init a UDP listening socket on the broadcast canal *)
  catch
    (fun () ->
       let main_socket = LU.(socket PF_INET6 SOCK_DGRAM 0) in
       LU.(setsockopt main_socket SO_BROADCAST true) ;
       LU.(setsockopt main_socket SO_REUSEADDR true) ;
       LU.(bind main_socket (ADDR_INET (Unix.inet6_addr_any, disco_port))) ;
       return (Some main_socket))
    (fun exn ->
       debug "(%a) will not listen to discovery requests (%s)"
         pp_gid my_gid (string_of_unix_exn exn) ;
       return None) >>= function
  | None -> return ()
  | Some main_socket ->
      (* the answering function *)
      let rec step () =
        let buffer = Netbits.to_raw (discovery_message my_gid 0) in
        let len = MBytes.length buffer in
        pick [ (cancelation () >>= fun () -> return None) ;
               (Lwt_bytes.recvfrom main_socket buffer 0 len [] >>= fun r ->
                return (Some r)) ] >>= function
        | Some (len, LU.ADDR_INET (addr, _)) ->
            if len <> len then
              step () (* drop bytes, better luck next time ! *)
            else
              answerable_discovery_message (Netbits.of_raw buffer) my_gid
                (fun _ port ->
                   catch
                     (fun () ->
                        let socket = LU.(socket PF_INET6 SOCK_STREAM 0) in
                        LU.connect socket LU.(ADDR_INET (addr, port)) >>= fun () ->
                        let addr = Ipaddr_unix.of_inet_addr addr in
                        callback addr port socket >>= fun () ->
                        return ())
                     (fun _ -> (* ignore errors *) return ()) >>= fun () ->
                   step ())
                step
        | Some (_, _) ->
            step ()
        | None -> return ()
      in step ()

(* Sends dicover messages into space in an exponentially delayed loop,
   restartable using a condition *)
let discovery_sender my_gid disco_port inco_port cancelation restart =
  let message = discovery_message my_gid inco_port in
  let rec loop delay n =
    catch
      (fun () ->
         let socket = LU.(socket PF_INET6 SOCK_DGRAM 0) in
         LU.setsockopt socket LU.SO_BROADCAST true ;
         LU.connect socket LU.(ADDR_INET (Unix.inet6_addr_any, disco_port)) >>= fun () ->
         Netbits.(write socket message) >>= fun _ ->
         LU.close socket)
      (fun _ ->
         debug "(%a) error broadcasting a discovery request" pp_gid my_gid ;
         return ()) >>= fun () ->
    pick [ (LU.sleep delay >>= fun () -> return (Some (delay, n + 1))) ;
           (cancelation () >>= fun () -> return None) ;
           (LC.wait restart >>= fun () -> return (Some (0.1, 0))) ] >>= function
      | Some (delay, n) when n = 10 ->
          loop delay 9
      | Some (delay, n) ->
          loop (delay *. 2.) n
      | None -> return ()
  in loop 0.2 1

(* Main network creation and initialisation function *)
let bootstrap config limits =
  (* we need to ignore SIGPIPEs *)
  Sys.(set_signal sigpipe Signal_ignore) ;
  (* a non exception-based cancelation mechanism *)
  let cancelation, cancel, on_cancel = canceler () in
  (* create the internal event queue *)
  let enqueue_event, dequeue_event =
    let queue, enqueue = Lwt_stream.create () in
    (fun msg -> enqueue (Some msg)),
    (fun () -> Lwt_stream.next queue)
  in
  (* create the external message queue *)
  let enqueue_msg, dequeue_msg, close_msg_queue =
    let queue, enqueue = Lwt_stream.create () in
    (fun msg -> enqueue (Some msg)),
    (fun () -> Lwt_stream.next queue),
    (fun () -> enqueue None)
  in
  on_cancel (fun () -> close_msg_queue () ; return ()) ;
  (* fill the known peers pools from last time *)
  Data_encoding.Json.read_file config.peers_file >>= fun res ->
  let known_peers, black_list, my_gid, my_public_key, my_secret_key =
    let init_peers () =
      let my_gid =
        fresh_gid () in
      let (my_secret_key, my_public_key) =
        Crypto_box.random_keypair () in
      let known_peers =
        let source =
          { unreachable_since = None ;
            connections = None ;
            white_listed = true } in
        List.fold_left
          (fun r point -> PeerMap.update point source r)
          PeerMap.empty config.known_peers in
      let black_list =
        BlackList.empty in
      known_peers, black_list, my_gid, my_public_key, my_secret_key in
    match res with
    | None ->
        let known_peers, black_list, my_gid, my_public_key, my_secret_key = init_peers () in
        debug "(%a) peer cache initiated" pp_gid my_gid ;
        ref known_peers, ref black_list, my_gid, my_public_key, my_secret_key
    | Some json ->
        match Data_encoding.Json.destruct peers_file_encoding json with
        | exception _ ->
            let known_peers, black_list, my_gid, my_public_key, my_secret_key = init_peers () in
            debug "(%a) peer cache reset" pp_gid my_gid ;
            ref known_peers, ref black_list, my_gid, my_public_key, my_secret_key
        | (my_gid, my_public_key, my_secret_key, (k, b, w)) ->
            let white_list =
              List.fold_right PointSet.add w PointSet.empty in
            let known_peers =
              List.fold_left
                (fun r (addr, port, infos) ->
                   match infos with
                   | None ->
                       let source =
                         { unreachable_since = None ;
                           connections = None ;
                           white_listed = true } in
                       PeerMap.update (addr, port) source r
                   | Some (c, t, gid) ->
                       let source =
                         { unreachable_since = None ;
                           connections = Some (c, t) ;
                           white_listed = PointSet.mem (addr, port) white_list } in
                       PeerMap.update (addr, port) ~gid source r)
                PeerMap.empty k in
            let black_list =
              List.fold_left
                (fun r (a, d) -> BlackList.add a d r)
                BlackList.empty b in
            debug "(%a) peer cache loaded" pp_gid my_gid ;
            ref known_peers, ref black_list, my_gid, my_public_key, my_secret_key
  in
  (* some peer reachability predicates *)
  let black_listed (addr, _) =
    BlackList.mem addr !black_list in
  let white_listed point =
    try (PeerMap.by_point point !known_peers).white_listed
    with Not_found -> false in
  let grey_listed point =
    try match (PeerMap.by_point point !known_peers).unreachable_since with
      | None -> false | Some t -> Unix.gettimeofday () -. t > 5.
    with Not_found -> false in
  (* save the cache at exit *)
  on_cancel (fun () ->
      (* save the known peers cache *)
      let json =
        Data_encoding.Json.construct peers_file_encoding @@
        (my_gid,
         my_public_key,
         my_secret_key,
         PeerMap.fold
           (fun (addr, port) gid source (k, b, w) ->
              let infos = match gid, source.connections with
                | Some gid, Some (n, t) -> Some (n, t, gid)
                | _ -> None in
              ((addr, port, infos) :: k,
               b,
               if source.white_listed then (addr, port) :: w else w))
           !known_peers ([], BlackList.bindings !black_list, []))
      in
      Data_encoding.Json.write_file config.peers_file json >>= fun _ ->
      debug "(%a) peer cache saved" pp_gid my_gid ;
      return ()) ;
  (* storage of active and not yet active peers *)
  let incoming = ref PointMap.empty in
  let connected = ref PeerMap.empty in
  (* peer welcoming (accept) loop *)
  let welcome () =
    match config.incoming_port with
    | None -> (* no input port => no welcome worker *) return ()
    | Some port ->
        (* open port for incoming connexions *)
        let addr = Unix.inet6_addr_any in
        catch
          (fun () ->
             let main_socket = LU.(socket PF_INET6 SOCK_STREAM 0) in
             LU.(setsockopt main_socket SO_REUSEADDR true) ;
             LU.(bind main_socket (ADDR_INET (addr, port))) ;
             LU.listen main_socket limits.max_connections ;
             return (Some main_socket))
          (fun exn ->
             debug "(%a) cannot accept incoming peers (%s)"
               pp_gid my_gid (string_of_unix_exn exn) ;
             return None)>>= function
        | None ->
            (* FIXME: run in degraded mode, better exit ? *)
            return ()
        | Some main_socket ->
            (* then loop *)
            let rec step () =
              pick [ (LU.accept main_socket >>= fun (s, a) -> return (Some (s, a))) ;
                     (cancelation () >>= fun _ -> return None) ] >>= function
              | None ->
                  LU.close main_socket
              | Some (socket, addr) ->
                  match addr with
                  | LU.ADDR_INET (addr, port) ->
                      let addr = Ipaddr_unix.of_inet_addr addr in
                      enqueue_event (Contact ((addr, port), socket)) ;
                      step ()
                  | _ ->
                      Lwt.async (fun () -> LU.close socket) ;
                      step ()
            in step ()
  in
  (* input maintenance events *)
  let too_many_peers = LC.create () in
  let too_few_peers = LC.create () in
  let new_peer = LC.create () in
  let new_contact = LC.create () in
  let please_maintain = LC.create () in
  let restart_discovery = LC.create () in
  (* output maintenance events *)
  let just_maintained = LC.create () in
  (* maintenance worker, returns when [connections] peers are connected *)
  let rec maintenance () =
    pick [ (LU.sleep 120. >>= fun () -> return true) ; (* every two minutes *)
           (LC.wait please_maintain >>= fun () -> return true) ; (* when asked *)
           (LC.wait too_few_peers >>= fun () -> return true) ; (* limits *)
           (LC.wait too_many_peers >>= fun () -> return true) ;
           (cancelation () >>= fun () -> return false) ] >>= fun continue ->
    let rec maintain () =
      let n_connected = PeerMap.cardinal !connected in
      if n_connected >= limits.expected_connections
      && n_connected <= limits.max_connections then
        (* end of maintenance when enough users have been reached *)
        (LC.broadcast just_maintained () ;
         debug "(%a) maintenance step ended"
           pp_gid my_gid ;
         maintenance ())
      else if n_connected < limits.expected_connections then
        (* too few peers, try and contact many peers *)
        let contact nb =
          let contactable =
            (* we sort sources by level (prefered first) *)
            PeerMap.bindings !known_peers |>
            List.sort (fun (_, _, s1) (_, _, s2) -> compare_sources s1 s2) |>
            (* remove the ones we're connect(ed/ing) to and the blacklisted *)
            List.filter (fun (point, gid, source) ->
                (not (black_listed point) || source.white_listed)
                && not (grey_listed point)
                && not (gid = Some my_gid)
                && not (PeerMap.mem_by_point point !connected)
                && not (PointMap.mem point !incoming)
                && match gid with | None -> true | Some gid ->
                  not (PeerMap.mem_by_gid gid !connected)) in
          let rec do_contact_loop strec =
            match strec with
            | 0, _ -> return true
            | _, [] -> return false (* we didn't manage to contact enough peers *)
            | nb, ((addr, port), gid, source) :: tl ->
                (* we try to open a connection *)
                let socket = LU.(socket (match addr with Ipaddr.V4 _ -> PF_INET | V6 _ -> PF_INET6) SOCK_STREAM 0) in
                let uaddr = Ipaddr_unix.to_inet_addr addr in
                catch
                  (fun () ->
                     lwt_debug "Trying to connect to %a:%d"
                       Ipaddr.pp_hum addr port >>= fun () ->
                     Lwt.pick
                       [ (Lwt_unix.sleep 2.0 >>= fun _ -> Lwt.fail Not_found) ;
                         LU.connect socket (LU.ADDR_INET (uaddr, port))
                       ] >>= fun () ->
                     lwt_debug "Connected to %a:%d"
                       Ipaddr.pp_hum addr port >>= fun () ->
                     enqueue_event (Contact ((addr, port), socket)) ;
                     return (nb - 1))
                  (fun exn ->
                     lwt_debug "Connection failed to %a:%d (%s)"
                       Ipaddr.pp_hum addr port
                       (string_of_unix_exn exn) >>= fun () ->
                     (* if we didn't succes, we greylist it *)
                     let now = Unix.gettimeofday () in
                     known_peers :=
                       PeerMap.update (addr, port) ?gid
                         { source with unreachable_since = Some now }
                         !known_peers ;
                     LU.close socket >>= fun () ->
                     return nb) >>= fun nrec ->
                do_contact_loop (nrec, tl)
          in do_contact_loop (nb, contactable)
        in
        let to_contact = limits.max_connections - n_connected in
        debug "(%a) too few connections (%d)" pp_gid my_gid n_connected ;
        contact to_contact >>= function
        | true -> (* enough contacts, now wait for connections *)
            pick [ (LC.wait new_peer >>= fun _ -> return true) ;
                   (LU.sleep 1.0 >>= fun () -> return true) ;
                   (cancelation () >>= fun () -> return false) ] >>= fun continue ->
            if continue then maintain () else return ()
        | false -> (* not enough contacts, ask the pals of our pals,
                      discover the local network and then wait *)
            LC.broadcast restart_discovery () ;
            (PeerMap.iter
               (fun _ _ peer -> Lwt.async (fun () -> peer.send Bootstrap))
               !connected ;
             pick [ (LC.wait new_peer >>= fun _ -> return true) ;
                    (LC.wait new_contact >>= fun _ -> return true) ;
                    (LU.sleep 1.0 >>= fun () -> return true) ;
                    (cancelation () >>= fun () -> return false) ] >>= fun continue ->
             if continue then maintain () else return ())
      else
        (* too many peers, start the russian roulette *)
        let to_kill = n_connected - limits.max_connections in
        debug "(%a) too many connections, will kill %d" pp_gid my_gid to_kill ;
        snd (PeerMap.fold
               (fun _ _ peer (i, t) ->
                  if i = 0 then (0, t)
                  else (i - 1, t >>= fun () -> peer.disconnect ()))
               !connected (to_kill, return ())) >>= fun () ->
        (* and directly skip to the next maintenance request *)
        LC.broadcast just_maintained () ;
        debug "(%a) maintenance step ended" pp_gid my_gid ;
        maintenance ()
    in
    if continue then maintain () else return ()
  in
  (* select the peers to send on a bootstrap request *)
  let bootstrap_peers () =
    (* we sort peers by desirability *)
    PeerMap.bindings !known_peers |>
    List.filter (fun ((ip,_),_,_) -> not (Ipaddr.is_private ip)) |>
    List.sort (fun (_, _, s1) (_, _, s2) -> compare_sources s1 s2) |>
    (* HERE *)
    (* we simply send the first 50 (or less) known peers *)
    List.fold_left
      (fun (n, l) (point, _, _) -> if n = 0 then (n, l) else (n - 1, point :: l))
      (50, []) |> snd
  in
  (* main internal event handling worker *)
  let rec main () =
    pick [ dequeue_event () ;
           cancelation () >>= fun () -> return Shutdown ] >>= fun event ->
    match event with
    | Disconnected peer ->
        debug "(%a) disconnected peer %a" pp_gid my_gid pp_gid peer.gid ;
        (* remove it from the tables *)
        connected := PeerMap.remove_by_point peer.point !connected ;
        if PeerMap.cardinal !connected < limits.min_connections then
          LC.broadcast too_few_peers () ;
        incoming := PointMap.remove peer.point !incoming ;
        main ()
    | Connected peer ->
        incoming := PointMap.remove peer.point !incoming ;
        let update_infos () =
          (* we update our knowledge table according to the
               reachable address given by the peer *)
          match peer.listening_port with
          | None -> ()
          | Some port ->
              let point = (fst peer.point, port) in
              let update source =
                (* delete previous infos about this address / gid *)
                known_peers := PeerMap.remove_by_point point !known_peers ;
                known_peers := PeerMap.remove_by_gid peer.gid !known_peers ;
                (* then assign *)
                known_peers := PeerMap.update point ~gid:peer.gid source !known_peers
              in update @@
              try match PeerMap.by_gid peer.gid !known_peers with
                | { connections = None ; white_listed } ->
                    { connections = Some (1, Unix.gettimeofday ()) ;
                      unreachable_since = None ;
                      white_listed }
                | { connections = Some (n, _) ; white_listed } ->
                    { connections = Some (n + 1, Unix.gettimeofday ()) ;
                      unreachable_since = None ;
                      white_listed}
              with Not_found ->
                { connections = Some (1, Unix.gettimeofday ()) ;
                  unreachable_since = None ;
                  white_listed = white_listed point }
        in
        (* if it's me, it's probably not me *)
        if my_gid = peer.gid then begin
          debug "(%a) rejected myself from %a:%d"
            pp_gid my_gid Ipaddr.pp_hum (fst peer.point) (snd peer.point) ;
          (* now that I know my address, I can save this info to
             prevent future reconnections to myself *)
          update_infos () ;
          Lwt.async peer.disconnect
        end
        (* keep only one connection to each node by checking its gid *)
        else if PeerMap.mem_by_gid peer.gid !connected then begin
          debug "(%a) rejected already connected peer %a @ %a:%d"
            pp_gid my_gid pp_gid peer.gid
            Ipaddr.pp_hum (fst peer.point) (snd peer.point) ;
          update_infos () ;
          Lwt.async peer.disconnect
        end else begin
          debug "(%a) connected peer %a @ %a:%d"
            pp_gid my_gid pp_gid peer.gid
            Ipaddr.pp_hum (fst peer.point) (snd peer.point) ;
          update_infos () ;
          connected :=
            PeerMap.update peer.point ~gid:peer.gid peer !connected ;
          if PeerMap.cardinal !connected > limits.max_connections then
            LC.broadcast too_many_peers () ;
          LC.broadcast new_peer peer
        end ;
        main ()
    | Contact ((addr, port), socket) ->
        (* we do not check the credentials at this stage, since they
           could change from one connection to the next *)
        if PointMap.mem (addr, port) !incoming
        || PeerMap.mem_by_point (addr, port) !connected
        || BlackList.mem addr !black_list then
          LU.close socket >>= fun () ->
          main ()
        else
          let canceler =
            connect_to_peer config limits my_gid my_public_key socket (addr, port) enqueue_event white_listed in
          debug "(%a) incoming peer at %a:%d"
            pp_gid my_gid Ipaddr.pp_hum addr port ;
          incoming := PointMap.add (addr, port) canceler !incoming ;
          main ()
    | Bootstrap peer ->
        let sample = bootstrap_peers () in
        Lwt.async (fun () -> peer.send (Advertise sample)) ;
        main ()
    | Recv (peer, message) ->
        enqueue_msg (peer, message) ;
        main ()
    | Peers peers ->
        List.iter
          (fun point ->
             if not (PeerMap.mem_by_point point !known_peers) then
               let source =
                 { unreachable_since = None ;
                   connections = None ;
                   white_listed = false } in
               known_peers := PeerMap.update point source !known_peers ;
               LC.broadcast new_contact point)
          peers ;
        main ()
    | Shutdown ->
        return ()
  in
  (* blacklist filter *)
  let rec unblock () =
    pick [ (Lwt_unix.sleep 20. >>= fun _ -> return true) ;
           (cancelation () >>= fun () -> return false) ] >>= fun continue ->
    if continue then
      let now = Unix.gettimeofday () in
      black_list := BlackList.fold
          (fun addr d map -> if d < now then map else BlackList.add addr d map)
          !black_list BlackList.empty ;
      known_peers :=
        PeerMap.fold (fun point gid source map ->
            let source =
              match source.unreachable_since with
              | Some t when now -. t < 20. -> source
              | _ -> { source with unreachable_since = None } in
            PeerMap.update point ?gid source map)
          !known_peers PeerMap.empty ;
      unblock ()
    else return ()
  in
  (* launch all workers *)
  let welcome = worker (Format.asprintf "(%a) welcome" pp_gid my_gid) welcome cancel in
  let maintenance = worker (Format.asprintf "(%a) maintenance" pp_gid my_gid) maintenance cancel in
  let main = worker (Format.asprintf "(%a) reception" pp_gid my_gid) main cancel in
  let unblock = worker (Format.asprintf "(%a) unblacklister" pp_gid my_gid) unblock cancel in
  let discovery_answerer =
    match config.discovery_port with
    | Some disco_port ->
        let answerer () =
          discovery_answerer my_gid disco_port cancelation @@ fun addr port socket ->
          (* do not reply to ourselves or conncted peers *)
          if not (PeerMap.mem_by_point (addr, port) !connected)
          && (try match PeerMap.gid_by_point (addr, port) !known_peers with
              | Some gid -> not (PeerMap.mem_by_gid gid !connected)
                            && not (my_gid = gid)
              | None -> true with Not_found -> true) then
            (* either reply by a list of peer or connect if we need peers *)
            if PeerMap.cardinal !connected >= limits.expected_connections then begin
              enqueue_event (Peers [ addr, port ]) ;
              send_packet socket (Advertise (bootstrap_peers ())) >>= fun _ ->
              LU.close socket
            end else begin
              enqueue_event (Contact ((addr, port), socket)) ;
              return ()
            end
          else LU.close socket in
        worker (Format.asprintf "(%a) discovery answerer" pp_gid my_gid) answerer cancel
    | _ -> return ()  in
  let discovery_sender =
    match config.incoming_port, config.discovery_port with
    | Some inco_port, Some disco_port ->
        let sender () =
          discovery_sender my_gid disco_port inco_port cancelation restart_discovery in
        worker (Format.asprintf "(%a) discovery sender" pp_gid my_gid) sender cancel
    | _ -> return ()  in
  (* net manipulation callbacks *)
  let rec shutdown () =
    debug "(%a) starting network shutdown" pp_gid my_gid ;
    (* stop accepting clients *)
    cancel () >>= fun () ->
    (* wait for both workers to end *)
    join [ welcome ; main ; maintenance ; unblock ;
           discovery_answerer ; discovery_sender ] >>= fun () ->
    (* properly shutdown all peers *)
    let cancelers =
      PeerMap.fold
        (fun point _ peer res ->
           (peer.disconnect () >>= fun () ->
            connected := PeerMap.remove_by_point point !connected ;
            return ()) :: res)
        !connected @@
      PointMap.fold
        (fun point canceler res ->
           (canceler () >>= fun () ->
            incoming := PointMap.remove point !incoming ;
            return ()) :: res)
        !incoming @@ []
    in
    join cancelers >>= fun () ->
    debug "(%a) network shutdown complete" pp_gid my_gid ;
    return ()
  and peers () =
    PeerMap.fold (fun _ _ peer r -> peer :: r) !connected []
  and peer_info peer =
    fst peer.point, snd peer.point, peer.version
  and recv_from () =
    dequeue_msg ()
  and send_to (peer, msg) =
    peer.send (Message msg) >>= fun _ -> return ()
  and push (peer, msg) =
    Lwt.async (fun () -> peer.send (Message msg))
  and broadcast msg =
    PeerMap.iter
      (fun _ _ peer ->
         Lwt.async (fun () -> peer.send (Message msg)))
      !connected
  and blacklist ?(duration = limits.blacklist_time) addr =
    let t = Unix.gettimeofday () +. duration in
    black_list := BlackList.add addr t !black_list ;
    debug "(%a) address %a blacklisted" pp_gid my_gid Ipaddr.pp_hum addr ;
    (* we ban this peer, but also all the ones at this address, even
       when whitelisted (the blacklist operation wins) *)
    known_peers :=
      PeerMap.fold
        (fun ((a, _) as point) gid p map ->
           if a = addr then map else PeerMap.update point ?gid p map)
        !known_peers PeerMap.empty ;
    (* we disconnect all peers at this address sur-le-champ *)
    PeerMap.iter
      (fun (a, _) _ p -> if addr = a then
          Lwt.async (fun () -> p.disconnect ()))
      !connected ;
    (* and prevent incoming connections *)
    PointMap.iter
      (fun (a, _) cancel -> if a = addr then Lwt.async cancel)
      !incoming

  and whitelist_point point =
    let source, gid = try
        { (PeerMap.by_point point !known_peers)
          with white_listed = true },
        PeerMap.gid_by_point point !known_peers
      with Not_found ->
        { unreachable_since = None ;
          connections = None ;
          white_listed = true },
        None in
    known_peers := PeerMap.update point ?gid source !known_peers
  and whitelist peer =
    (* we promote this peer to the white list, if reachable *)
    match peer.listening_port with
    | Some port ->
        let point = fst peer.point, port in
        whitelist_point point
    | None -> ()

  and maintain () =
    let waiter = LC.wait just_maintained in
    LC.broadcast please_maintain () ;
    waiter
  and roll () = Pervasives.failwith "roll"
  in
  let net = { shutdown ; peers ; recv_from ; send_to ; push ; broadcast ;
              blacklist ; whitelist ; maintain ; roll ; peer_info } in
  (* main thread, returns after first successful maintenance *)
  maintain () >>= fun () ->
  debug "(%a) network succesfully bootstrapped" pp_gid my_gid ;
  return net

let faked_network =
  let infinity, wakeup = Lwt.wait () in
  let shutdown () =
    Lwt.wakeup_exn wakeup Lwt_stream.Empty;
    Lwt.return_unit in
  let peers () = [] in
  let recv_from () = infinity in
  let send_to _ = Lwt.return_unit in
  let push _ = () in
  let broadcast _ = () in
  let blacklist ?duration _ = ignore duration ; () in
  let whitelist _ = () in
  let maintain () = Lwt.return_unit in
  let roll () = Lwt.return_unit in
  let peer_info _ = assert false in
  { shutdown ; peers ; recv_from ; send_to ; push ; broadcast ;
    blacklist ; whitelist ; maintain ; roll ; peer_info }


(* Plug toplevel functions to callback calls. *)
let shutdown net = net.shutdown ()
let peers net = net.peers ()
let peer_info peer net = net.peer_info peer
let recv net = net.recv_from ()
let send (peer, msg) net = net.send_to (peer, msg)
let push peer net = net.push peer
let broadcast msg net = net.broadcast msg
let maintain net = net.maintain ()
let roll net = net.roll ()
let blacklist ?duration peer net = net.blacklist ?duration peer
let whitelist peer net = net.whitelist peer
