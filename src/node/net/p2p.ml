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

open Lwt.Infix
open Logging.Net

(* public types *)
type addr = Ipaddr.t
type port = int
type version = {
  name : string ;
  major : int ;
  minor : int ;
}

let version_encoding =
  let open Data_encoding in
  conv
    (fun { name; major; minor } -> (name, major, minor))
    (fun (name, major, minor) -> { name; major; minor })
    (obj3
       (req "name" string)
       (req "major" int8)
       (req "minor" int8))

type limits = {
  max_message_size : int ;
  peer_answer_timeout : float ;
  expected_connections : int ;
  min_connections : int ;
  max_connections : int ;
  blacklist_time : float ;
}
type config = {
  incoming_port : port option ;
  discovery_port : port option ;
  known_peers : (addr * port) list ;
  peers_file : string ;
  closed_network : bool ;
}

(* The global net identificator. *)
type gid = string

let gid_length = 16

let pp_gid ppf gid =
  Format.pp_print_string ppf (Hex_encode.hex_encode gid)

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

(* A net point (address x port). *)
type point = addr * port

let point_encoding =
  let open Data_encoding in
  let open Ipaddr in
  conv
    (fun (addr, port) ->
       (match addr with
        | V4 v4 -> V4.to_bytes v4
        | V6 v6 -> V6.to_bytes v6), port)
    (fun (addr, port) ->
       (match String.length addr with
        | 4 -> V4 (V4.of_bytes_exn addr)
        | 16 -> V6 (V6.of_bytes_exn addr)
        | _ -> Pervasives.failwith "point_encoding"), port)
    (obj2
       (req "addr" string)
       (req "port" int16))

type 'msg encoding = Encoding : {
    tag: int ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg encoding

module type PARAMS = sig

  (** Type of message used by higher layers *)
  type msg

  val encodings : msg encoding list

  (** Type of metadata associated to an identity *)
  type metadata

  val initial_metadata : metadata
  val metadata_encoding : metadata Data_encoding.t
  val score : metadata -> float

  (** High level protocol(s) talked by the peer. When two peers
      initiate a connection, they exchange their list of supported
      versions. The chosen one, if any, is the maximum common one (in
      lexicographic order) *)
  val supported_versions : version list

end

module Make (P: PARAMS) = struct

  (* Low-level network protocol messages (internal). The protocol is
     completely symmetrical and asynchronous. First both peers must
     present their credentials with a [Connect] message, then any
     combination of the other messages can be received at any time. An
     exception is the [Disconnect] message, which should mark the end of
     transmission (and needs not being replied). *)
  type msg =
    | Connect of {
        gid : string ;
        port : int option ;
        versions : version list ;
        public_key : Crypto_box.public_key ;
        proof_of_work : Crypto_box.nonce ;
        message_nonce : Crypto_box.nonce ;
      }
    | Disconnect
    | Bootstrap
    | Advertise of point list
    | Message of P.msg

  let msg_encoding =
    let open Data_encoding in
    union ~tag_size:`Uint16
      ([ case ~tag:0x00
           (obj6
              (req "gid" (Fixed.string gid_length))
              (req "port" uint16)
              (req "pubkey" Crypto_box.public_key_encoding)
              (req "proof_of_work" Crypto_box.nonce_encoding)
              (req "message_nonce" Crypto_box.nonce_encoding)
              (req "versions" (Variable.list version_encoding)))
           (function
             | Connect { gid ; port ; public_key ;
                         proof_of_work ; message_nonce ; versions } ->
                 let port = match port with None -> 0 | Some port -> port in
                 Some (gid, port, public_key,
                       proof_of_work, message_nonce, versions)
             | _ -> None)
           (fun (gid, port, public_key,
                 proof_of_work, message_nonce, versions) ->
             let port = if port = 0 then None else Some port in
             Connect { gid ; port ; versions ;
                       public_key ; proof_of_work ; message_nonce });
         case ~tag:0x01 null
           (function Disconnect -> Some () | _ -> None)
           (fun () -> Disconnect);
         case ~tag:0x02 null
           (function Bootstrap -> Some () | _ -> None)
           (fun () -> Bootstrap);
         case ~tag:0x03 (Variable.list point_encoding)
           (function Advertise points -> Some points | _ -> None)
           (fun points -> Advertise points);
       ] @
       ListLabels.map P.encodings
         ~f:(function Encoding { tag ; encoding ; wrap ; unwrap } ->
             case ~tag encoding
               (function Message msg -> unwrap msg | _ -> None)
               (fun msg -> Message (wrap msg))))

  let hdrlen = 2
  let maxlen = hdrlen + 2 lsl 16

  (* read a message from a TCP socket *)
  let recv_msg ?(uncrypt = (fun buf -> Some buf)) fd buf =
    Lwt.catch begin fun () ->
      assert (MBytes.length buf >= 2 lsl 16) ;
      Lwt_utils.read_mbytes ~len:hdrlen fd buf >>= fun () ->
      let len = EndianBigstring.BigEndian.get_uint16 buf 0 in
      (* TODO timeout read ??? *)
      Lwt_utils.read_mbytes ~len fd buf >>= fun () ->
      let buf = MBytes.sub buf 0 len in
      match uncrypt buf with
      | None ->
          (* TODO track invalid message *)
          Lwt.return Disconnect
      | Some buf ->
          match Data_encoding.Binary.of_bytes msg_encoding buf with
          | None ->
              (* TODO track invalid message *)
              Lwt.return Disconnect
          | Some msg ->
              Lwt.return msg
    end
      (function
        | Unix.Unix_error _ | End_of_file -> Lwt.return Disconnect
        | e -> Lwt.fail e)

  (* send a message over a TCP socket *)
  let send_msg ?crypt fd buf msg =
    Lwt.catch begin fun () ->
      match Data_encoding.Binary.write msg_encoding msg buf hdrlen with
      | None -> Lwt.return_false
      | Some len ->
          match crypt with
          | None ->
              if len > maxlen then
                Lwt.return_false
              else begin
                EndianBigstring.BigEndian.set_int16 buf 0 (len - hdrlen) ;
                (* TODO timeout write ??? *)
                Lwt_utils.write_mbytes ~len fd buf >>= fun () ->
                Lwt.return_true
              end
          | Some crypt ->
              let encbuf = crypt (MBytes.sub buf hdrlen (len - hdrlen)) in
              let len = MBytes.length encbuf in
              if len > maxlen then
                Lwt.return_false
              else begin
                let lenbuf = MBytes.create 2 in
                EndianBigstring.BigEndian.set_int16 lenbuf 0 len ;
                Lwt_utils.write_mbytes fd lenbuf >>= fun () ->
                Lwt_utils.write_mbytes fd encbuf >>= fun () ->
                Lwt.return_true
              end
    end
      (function
        | Unix.Unix_error _ | End_of_file -> Lwt.return_false
        | e -> Lwt.fail e)

  (* The (internal) type of network events, those dispatched from peer
     workers to the net and others internal to net workers. *)
  type event =
    | Disconnected of peer
    | Bootstrap of peer
    | Recv of peer * P.msg
    | Peers of point list
    | Contact of point * LU.file_descr
    | Connected of peer
    | Shutdown

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
    send : msg -> unit Lwt.t ;
  }

  type peer_info = {
    gid : gid ;
    addr : addr ;
    port : port ;
    version : version ;
  }

  (* A net handler, as a record-encoded object, abstract from the
     outside world. Hidden Lwt workers are associated to a net at its
     creation and can be killed using the shutdown callback. *)
  type net = {
    recv_from : unit -> (peer * P.msg) Lwt.t ;
    send_to : peer -> P.msg -> unit Lwt.t ;
    try_send_to : peer -> P.msg -> bool ;
    broadcast : P.msg -> unit ;
    blacklist : ?duration:float -> addr -> unit ;
    whitelist : peer -> unit ;
    maintain : unit -> unit Lwt.t ;
    roll : unit -> unit Lwt.t ;
    shutdown : unit -> unit Lwt.t ;
    peers : unit -> peer list ;
    find_peer : gid -> peer option ;
    peer_info : peer -> peer_info ;
    set_metadata : gid -> P.metadata -> unit ;
    get_metadata : gid -> P.metadata option ;
  }

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
  let connect_to_peer
      config limits my_gid my_public_key my_secret_key my_proof_of_work
      socket (addr, port) push white_listed =
    (* a non exception-based cancelation mechanism *)
    let cancelation, cancel, on_cancel = Lwt_utils.canceler () in
    (* a cancelable encrypted reception *)
    let recv ~uncrypt buf =
      Lwt.pick [ recv_msg ~uncrypt socket buf ;
                 (cancelation () >>= fun () -> Lwt.return Disconnect) ] in
    (* First step: send and receive credentials, makes no difference
       whether we're trying to connect to a peer or checking an incoming
       connection, both parties must first present themselves. *)
    let rec connect buf =
      let local_nonce = Crypto_box.random_nonce () in
      send_msg socket buf
        (Connect { gid = my_gid ;
                   public_key = my_public_key ;
                   proof_of_work = my_proof_of_work ;
                   message_nonce = local_nonce ;
                   port = config.incoming_port ;
                   versions = P.supported_versions }) >>= fun _ ->
      Lwt.pick
        [ ( LU.sleep limits.peer_answer_timeout >>= fun () ->
            Lwt.return Disconnect ) ;
          recv_msg socket buf ] >>= function
      | Connect { gid; port = listening_port; versions ; public_key ;
                  proof_of_work ; message_nonce } ->
          debug "(%a) connection requested from %a @@ %a:%d"
            pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
          let work_proved =
            Crypto_box.check_proof_of_work
              public_key proof_of_work Crypto_box.default_target in
          if not work_proved then begin
            debug "connection rejected (invalid proof of work)" ;
            cancel ()
          end else begin
            match common_version P.supported_versions versions with
            | None ->
                debug
                  "(%a) connection rejected (incompatible versions) from %a:%d"
                  pp_gid my_gid Ipaddr.pp_hum addr port ;
                cancel ()
            | Some version ->
                if config.closed_network then
                  match listening_port with
                  | Some port when white_listed (addr, port) ->
                      connected
                        buf local_nonce version gid
                        public_key message_nonce listening_port
                  | Some port ->
                      debug
                        "(%a) connection rejected (out of the closed network) from %a:%d"
                        pp_gid my_gid Ipaddr.pp_hum addr port ;
                      cancel ()
                  | None ->
                      debug
                        "(%a) connection rejected (out of the closed network) from %a:unknown"
                        pp_gid my_gid Ipaddr.pp_hum addr ;
                      cancel ()
                else
                  connected
                    buf local_nonce version gid
                    public_key message_nonce listening_port
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
    and connected buf local_nonce version gid public_key nonce listening_port =
      (* net object state *)
      let last = ref (Unix.gettimeofday ()) in
      let local_nonce = ref local_nonce in
      let remote_nonce = ref nonce in
      (* net object callbaks *)
      let last_seen () = !last in
      let get_nonce nonce =
        let current_nonce = !nonce in
        nonce := Crypto_box.increment_nonce !nonce ;
        current_nonce in
      let disconnect () = cancel () in
      let crypt buf =
        let nonce = get_nonce remote_nonce in
        Crypto_box.box my_secret_key public_key buf nonce in
      let send p = send_msg ~crypt socket buf p >>= fun _ -> Lwt.return_unit in
      (* net object construction *)
      let peer = { gid ; public_key ; point = (addr, port) ;
                   listening_port ; version ; last_seen ; disconnect ; send } in
      let uncrypt buf =
        let nonce = get_nonce local_nonce in
        match Crypto_box.box_open my_secret_key public_key buf nonce with
        | None ->
            debug "(%a) cannot decrypt message (from peer) %a @ %a:%d"
              pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
            None
        | Some _ as res -> res in
      (* The message reception loop. *)
      let rec receiver () =
        recv ~uncrypt buf >>= fun message ->
        last := Unix.gettimeofday () ;
        match message with
        | Connect _
        | Disconnect ->
            debug "(%a) disconnected (by peer) %a @@ %a:%d"
              pp_gid my_gid pp_gid gid Ipaddr.pp_hum addr port ;
            cancel ()
        | Bootstrap -> push (Bootstrap peer) ; receiver ()
        | Advertise peers -> push (Peers peers) ; receiver ()
        | Message msg -> push (Recv (peer, msg)) ; receiver ()
      in
      (* Events for the main worker *)
      push (Connected peer) ;
      on_cancel (fun () -> push (Disconnected peer) ; Lwt.return_unit) ;
      (* Launch the worker *)
      receiver ()
    in
    let buf = MBytes.create maxlen in
    on_cancel (fun () ->
        (* send_msg ~crypt socket buf Disconnect >>= fun _ -> *)
        LU.close socket >>= fun _ ->
        Lwt.return_unit) ;
    let worker_name =
      Format.asprintf
        "(%a) connection handler for %a:%d"
        pp_gid my_gid Ipaddr.pp_hum addr port in
    ignore (Lwt_utils.worker worker_name
              ~safe:true ~run:(fun () -> connect buf) ~cancel) ;
    (* return the canceler *)
    cancel


  (* JSON format for on-disk peers cache file *)
  let addr_encoding =
    let open Data_encoding in
    splitted
      ~json:
        (conv
           Ipaddr.to_string
           (Data_encoding.Json.wrap_error Ipaddr.of_string_exn)
           string)
      ~binary:
        (union ~tag_size:`Uint8
           [ case ~tag:4
               (Fixed.string 4)
               (fun ip -> Utils.map_option Ipaddr.V4.to_bytes (Ipaddr.to_v4 ip) )
               (fun b -> Ipaddr.(V4 (V4.of_bytes_exn b))) ;
             case ~tag:6
               (Fixed.string 32)
               (fun ip -> Some (Ipaddr.V6.to_bytes (Ipaddr.to_v6 ip)))
               (fun b -> Ipaddr.(V6 (V6.of_bytes_exn b))) ;
           ])

  let peers_file_encoding =
    let open Data_encoding in
    obj5
      (req "gid" string)
      (req "public_key" Crypto_box.public_key_encoding)
      (req "secret_key" Crypto_box.secret_key_encoding)
      (req "proof_of_work" Crypto_box.nonce_encoding)
      (req "peers"
         (obj3
            (req "known"
               (list (obj3
                        (req "addr" addr_encoding)
                        (req "port" int31)
                        (opt "infos"
                           (obj4
                              (req "connections" int31)
                              (req "lastSeen" float)
                              (req "gid" string)
                              (req "public_key"
                                 Crypto_box.public_key_encoding))))))
            (req "blacklisted"
               (list (obj2
                        (req "addr" addr_encoding)
                        (req "until" float))))
            (req "whitelisted"
               (list (obj2
                        (req "addr" addr_encoding)
                        (req "port" int31))))))

  (* Info on peers maintained between connections *)
  type source = {
    unreachable_since : float option;
    connections : (int * float * Crypto_box.public_key) option ;
    white_listed : bool ;
    meta : P.metadata ;
  }

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
            | Some (n1, t1, _), Some (n2, t2, _) ->
                if n1 = n2 then compare t2 t1
                else compare n2 n1

  (* A store for blacklisted addresses (we ban any peer on a blacklisted
     address, which is the policy that seems to make the most sense) *)
  module BlackList = Map.Make (struct type t = addr let compare = compare end)

  (* A good random string so it is probably unique on the network *)
  let fresh_gid () =
    Bytes.to_string @@ Sodium.Random.Bytes.generate gid_length

  (* The (fixed size) broadcast frame. *)
  let discovery_message_encoding =
    let open Data_encoding in
    tup3 (Fixed.string 8) (Fixed.string gid_length) int16

  let discovery_message gid port =
    Data_encoding.Binary.to_bytes
      discovery_message_encoding
      ("DISCOVER", gid, port)

  (* Broadcast frame verifier. *)
  let answerable_discovery_message msg my_gid when_ok when_not =
    match msg with
    | Some ("DISCOVER", gid, port) when gid <> my_gid -> when_ok gid port
    | _ -> when_not ()

  let string_of_unix_exn = function
    | Unix.Unix_error (err, fn, _) -> "in " ^ fn ^ ", " ^ Unix.error_message err
    | exn -> Printexc.to_string exn

  (* Launch an answer machine for the discovery mechanism, takes a
     callback to fill the answers and returns a canceler function *)
  let discovery_answerer my_gid disco_port cancelation callback =
    (* init a UDP listening socket on the broadcast canal *)
    Lwt.catch begin fun () ->
      let main_socket = LU.(socket PF_INET SOCK_DGRAM 0) in
      LU.(setsockopt main_socket SO_BROADCAST true) ;
      LU.(setsockopt main_socket SO_REUSEADDR true) ;
      LU.(bind main_socket (ADDR_INET (Unix.inet_addr_any, disco_port))) ;
      Lwt.return (Some main_socket)
    end
      (fun exn ->
         debug "(%a) will not listen to discovery requests (%s)"
           pp_gid my_gid (string_of_unix_exn exn) ;
         Lwt.return_none) >>= function
    | None -> Lwt.return_unit
    | Some main_socket ->
        (* the answering function *)
        let rec step () =
          let buffer = discovery_message my_gid 0 in
          let len = MBytes.length buffer in
          Lwt.pick
            [ (cancelation () >>= fun () -> Lwt.return_none) ;
              (Lwt_bytes.recvfrom main_socket buffer 0 len [] >>= fun r ->
               Lwt.return (Some r)) ] >>= function
          | None -> Lwt.return_unit
          | Some (len', LU.ADDR_INET (addr, _)) when len' = len ->
              answerable_discovery_message
                (Data_encoding.Binary.of_bytes
                   discovery_message_encoding buffer)
                my_gid
                (fun _ port ->
                   Lwt.catch begin fun () ->
                     let ipaddr =
                       let open Ipaddr in
                       match Ipaddr_unix.of_inet_addr addr with
                       | V4 addr -> V6 (v6_of_v4 addr)
                       | V6 _ as addr -> addr in
                     let addr = Ipaddr_unix.to_inet_addr ipaddr in
                     let socket = LU.(socket PF_INET6 SOCK_STREAM 0) in
                     LU.connect socket LU.(ADDR_INET (addr, port)) >>= fun () ->
                     callback ipaddr port socket >>= fun () ->
                     Lwt.return_unit
                   end
                     (fun _ -> (* ignore errors *) Lwt.return_unit) >>= fun () ->
                   step ())
                step
          | Some _ -> step ()
        in step ()

  (* Sends dicover messages into space in an exponentially delayed loop,
     restartable using a condition *)
  let discovery_sender my_gid disco_port inco_port cancelation restart =
    let msg = discovery_message my_gid inco_port in
    let rec loop delay n =
      Lwt.catch begin fun () ->
        let socket = LU.(socket PF_INET SOCK_DGRAM 0) in
        LU.setsockopt socket LU.SO_BROADCAST true ;
        let broadcast_ipv4 = Unix.inet_addr_of_string "255.255.255.255" in
        LU.connect socket
          LU.(ADDR_INET (broadcast_ipv4, disco_port)) >>= fun () ->
        Lwt_utils.write_mbytes socket msg >>= fun _ ->
        LU.close socket
      end
        (fun _ ->
           debug "(%a) error broadcasting a discovery request" pp_gid my_gid ;
           Lwt.return_unit) >>= fun () ->
      Lwt.pick
        [ (LU.sleep delay >>= fun () -> Lwt.return (Some (delay, n + 1))) ;
          (cancelation () >>= fun () -> Lwt.return_none) ;
          (LC.wait restart >>= fun () -> Lwt.return (Some (0.1, 0))) ]
      >>= function
      | Some (delay, n) when n = 10 ->
          loop delay 9
      | Some (delay, n) ->
          loop (delay *. 2.) n
      | None -> Lwt.return_unit
    in loop 0.2 1

  (* Main network creation and initialisation function *)
  let bootstrap ~config ~limits =
    (* we need to ignore SIGPIPEs *)
    Sys.(set_signal sigpipe Signal_ignore) ;
    (* a non exception-based cancelation mechanism *)
    let cancelation, cancel, on_cancel = Lwt_utils.canceler () in
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
    on_cancel (fun () -> close_msg_queue () ; Lwt.return_unit) ;
    (* fill the known peers pools from last time *)
    Data_encoding.Json.read_file config.peers_file >>= fun res ->
    let known_peers, black_list, my_gid,
        my_public_key, my_secret_key, my_proof_of_work =
      let init_peers () =
        let my_gid =
          fresh_gid () in
        let (my_secret_key, my_public_key) =
          Crypto_box.random_keypair () in
        let my_proof_of_work =
          Crypto_box.generate_proof_of_work
            my_public_key Crypto_box.default_target in
        let known_peers =
          let source = { unreachable_since = None ;
                         connections = None ;
                         white_listed = true ;
                         meta = P.initial_metadata ;
                       }
          in
          List.fold_left
            (fun r point -> PeerMap.update point source r)
            PeerMap.empty config.known_peers in
        let black_list =
          BlackList.empty in
        known_peers, black_list, my_gid,
        my_public_key, my_secret_key, my_proof_of_work in
      match res with
      | None ->
          let known_peers, black_list, my_gid,
              my_public_key, my_secret_key, my_proof_of_work = init_peers () in
          debug "(%a) peer cache initiated" pp_gid my_gid ;
          ref known_peers, ref black_list, my_gid,
          my_public_key, my_secret_key, my_proof_of_work
      | Some json ->
          match Data_encoding.Json.destruct peers_file_encoding json with
          | exception _ ->
              let known_peers, black_list, my_gid,
                  my_public_key, my_secret_key, my_proof_of_work = init_peers () in
              debug "(%a) peer cache reset" pp_gid my_gid ;
              ref known_peers, ref black_list,
              my_gid, my_public_key, my_secret_key, my_proof_of_work
          | (my_gid, my_public_key, my_secret_key, my_proof_of_work, (k, b, w)) ->
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
                             white_listed = true ;
                               meta = P.initial_metadata ; } in
                         PeerMap.update (addr, port) source r
                     | Some (c, t, gid, pk) ->
                         let source =
                           { unreachable_since = None ;
                             connections = Some (c, t, pk) ;
                             white_listed = PointSet.mem (addr, port) white_list ;
                             meta = P.initial_metadata ; } in
                         PeerMap.update (addr, port) ~gid source r)
                  PeerMap.empty k in
              let black_list =
                List.fold_left
                  (fun r (a, d) -> BlackList.add a d r)
                  BlackList.empty b in
              debug "(%a) peer cache loaded" pp_gid my_gid ;
              ref known_peers, ref black_list,
              my_gid, my_public_key, my_secret_key, my_proof_of_work
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
           my_proof_of_work,
           PeerMap.fold
             (fun (addr, port) gid source (k, b, w) ->
                let infos = match gid, source.connections with
                  | Some gid, Some (n, t, pk) -> Some (n, t, gid, pk)
                  | _ -> None in
                ((addr, port, infos) :: k,
                 b,
                 if source.white_listed then (addr, port) :: w else w))
             !known_peers ([], BlackList.bindings !black_list, []))
        in
        Data_encoding.Json.write_file config.peers_file json >>= fun _ ->
        debug "(%a) peer cache saved" pp_gid my_gid ;
        Lwt.return_unit) ;
    (* storage of active and not yet active peers *)
    let incoming = ref PointMap.empty in
    let connected = ref PeerMap.empty in
    (* peer welcoming (accept) loop *)
    let welcome () =
      match config.incoming_port with
      | None -> (* no input port => no welcome worker *) Lwt.return_unit
      | Some port ->
          (* open port for incoming connexions *)
          let addr = Unix.inet6_addr_any in
          Lwt.catch begin fun () ->
            let main_socket = LU.(socket PF_INET6 SOCK_STREAM 0) in
            LU.(setsockopt main_socket SO_REUSEADDR true) ;
            LU.(bind main_socket (ADDR_INET (addr, port))) ;
            LU.listen main_socket limits.max_connections ;
            Lwt.return (Some main_socket)
          end
            (fun exn ->
               debug "(%a) cannot accept incoming peers (%s)"
                 pp_gid my_gid (string_of_unix_exn exn) ;
               Lwt.return_none)
          >>= function
          | None ->
              (* FIXME: run in degraded mode, better exit ? *)
              Lwt.return_unit
          | Some main_socket ->
              (* then loop *)
              let rec step () =
                Lwt.pick
                  [ ( LU.accept main_socket >>= fun (s, a) ->
                      Lwt.return (Some (s, a)) ) ;
                    ( cancelation () >>= fun _ ->
                      Lwt.return_none ) ]
                >>= function
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
      Lwt.pick
        [ ( LU.sleep 120. >>= fun () ->
            Lwt.return_true) ; (* every two minutes *)
          ( LC.wait please_maintain >>= fun () ->
            Lwt.return_true) ; (* when asked *)
          ( LC.wait too_few_peers >>= fun () ->
            Lwt.return_true) ; (* limits *)
          ( LC.wait too_many_peers >>= fun () ->
            Lwt.return_true) ;
          ( cancelation () >>= fun () ->
            Lwt.return_false) ] >>= fun continue ->
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
              | 0, _ -> Lwt.return_true
              | _, [] ->
                  Lwt.return_false (* we didn't manage to contact enough peers *)
              | nb, ((addr, port), gid, source) :: tl ->
                  (* we try to open a connection *)
                  let socket =
                    let open LU in
                    let open Ipaddr in
                    let family =
                      match addr with V4 _ -> PF_INET | V6 _ -> PF_INET6 in
                    socket family SOCK_STREAM 0 in
                  let uaddr = Ipaddr_unix.to_inet_addr addr in
                  Lwt.catch begin fun () ->
                    debug "(%a) trying to connect to %a:%d"
                      pp_gid my_gid Ipaddr.pp_hum addr port ;
                    Lwt.pick
                      [ (Lwt_unix.sleep 2.0 >>= fun _ -> Lwt.fail Not_found) ;
                        LU.connect socket (LU.ADDR_INET (uaddr, port))
                      ] >>= fun () ->
                    debug "(%a) connected to %a:%d"
                      pp_gid my_gid Ipaddr.pp_hum addr port;
                    enqueue_event (Contact ((addr, port), socket)) ;
                    Lwt.return (nb - 1)
                  end
                    (fun exn ->
                       debug "(%a) connection failed to %a:%d (%s)"
                         pp_gid my_gid Ipaddr.pp_hum addr port
                         (string_of_unix_exn exn);
                       (* if we didn't succes, we greylist it *)
                       let now = Unix.gettimeofday () in
                       known_peers :=
                         PeerMap.update (addr, port) ?gid
                           { source with unreachable_since = Some now }
                           !known_peers ;
                       LU.close socket >>= fun () ->
                       Lwt.return nb) >>= fun nrec ->
                  do_contact_loop (nrec, tl)
            in do_contact_loop (nb, contactable)
          in
          let to_contact = limits.max_connections - n_connected in
          debug "(%a) too few connections (%d)" pp_gid my_gid n_connected ;
          contact to_contact >>= function
          | true -> (* enough contacts, now wait for connections *)
              Lwt.pick
                [ (LC.wait new_peer >>= fun _ -> Lwt.return_true) ;
                  (LU.sleep 1.0 >>= fun () -> Lwt.return_true) ;
                  (cancelation () >>= fun () -> Lwt.return_false) ]
              >>= fun continue ->
              if continue then maintain () else Lwt.return_unit
          | false -> (* not enough contacts, ask the pals of our pals,
                        discover the local network and then wait *)
              LC.broadcast restart_discovery () ;
              (PeerMap.iter
                 (fun _ _ peer -> Lwt.async (fun () -> peer.send Bootstrap))
                 !connected ;
               Lwt.pick
                 [ (LC.wait new_peer >>= fun _ -> Lwt.return_true) ;
                   (LC.wait new_contact >>= fun _ -> Lwt.return_true) ;
                   (LU.sleep 1.0 >>= fun () -> Lwt.return_true) ;
                   (cancelation () >>= fun () -> Lwt.return_false) ]
               >>= fun continue ->
               if continue then maintain () else Lwt.return_unit)
        else
          (* too many peers, start the russian roulette *)
          let to_kill = n_connected - limits.max_connections in
          debug "(%a) too many connections, will kill %d" pp_gid my_gid to_kill ;
          snd (PeerMap.fold
                 (fun _ _ peer (i, t) ->
                    if i = 0 then (0, t)
                    else (i - 1, t >>= fun () -> peer.disconnect ()))
                 !connected (to_kill, Lwt.return_unit)) >>= fun () ->
          (* and directly skip to the next maintenance request *)
          LC.broadcast just_maintained () ;
          debug "(%a) maintenance step ended" pp_gid my_gid ;
          maintenance ()
      in
      if continue then maintain () else Lwt.return_unit
    in
    (* select the peers to send on a bootstrap request *)
    let bootstrap_peers () =
      (* we sort peers by desirability *)
      PeerMap.bindings !known_peers |>
      List.filter (fun ((ip,_),_,_) -> not (Ipaddr.is_private ip)) |>
      List.sort (fun (_, _, s1) (_, _, s2) -> compare_sources s1 s2) |>
      (* we simply send the first 50 (or less) known peers *)
      List.fold_left
        (fun (n, l) (point, _, _) -> if n = 0 then (n, l) else (n - 1, point :: l))
        (50, []) |> snd
    in
    (* main internal event handling worker *)
    let rec main () =
      Lwt.pick
        [ dequeue_event () ;
          cancelation () >>= fun () -> Lwt.return Shutdown ] >>= fun event ->
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
                  known_peers :=
                    PeerMap.update point ~gid:peer.gid source !known_peers
                in update @@
                try match PeerMap.by_gid peer.gid !known_peers with
                  | { connections = None ; white_listed } ->
                      { connections =
                          Some (1, Unix.gettimeofday (), peer.public_key) ;
                        unreachable_since = None ;
                        white_listed ;
                        meta = P.initial_metadata }
                  | { connections = Some (n, _, _) ; white_listed } ->
                      { connections =
                          Some (n + 1, Unix.gettimeofday (), peer.public_key) ;
                        unreachable_since = None ;
                        white_listed ;
                        meta = P.initial_metadata }
                with Not_found ->
                  { connections =
                      Some (1, Unix.gettimeofday (), peer.public_key) ;
                    unreachable_since = None ;
                    white_listed = white_listed point ;
                    meta = P.initial_metadata }
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
            debug "(%a) rejected already connected peer %a @@ %a:%d"
              pp_gid my_gid pp_gid peer.gid
              Ipaddr.pp_hum (fst peer.point) (snd peer.point) ;
            update_infos () ;
            Lwt.async peer.disconnect
          end else begin
            debug "(%a) connected peer %a @@ %a:%d"
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
              connect_to_peer
                config limits my_gid my_public_key my_secret_key my_proof_of_work
                socket (addr, port) enqueue_event white_listed in
            debug "(%a) incoming peer @@ %a:%d"
              pp_gid my_gid Ipaddr.pp_hum addr port ;
            incoming := PointMap.add (addr, port) canceler !incoming ;
            main ()
      | Bootstrap peer ->
          let sample = bootstrap_peers () in
          Lwt.async (fun () -> peer.send (Advertise sample)) ;
          main ()
      | Recv (peer, msg) ->
          enqueue_msg (peer, msg) ;
          main ()
      | Peers peers ->
          List.iter
            (fun point ->
               if not (PeerMap.mem_by_point point !known_peers) then
                 let source =
                   { unreachable_since = None ;
                     connections = None ;
                     white_listed = false ;
                     meta = P.initial_metadata } in
                 known_peers := PeerMap.update point source !known_peers ;
                 LC.broadcast new_contact point)
            peers ;
          main ()
      | Shutdown ->
          Lwt.return_unit
    in
    (* blacklist filter *)
    let rec unblock () =
      Lwt.pick
        [ (Lwt_unix.sleep 20. >>= fun _ -> Lwt.return_true) ;
          (cancelation () >>= fun () -> Lwt.return_false) ] >>= fun continue ->
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
      else Lwt.return_unit
    in
    (* launch all workers *)
    let welcome =
      Lwt_utils.worker
        (Format.asprintf "(%a) welcome" pp_gid my_gid)
        welcome cancel in
    let maintenance =
      Lwt_utils.worker
        (Format.asprintf "(%a) maintenance" pp_gid my_gid)
        maintenance cancel in
    let main =
      Lwt_utils.worker
        (Format.asprintf "(%a) reception" pp_gid my_gid)
        main cancel in
    let unblock =
      Lwt_utils.worker
        (Format.asprintf "(%a) unblacklister" pp_gid my_gid)
        unblock cancel in
    let discovery_answerer =
      let buf = MBytes.create 0x100_000 in
      match config.discovery_port with
      | Some disco_port ->
          let answerer () =
            discovery_answerer
              my_gid disco_port cancelation @@ fun addr port socket ->
            (* do not reply to ourselves or connected peers *)
            if not (PeerMap.mem_by_point (addr, port) !connected)
            && (try match PeerMap.gid_by_point (addr, port) !known_peers with
                | Some gid -> not (PeerMap.mem_by_gid gid !connected)
                              && not (my_gid = gid)
                | None -> true with Not_found -> true) then
              (* either reply by a list of peer or connect if we need peers *)
              if PeerMap.cardinal !connected >= limits.expected_connections then begin
                enqueue_event (Peers [ addr, port ]) ;
                send_msg socket buf (Advertise (bootstrap_peers ())) >>= fun _ ->
                LU.close socket
              end else begin
                enqueue_event (Contact ((addr, port), socket)) ;
                Lwt.return_unit
              end
            else LU.close socket in
          Lwt_utils.worker
            (Format.asprintf "(%a) discovery answerer" pp_gid my_gid)
            answerer cancel
      | _ -> Lwt.return_unit  in
    let discovery_sender =
      match config.incoming_port, config.discovery_port with
      | Some inco_port, Some disco_port ->
          let sender () =
            discovery_sender
              my_gid disco_port inco_port cancelation restart_discovery in
          Lwt_utils.worker
            (Format.asprintf "(%a) discovery sender" pp_gid my_gid)
            sender cancel
      | _ -> Lwt.return_unit in
    (* net manipulation callbacks *)
    let rec shutdown () =
      debug "(%a) starting network shutdown" pp_gid my_gid ;
      (* stop accepting clients *)
      cancel () >>= fun () ->
      (* wait for both workers to end *)
      Lwt.join [ welcome ; main ; maintenance ; unblock ;
                 discovery_answerer ; discovery_sender ] >>= fun () ->
      (* properly shutdown all peers *)
      let cancelers =
        PeerMap.fold
          (fun point _ peer res ->
             (peer.disconnect () >>= fun () ->
              connected := PeerMap.remove_by_point point !connected ;
              Lwt.return_unit) :: res)
          !connected @@
        PointMap.fold
          (fun point canceler res ->
             (canceler () >>= fun () ->
              incoming := PointMap.remove point !incoming ;
              Lwt.return_unit) :: res)
          !incoming @@ []
      in
      Lwt.join cancelers >>= fun () ->
      debug "(%a) network shutdown complete" pp_gid my_gid ;
      Lwt.return_unit
    and peers () =
      PeerMap.fold (fun _ _ peer r -> peer :: r) !connected []
    and find_peer gid =
      try Some (PeerMap.by_gid gid !connected) with Not_found -> None
    and peer_info (peer : peer) = {
      gid = peer.gid ;
      addr = fst peer.point ;
      port = snd peer.point ;
      version = peer.version ;
    }
    and recv_from () =
      dequeue_msg ()
    and send_to peer msg =
      peer.send (Message msg) >>= fun _ -> Lwt.return_unit
    and try_send_to peer msg =
      Lwt.async (fun () -> peer.send (Message msg)); true
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
            white_listed = true ;
            meta = P.initial_metadata },
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
    and get_metadata _gid = None (* TODO: implement *)
    and set_metadata _gid _meta = () (* TODO: implement *)
    in
    let net =
      { shutdown ; peers ; find_peer ;
        recv_from ; send_to ; try_send_to ; broadcast ;
        blacklist ; whitelist ; maintain ; roll ;
        peer_info ; get_metadata ; set_metadata } in
    (* main thread, returns after first successful maintenance *)
    maintain () >>= fun () ->
    debug "(%a) network succesfully bootstrapped" pp_gid my_gid ;
    Lwt.return net

  let faked_network =
    let infinity, wakeup = Lwt.wait () in
    let shutdown () =
      Lwt.wakeup_exn wakeup Lwt_stream.Empty;
      Lwt.return_unit in
    let peers () = [] in
    let find_peer _ = None in
    let recv_from () = infinity in
    let send_to _ _ = Lwt.return_unit in
    let try_send_to _ _ = true in
    let broadcast _ = () in
    let blacklist ?duration _ = ignore duration ; () in
    let whitelist _ = () in
    let maintain () = Lwt.return_unit in
    let roll () = Lwt.return_unit in
    let peer_info _ = assert false in
    let get_metadata _ = None in
    let set_metadata _ _ = () in
    { shutdown ; peers ; find_peer ;
      recv_from ; send_to ; try_send_to ; broadcast ;
      blacklist ; whitelist ; maintain ; roll ;
      peer_info ; get_metadata ; set_metadata }


  (* Plug toplevel functions to callback calls. *)
  let shutdown net = net.shutdown ()
  let peers net = net.peers ()
  let find_peer net gid = net.find_peer gid
  let peer_info net peer = net.peer_info peer
  let recv net = net.recv_from ()
  let send net peer msg = net.send_to peer msg
  let try_send net peer msg = net.try_send_to peer msg
  let broadcast net msg = net.broadcast msg
  let maintain net = net.maintain ()
  let roll net = net.roll ()
  let blacklist _net _gid = ()
  let whitelist _net _gid = ()
  let get_metadata net gid = net.get_metadata gid
  let set_metadata net gid meta = net.set_metadata gid meta
end
