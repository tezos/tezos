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

module Id = struct

  (* A net point (address x port). *)
  type t = P2p_addr.t * P2p_addr.port option
  let compare (a1, p1) (a2, p2) =
    match Ipaddr.V6.compare a1 a2 with
    | 0 -> Pervasives.compare p1 p2
    | x -> x
  let equal p1 p2 = compare p1 p2 = 0
  let hash = Hashtbl.hash
  let pp ppf (addr, port) =
    match port with
    | None ->
        Format.fprintf ppf "[%a]:??" Ipaddr.V6.pp addr
    | Some port ->
        Format.fprintf ppf "[%a]:%d" Ipaddr.V6.pp addr port
  let pp_opt ppf = function
    | None -> Format.pp_print_string ppf "none"
    | Some point -> pp ppf point
  let to_string t = Format.asprintf "%a" pp t

  let is_local (addr, _) = Ipaddr.V6.is_private addr
  let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

  let of_point (addr, port) = addr, Some port
  let to_point = function
    | _, None -> None
    | addr, Some port -> Some (addr, port)
  let to_point_exn = function
    | _, None -> invalid_arg "to_point_exn"
    | addr, Some port -> addr, port

  let encoding =
    let open Data_encoding in
    (obj2
       (req "addr" P2p_addr.encoding)
       (opt "port" uint16))

end

module Map = Map.Make (Id)
module Set = Set.Make (Id)
module Table = Hashtbl.Make (Id)

module Info = struct

  type 'meta t = {
    incoming : bool ;
    peer_id : P2p_peer_id.t ;
    id_point : Id.t ;
    remote_socket_port : P2p_addr.port ;
    announced_version : Network_version.t ;
    private_node : bool ;
    local_metadata : 'meta ;
    remote_metadata : 'meta ;
  }

  let encoding metadata_encoding =
    let open Data_encoding in
    conv
      (fun { incoming ; peer_id ; id_point ; remote_socket_port ;
             announced_version ; private_node ;
             local_metadata ; remote_metadata } ->
        (incoming, peer_id, id_point, remote_socket_port,
         announced_version, private_node,
         local_metadata, remote_metadata))
      (fun (incoming, peer_id, id_point, remote_socket_port,
            announced_version, private_node,
            local_metadata, remote_metadata) ->
        { incoming ; peer_id ; id_point ; remote_socket_port ;
          announced_version ; private_node ;
          local_metadata ; remote_metadata })
      (obj8
         (req "incoming" bool)
         (req "peer_id" P2p_peer_id.encoding)
         (req "id_point" Id.encoding)
         (req "remote_socket_port" uint16)
         (req "announced_version" Network_version.encoding)
         (req "private" bool)
         (req "local_metadata" metadata_encoding)
         (req "remote_metadata" metadata_encoding))

  let pp pp_meta ppf
      { incoming ; id_point = (remote_addr, remote_port) ;
        remote_socket_port ; peer_id ; announced_version ;
        private_node ;
        local_metadata = _ ; remote_metadata } =
    let point = match remote_port with
      | None -> remote_addr, remote_socket_port
      | Some port -> remote_addr, port in
    Format.fprintf ppf "%s %a %a (%a) %s%a"
      (if incoming then "↘" else "↗")
      P2p_peer_id.pp peer_id
      P2p_point.Id.pp point
      Network_version.pp announced_version
      (if private_node then " private" else "")
      pp_meta remote_metadata

end

module Pool_event = struct

  (** Pool-level events *)

  type t =

    | Too_few_connections
    | Too_many_connections

    | New_point of P2p_point.Id.t
    | New_peer of P2p_peer_id.t

    | Gc_points
    | Gc_peer_ids

    | Incoming_connection of P2p_point.Id.t
    | Outgoing_connection of P2p_point.Id.t
    | Authentication_failed of P2p_point.Id.t
    | Accepting_request of P2p_point.Id.t * Id.t * P2p_peer_id.t
    | Rejecting_request of P2p_point.Id.t * Id.t * P2p_peer_id.t
    | Request_rejected of P2p_point.Id.t * (Id.t * P2p_peer_id.t) option
    | Connection_established of Id.t * P2p_peer_id.t

    | Swap_request_received of { source : P2p_peer_id.t }
    | Swap_ack_received of { source : P2p_peer_id.t }
    | Swap_request_sent of { source : P2p_peer_id.t }
    | Swap_ack_sent of { source : P2p_peer_id.t }
    | Swap_request_ignored of { source : P2p_peer_id.t }
    | Swap_success of { source : P2p_peer_id.t }
    | Swap_failure of { source : P2p_peer_id.t }

    | Disconnection of P2p_peer_id.t
    | External_disconnection of P2p_peer_id.t

  let pp ppf (event:t) =
    match event with
    | Too_few_connections -> Format.pp_print_string ppf "Too_few_connections"
    | Too_many_connections -> Format.pp_print_string ppf "Too_many_connections"
    | New_point p -> Format.pp_print_string ppf "New_point " ; P2p_point.Id.pp ppf p
    | New_peer p -> Format.pp_print_string ppf "New_peer " ; P2p_peer_id.pp ppf p
    | Gc_points -> Format.pp_print_string ppf "Gc_points"
    | Gc_peer_ids -> Format.pp_print_string ppf "Gc_peer_ids"
    | Incoming_connection p ->
        Format.pp_print_string ppf "Incoming_connection " ;
        P2p_point.Id.pp ppf p
    | Outgoing_connection p ->
        Format.pp_print_string ppf "Outgoing_connection " ;
        P2p_point.Id.pp ppf p
    | Authentication_failed p ->
        Format.pp_print_string ppf "Authentication_failed " ;
        P2p_point.Id.pp ppf p
    | Accepting_request (pi, _, _) ->
        Format.pp_print_string ppf "Accepting_request " ;
        P2p_point.Id.pp ppf pi
    | Rejecting_request (pi, _, _) ->
        Format.pp_print_string ppf "Rejecting_request " ;
        P2p_point.Id.pp ppf pi
    | Request_rejected (pi, _) ->
        Format.pp_print_string ppf "Request_rejected " ;
        P2p_point.Id.pp ppf pi
    | Connection_established (_, pi) ->
        Format.pp_print_string ppf "Connection_established " ;
        P2p_peer_id.pp ppf pi
    | Swap_request_received { source } ->
        Format.pp_print_string ppf "Swap_request_received " ;
        P2p_peer_id.pp ppf source
    | Swap_ack_received { source } ->
        Format.pp_print_string ppf "Swap_ack_received " ;
        P2p_peer_id.pp ppf source
    | Swap_request_sent { source } ->
        Format.pp_print_string ppf "Swap_request_sent " ;
        P2p_peer_id.pp ppf source
    | Swap_ack_sent { source } ->
        Format.pp_print_string ppf "Swap_ack_sent " ;
        P2p_peer_id.pp ppf source
    | Swap_request_ignored { source } ->
        Format.pp_print_string ppf "Swap_request_ignored " ;
        P2p_peer_id.pp ppf source
    | Swap_success { source } ->
        Format.pp_print_string ppf "Swap_success " ;
        P2p_peer_id.pp ppf source
    | Swap_failure { source } ->
        Format.pp_print_string ppf "Swap_failure " ;
        P2p_peer_id.pp ppf source
    | Disconnection source ->
        Format.pp_print_string ppf "Disconnection " ;
        P2p_peer_id.pp ppf source
    | External_disconnection source ->
        Format.pp_print_string ppf "External_disconnection " ;
        P2p_peer_id.pp ppf source

  let encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv (fun x -> (), x) (fun ((), x) -> x)
        (merge_objs
           (obj1 (req "event" (constant name))) obj) in
    union ~tag_size:`Uint8 [
      case (Tag 0)
        ~title:"Too_few_connections"
        (branch_encoding "too_few_connections" empty)
        (function Too_few_connections -> Some () | _ -> None)
        (fun () -> Too_few_connections) ;
      case (Tag 1)
        ~title:"Too_many_connections"
        (branch_encoding "too_many_connections" empty)
        (function Too_many_connections -> Some () | _ -> None)
        (fun () -> Too_many_connections) ;
      case (Tag 2)
        ~title:"New_point"
        (branch_encoding "new_point"
           (obj1 (req "point" P2p_point.Id.encoding)))
        (function New_point p -> Some p | _ -> None)
        (fun p -> New_point p) ;
      case (Tag 3)
        ~title:"New_peer"
        (branch_encoding "new_peer"
           (obj1 (req "peer_id" P2p_peer_id.encoding)))
        (function New_peer p -> Some p | _ -> None)
        (fun p -> New_peer p) ;
      case (Tag 4)
        ~title:"Incoming_connection"
        (branch_encoding "incoming_connection"
           (obj1 (req "point" P2p_point.Id.encoding)))
        (function Incoming_connection p -> Some p | _ -> None)
        (fun p -> Incoming_connection p) ;
      case (Tag 5)
        ~title:"Outgoing_connection"
        (branch_encoding "outgoing_connection"
           (obj1 (req "point" P2p_point.Id.encoding)))
        (function Outgoing_connection p -> Some p | _ -> None)
        (fun p -> Outgoing_connection p) ;
      case (Tag 6)
        ~title:"Authentication_failed"
        (branch_encoding "authentication_failed"
           (obj1 (req "point" P2p_point.Id.encoding)))
        (function Authentication_failed p -> Some p | _ -> None)
        (fun p -> Authentication_failed p) ;
      case (Tag 7)
        ~title:"Accepting_request"
        (branch_encoding "accepting_request"
           (obj3
              (req "point" P2p_point.Id.encoding)
              (req "id_point" Id.encoding)
              (req "peer_id" P2p_peer_id.encoding)))
        (function Accepting_request (p, id_p, g) ->
           Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Accepting_request (p, id_p, g)) ;
      case (Tag 8)
        ~title:"Rejecting_request"
        (branch_encoding "rejecting_request"
           (obj3
              (req "point" P2p_point.Id.encoding)
              (req "id_point" Id.encoding)
              (req "peer_id" P2p_peer_id.encoding)))
        (function Rejecting_request (p, id_p, g) ->
           Some (p, id_p, g) | _ -> None)
        (fun (p, id_p, g) -> Rejecting_request (p, id_p, g)) ;
      case (Tag 9)
        ~title:"Request_rejected"
        (branch_encoding "request_rejected"
           (obj2
              (req "point" P2p_point.Id.encoding)
              (opt "identity"
                 (tup2 Id.encoding P2p_peer_id.encoding))))
        (function Request_rejected (p, id) -> Some (p, id) | _ -> None)
        (fun (p, id) -> Request_rejected (p, id)) ;
      case (Tag 10)
        ~title:"Connection_established"
        (branch_encoding "connection_established"
           (obj2
              (req "id_point" Id.encoding)
              (req "peer_id" P2p_peer_id.encoding)))
        (function Connection_established (id_p, g) ->
           Some (id_p, g) | _ -> None)
        (fun (id_p, g) -> Connection_established (id_p, g)) ;
      case (Tag 11)
        ~title:"Disconnection"
        (branch_encoding "disconnection"
           (obj1 (req "peer_id" P2p_peer_id.encoding)))
        (function Disconnection g -> Some g | _ -> None)
        (fun g -> Disconnection g) ;
      case (Tag 12)
        ~title:"External_disconnection"
        (branch_encoding "external_disconnection"
           (obj1 (req "peer_id" P2p_peer_id.encoding)))
        (function External_disconnection g -> Some g | _ -> None)
        (fun g -> External_disconnection g) ;
      case (Tag 13)
        ~title:"Gc_points"
        (branch_encoding "gc_points" empty)
        (function Gc_points -> Some () | _ -> None)
        (fun () -> Gc_points) ;
      case (Tag 14)
        ~title:"Gc_peer_ids"
        (branch_encoding "gc_peer_ids" empty)
        (function Gc_peer_ids -> Some () | _ -> None)
        (fun () -> Gc_peer_ids) ;
      case (Tag 15)
        ~title:"Swap_request_received"
        (branch_encoding "swap_request_received"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_request_received { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_received { source }) ;
      case (Tag 16)
        ~title:"Swap_ack_received"
        (branch_encoding "swap_ack_received"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_ack_received { source } -> Some source
          | _ -> None)
        (fun source -> Swap_ack_received { source }) ;
      case (Tag 17)
        ~title:"Swap_request_sent"
        (branch_encoding "swap_request_sent"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_request_sent { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_sent { source }) ;
      case (Tag 18)
        ~title:"Swap_ack_sent"
        (branch_encoding "swap_ack_sent"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_ack_sent { source } -> Some source
          | _ -> None)
        (fun source -> Swap_ack_sent { source }) ;
      case (Tag 19)
        ~title:"Swap_request_ignored"
        (branch_encoding "swap_request_ignored"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_request_ignored { source } -> Some source
          | _ -> None)
        (fun source -> Swap_request_ignored { source }) ;
      case (Tag 20)
        ~title:"Swap_success"
        (branch_encoding "swap_success"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_success { source } -> Some source
          | _ -> None)
        (fun source -> Swap_success { source }) ;
      case (Tag 21)
        ~title:"Swap_failure"
        (branch_encoding "swap_failure"
           (obj1 (req "source" P2p_peer_id.encoding)))
        (function
          | Swap_failure { source } -> Some source
          | _ -> None)
        (fun source -> Swap_failure { source }) ;
    ]

end
