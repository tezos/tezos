(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(************************ p2p io scheduler ********************************)

type error += Connection_closed

let () =
  (* Connection closed *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_io_scheduler.connection_closed"
    ~title:"Connection closed"
    ~description:"IO error: connection with a peer is closed."
    ~pp:(fun ppf () -> Format.fprintf ppf "IO error: connection with a peer is closed.")
    Data_encoding.empty
    (function Connection_closed -> Some () | _ -> None)
    (fun () -> Connection_closed)

(***************************** p2p socket *********************************)

type error += Decipher_error
type error += Invalid_message_size
type error += Encoding_error
type error += Rejected_socket_connection
type error += Rejected_no_common_protocol of { announced : Network_version.t }
type error += Decoding_error
type error += Myself of P2p_connection.Id.t
type error += Not_enough_proof_of_work of P2p_peer.Id.t
type error += Invalid_auth
type error += Invalid_chunks_size of { value: int ; min: int ; max: int }

let () =
  (* Decipher error *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.decipher_error"
    ~title:"Decipher error"
    ~description:"An error occurred while deciphering."
    ~pp:(fun ppf () -> Format.fprintf ppf "An error occurred while deciphering.")
    Data_encoding.empty
    (function Decipher_error -> Some () | _ -> None)
    (fun () -> Decipher_error) ;
  (* Invalid message size *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_message_size"
    ~title:"Invalid message size"
    ~description:"The size of the message to be written is invalid."
    ~pp:(fun ppf () -> Format.fprintf ppf "The size of the message to be written is invalid.")
    Data_encoding.empty
    (function Invalid_message_size -> Some () | _ -> None)
    (fun () -> Invalid_message_size) ;
  (* Encoding error *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.encoding_error"
    ~title:"Encoding error"
    ~description:"An error occurred while encoding."
    ~pp:(fun ppf () -> Format.fprintf ppf "An error occurred while encoding.")
    Data_encoding.empty
    (function Encoding_error -> Some () | _ -> None)
    (fun () -> Encoding_error) ;
  (* Rejected socket connection *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejected_socket_connection"
    ~title:"Rejected socket connection"
    ~description:"Rejected peer connection: rejected socket connection."
    ~pp:(fun ppf () -> Format.fprintf ppf "Rejected peer connection: rejected socket connection.")
    Data_encoding.empty
    (function Rejected_socket_connection -> Some () | _ -> None)
    (fun () -> Rejected_socket_connection) ;
  (* Rejected socket connection, no common network protocol *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejected_no_common_protocol"
    ~title:"Rejected socket connection - no common network protocol"
    ~description:"Rejected peer connection: \
                  rejected socket connection as we have no common \
                  network protocol with the peer."
    ~pp:(fun ppf _lst -> Format.fprintf ppf
            "Rejected peer connection: no common network protocol.")
    Data_encoding.(obj1 (req "announced_version" Network_version.encoding))
    (function
      | Rejected_no_common_protocol { announced } -> Some announced
      | _ -> None)
    (fun announced -> Rejected_no_common_protocol { announced });
  (* Decoding error *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.decoding_error"
    ~title:"Decoding error"
    ~description:"An error occurred while decoding."
    ~pp:(fun ppf () -> Format.fprintf ppf "An error occurred while decoding.")
    Data_encoding.empty
    (function Decoding_error -> Some () | _ -> None)
    (fun () -> Decoding_error) ;
  (* Myself *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.myself"
    ~title:"Myself"
    ~description:"Remote peer is actually yourself."
    ~pp:(fun ppf id -> Format.fprintf ppf
            "Remote peer %a cannot be authenticated: peer is actually yourself."
            P2p_connection.Id.pp id)
    Data_encoding.(obj1 (req "connection id" P2p_connection.Id.encoding))
    (function Myself id -> Some id | _ -> None)
    (fun id -> Myself id) ;
  (* Not enough proof of work *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.not_enough_proof_of_work"
    ~title:"Not enough proof of work"
    ~description:"Remote peer cannot be authenticated: not enough proof of work."
    ~pp:(fun ppf id ->
        Format.fprintf ppf
          "Remote peer %a cannot be authenticated: not enough proof of work."
          P2p_peer.Id.pp id)
    Data_encoding.(obj1 (req "peer id" P2p_peer.Id.encoding))
    (function Not_enough_proof_of_work id -> Some id | _ -> None)
    (fun id -> Not_enough_proof_of_work id) ;
  (* Invalid authentication *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_auth"
    ~title:"Invalid authentication"
    ~description:"Rejected peer connection: invalid authentication."
    ~pp:(fun ppf () -> Format.fprintf ppf "Rejected peer connection: invalid authentication.")
    Data_encoding.empty
    (function Invalid_auth -> Some () | _ -> None)
    (fun () -> Invalid_auth) ;
  (* Invalid chunks size *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_chunks_size"
    ~title:"Invalid chunks size"
    ~description:"Size of chunks is not valid."
    ~pp:(fun ppf (value, min, max) ->
        Format.fprintf ppf "Size of chunks is invalid: should be between %d and %d but is %d" min max value)
    Data_encoding.(obj3
                     (req "value" int31)
                     (req "min" int31)
                     (req "max" int31))
    (function Invalid_chunks_size { value ; min ; max }
      -> Some (value, min, max) | _ -> None)
    (fun (value, min, max) -> Invalid_chunks_size { value ; min ; max })

(***************************** p2p pool ***********************************)

type error += Pending_connection
type error += Connected
type error += Connection_refused
type error += Rejected of P2p_peer.Id.t
type error += Too_many_connections
type error += Private_mode
type error += Point_banned of P2p_point.Id.t
type error += Peer_banned of P2p_peer.Id.t

let () =
  (* Pending connection *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.pending_connection"
    ~title:"Pending connection"
    ~description:"Fail to connect with a peer: a connection is already pending."
    ~pp:(fun ppf () -> Format.fprintf ppf "Fail to connect with a peer: a connection is already pending.")
    Data_encoding.empty
    (function Pending_connection -> Some () | _ -> None)
    (fun () -> Pending_connection) ;
  (* Connected *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.connected"
    ~title:"Connected"
    ~description:"Fail to connect with a peer: a connection is already established."
    ~pp:(fun ppf () -> Format.fprintf ppf "Fail to connect with a peer: a connection is already established.")
    Data_encoding.empty
    (function Connected -> Some () | _ -> None)
    (fun () -> Connected) ;
  (* Connected refused *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.connection_refused"
    ~title:"Connection refused"
    ~description:"Connection was refused."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection was refused.")
    Data_encoding.empty
    (function Connection_refused -> Some () | _ -> None)
    (fun () -> Connection_refused) ;
  (* Rejected *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.rejected"
    ~title:"Rejected peer"
    ~description:"Connection to peer was rejected."
    ~pp:(fun ppf id ->
        Format.fprintf ppf "Connection to peer %a was rejected." P2p_peer.Id.pp id)
    Data_encoding.(obj1 (req "peer id" P2p_peer.Id.encoding))
    (function Rejected id -> Some id | _ -> None)
    (fun id -> Rejected id) ;
  (* Too many connections *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.too_many_connections"
    ~title:"Too many connections"
    ~description:"Too many connections."
    ~pp:(fun ppf () -> Format.fprintf ppf "Too many connections.")
    Data_encoding.empty
    (function Too_many_connections -> Some () | _ -> None)
    (fun () -> Too_many_connections) ;
  (* Private mode *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.private_mode"
    ~title:"Private mode"
    ~description:"Node is in private mode."
    ~pp:(fun ppf () -> Format.fprintf ppf "Node is in private mode.")
    Data_encoding.empty
    (function Private_mode -> Some () | _ -> None)
    (fun () -> Private_mode) ;
  (* Point Banned *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.point_banned"
    ~title:"Point Banned"
    ~description:"The addr you tried to connect is banned."
    ~pp:(fun ppf (addr, _port) ->
        Format.fprintf ppf
          "The addr you tried to connect (%a) is banned."
          P2p_addr.pp addr)
    Data_encoding.(obj1 (req "point" P2p_point.Id.encoding))
    (function Point_banned point -> Some point | _ -> None)
    (fun point -> Point_banned point) ;
  (* Peer Banned *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.peer_banned"
    ~title:"Peer Banned"
    ~description:"The peer identity you tried to connect is banned."
    ~pp:(fun ppf peer_id ->
        Format.fprintf ppf
          "The peer identity you tried to connect (%a) is banned."
          P2p_peer.Id.pp peer_id)
    Data_encoding.(obj1 (req "peer" P2p_peer.Id.encoding))
    (function Peer_banned peer_id -> Some peer_id | _ -> None)
    (fun peer_id -> Peer_banned peer_id)
