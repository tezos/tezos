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

module Id : sig

  type t = P2p_addr.t * P2p_addr.port
  val compare : t -> t -> int
  val equal :  t -> t -> bool

  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit
  val pp_list : Format.formatter -> t list -> unit

  val of_string_exn : string -> t
  val of_string : string -> (t, string) result
  val to_string : t -> string
  val encoding : t Data_encoding.t
  val is_local : t -> bool
  val is_global : t -> bool
  val parse_addr_port : string -> string * string

  val rpc_arg : t RPC_arg.t
end

module Map : Map.S with type key = Id.t
module Set : Set.S with type elt = Id.t
module Table : Hashtbl.S with type key = Id.t

module Filter : sig

  type t =
    | Requested
    | Accepted
    | Running
    | Disconnected

  val rpc_arg : t RPC_arg.t

end

module State : sig

  type t =
    | Requested
    | Accepted of P2p_peer_id.t
    | Running of P2p_peer_id.t
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

  val of_p2p_peer_id : t -> P2p_peer_id.t option
  val of_peerid_state : t -> P2p_peer_id.t option -> t

  val filter : Filter.t list -> t -> bool

end

module Info : sig

  type t = {
    trusted : bool ;
    greylisted_until : Time.System.t ;
    state : State.t ;
    last_failed_connection : Time.System.t option ;
    last_rejected_connection : (P2p_peer_id.t * Time.System.t) option ;
    last_established_connection : (P2p_peer_id.t * Time.System.t) option ;
    last_disconnection : (P2p_peer_id.t * Time.System.t) option ;
    last_seen : (P2p_peer_id.t * Time.System.t) option ;
    last_miss : Time.System.t option ;
  }

  val encoding: t Data_encoding.t

end

module Pool_event : sig

  type kind =
    | Outgoing_request
    (** We initiated a connection. *)
    | Accepting_request of P2p_peer_id.t
    (** We accepted a connection after authentifying the remote peer. *)
    | Rejecting_request of P2p_peer_id.t
    (** We rejected a connection after authentifying the remote peer. *)
    | Request_rejected of P2p_peer_id.t option
    (** The remote peer rejected our connection. *)
    | Connection_established of P2p_peer_id.t
    (** We successfully established a authentified connection. *)
    | Disconnection of P2p_peer_id.t
    (** We decided to close the connection. *)
    | External_disconnection of P2p_peer_id.t
    (** The connection was closed for external reason. *)

  type t = kind Time.System.stamped

  val encoding : t Data_encoding.t

end


