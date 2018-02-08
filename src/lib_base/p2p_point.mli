(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Id : sig

  type t = P2p_addr.t * P2p_addr.port
  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit

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

end

module Info : sig

  type t = {
    trusted : bool ;
    greylisted_until : Time.t ;
    state : State.t ;
    last_failed_connection : Time.t option ;
    last_rejected_connection : (P2p_peer_id.t * Time.t) option ;
    last_established_connection : (P2p_peer_id.t * Time.t) option ;
    last_disconnection : (P2p_peer_id.t * Time.t) option ;
    last_seen : (P2p_peer_id.t * Time.t) option ;
    last_miss : Time.t option ;
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
    (** We succesfully established a authentified connection. *)
    | Disconnection of P2p_peer_id.t
    (** We decided to close the connection. *)
    | External_disconnection of P2p_peer_id.t
    (** The connection was closed for external reason. *)

  type t = {
    kind : kind ;
    timestamp : Time.t ;
  }

  val encoding : t Data_encoding.t

end


