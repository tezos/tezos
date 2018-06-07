(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Id = P2p_peer_id

module Map = Id.Map
module Set = Id.Set
module Table = Id.Table

module Filter : sig

  type t =
    | Accepted
    | Running
    | Disconnected

  val rpc_arg : t RPC_arg.t

end

module State : sig

  type t =
    | Accepted
    | Running
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

  val filter : Filter.t list -> t -> bool

end

module Info : sig

  type 'conn_meta t = {
    score : float ;
    trusted : bool ;
    conn_metadata : 'conn_meta option ;
    state : State.t ;
    id_point : P2p_connection.Id.t option ;
    stat : P2p_stat.t ;
    last_failed_connection : (P2p_connection.Id.t * Time.t) option ;
    last_rejected_connection : (P2p_connection.Id.t * Time.t) option ;
    last_established_connection : (P2p_connection.Id.t * Time.t) option ;
    last_disconnection : (P2p_connection.Id.t * Time.t) option ;
    last_seen : (P2p_connection.Id.t * Time.t) option ;
    last_miss : (P2p_connection.Id.t * Time.t) option ;
  }

  val encoding : 'conn_meta Data_encoding.t -> 'conn_meta t Data_encoding.t

end

module Pool_event : sig

  type kind =
    | Accepting_request
    (** We accepted a connection after authentifying the remote peer. *)
    | Rejecting_request
    (** We rejected a connection after authentifying the remote peer. *)
    | Request_rejected
    (** The remote peer rejected our connection. *)
    | Connection_established
    (** We succesfully established a authentified connection. *)
    | Disconnection
    (** We decided to close the connection. *)
    | External_disconnection
    (** The connection was closed for external reason. *)

  type t = {
    kind : kind ;
    timestamp : Time.t ;
    point : P2p_connection.Id.t ;
  }

  val encoding : t Data_encoding.t

end
