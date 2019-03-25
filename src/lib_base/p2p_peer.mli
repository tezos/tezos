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

  type ('peer_meta, 'conn_meta) t = {
    score : float ;
    trusted : bool ;
    conn_metadata : 'conn_meta option ;
    peer_metadata : 'peer_meta ;
    state : State.t ;
    id_point : P2p_connection.Id.t option ;
    stat : P2p_stat.t ;
    last_failed_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_rejected_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_established_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_disconnection : (P2p_connection.Id.t * Time.System.t) option ;
    last_seen : (P2p_connection.Id.t * Time.System.t) option ;
    last_miss : (P2p_connection.Id.t * Time.System.t) option ;
  }

  val encoding : 'peer_meta Data_encoding.t ->
    'conn_meta Data_encoding.t -> ('peer_meta, 'conn_meta) t Data_encoding.t

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
    (** We successfully established a authentified connection. *)
    | Disconnection
    (** We decided to close the connection. *)
    | External_disconnection
    (** The connection was closed for external reason. *)

  type t = {
    kind : kind ;
    timestamp : Time.System.t ;
    point : P2p_connection.Id.t ;
  }

  val encoding : t Data_encoding.t

end
