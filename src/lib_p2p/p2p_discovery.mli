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


(** Local peer discovery.

    This module manages the discovery of local peers through UDP broadcasting.
    It is composed of two workers:
    - The sender worker whose role is to broadcast discovery messages.
    - The answer worker whose role is to listen discovery messages and register new
      peers in the current pool.
    Discovery messages are composed of an arbitrary key, the listening port and
    the peer id of the current peer.
*)

(** Type of a discovery worker. *)
type t

(** [create ~listening_port ~discovery_port ~discovery_addr pool peer_id]
    returns a discovery worker registering local peers to the [pool]
    and broadcasting discovery messages with the [peer_id] and
    the [listening_port] through the address [discovery_addr:discovery_port]. *)
val create :
  listening_port:int ->
  discovery_port:int -> discovery_addr:Ipaddr.V4.t ->
  trust_discovered_peers:bool ->
  ('a, 'b, 'c) P2p_pool.t -> P2p_peer.Table.key ->
  t

val activate : t -> unit

(** [wakeup t] sends a signal to the sender machine of [t], asking it
    to immediately proceed to broadcasting. *)
val wakeup : t -> unit

(** [shutdown t] returns when [t] has completed shutdown. *)
val shutdown : t -> unit Lwt.t
