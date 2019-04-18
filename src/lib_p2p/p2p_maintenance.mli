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

(* min <= min_threshold <= min_target <= max_target <= max_threshold <= max *)

(** P2P maintenance worker.

    The P2P layer urges the maintainer to work when the number of
    connections reaches `max` or is below `min`. Otherwise, the
    maintener is lazy and only looks up for connections every two
    minutes (hardcoded constant). The [maintain] function is another
    way to signal the maintainer that a maintenance step is desired.

    When the maintener detects that the number of connections is over
    `max_threshold`, it randomly kills connections to reach
    `max_target`.

    When the maintener detects that the number of connections is below
    `min_threshold`, it creates enough connection to reach at least
    `min_target` (and never more than `max_target`). In the process, it
    might ask its actual peers for new peers.  *)

type bounds = {
  min_threshold: int ;
  min_target: int ;
  max_target: int ;
  max_threshold: int ;
}

type config = {

  maintenance_idle_time: Time.System.Span.t ;
  (** How long to wait at most, in seconds, before running a maintenance loop. *)

  greylist_timeout: Time.System.Span.t ;
  (** GC delay for the greylists tables, in seconds. *)

  private_mode: bool ;
  (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should be revealed to
      the rest of the network. *)

}


type 'meta t
(** Type of a maintenance worker. *)

val create:
  ?discovery:P2p_discovery.t ->
  config -> bounds ->
  ('msg, 'meta, 'meta_conn) P2p_pool.t ->
  'meta t
(** [run ?discovery config bounds pool] returns a maintenance worker, with
    the [discovery] worker if present, for [pool] with connection targets
    specified in [bounds]. *)

val activate: 'meta t -> unit
(** [activate t] start the worker that will maintain connections *)

val maintain: 'meta t -> unit Lwt.t
(** [maintain t] gives a hint to maintenance worker [t] that
    maintenance is needed and returns whenever [t] has done a
    maintenance cycle. *)

val shutdown: 'meta t -> unit Lwt.t
(** [shutdown t] is a thread that returns whenever [t] has
    successfully shut down. *)
