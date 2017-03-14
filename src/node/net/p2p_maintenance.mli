(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* min <= min_threshold <= min_target <= max_target <= max_threshold <= max *)

(* The 'pool' urges the maintainer to work when the number of
   connections reaches `max` or is below `min`. Otherwise, the
   maintener is lazy and only lookup for connection every two
   minutes. The [maintain] function is another way to signal the
   maintainer that a maintenance step is desired.

   When the maintener detects that the number of connections is over
   `max_threshold`, it randomly kills connections to reach `max_target`.

   When the maintener detects that the number of connections is below
   `min_threshold`, it creates enough connection to reach at least
   `min_target` (and never more than `max_target`). In the process, it
   might ask its actual peers for new peers. *)

type bounds = {
  min_threshold: int ;
  min_target: int ;
  max_target: int ;
  max_threshold: int ;
}

type 'meta t
(** Type of a maintenance worker. *)

val run:
  connection_timeout:float ->
  bounds ->
  ('msg, 'meta) P2p_connection_pool.t ->
  P2p_discovery.t option ->
  'meta t

val maintain: 'meta t -> unit Lwt.t

val shutdown: 'meta t -> unit Lwt.t
