(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

type 'meta t
(** Type of a maintenance worker. *)

val run:
  greylist_timeout:float ->
  bounds -> ('msg, 'meta) P2p_pool.t -> 'meta t
(** [run ~greylist_timeout bounds pool] is a maintenance worker for
    [pool] with connection targets specified in [bounds] and greylist
    GC frequency [greylist_timeout]. *)

val maintain: 'meta t -> unit Lwt.t
(** [maintain t] gives a hint to maintenance worker [t] that
    maintenance is needed and returns whenever [t] has done a
    maintenance cycle. *)

val shutdown: 'meta t -> unit Lwt.t
(** [shutdown t] is a thread that returns whenever [t] has
    successfully shut down. *)
