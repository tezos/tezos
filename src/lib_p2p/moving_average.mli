(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Moving averages.

    This module implements bandwidth counters based on (cumulative)
    exponential moving average. Each counter is identified by an
    integer. They are stored in an internal hash table.

    See i.e.
    https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
    for the algorithm.
*)

type t
(** Type of one bandwidth counter. *)

val create: init:int -> alpha:float -> t
(** [create ~init ~alpha] is a counter with initial value [init] and
    factor [alpha]. *)

val destroy: t -> unit
(** [destroy t] removes counter [t] from the internal hash table. *)

val add: t -> int -> unit
(** [add t id] adds [t] in the internal hash table under identifies
    [id]. *)

val on_update: (unit -> unit) -> unit
(** [of_update f] registers [f] to be called on each update of the
    internal worker (currently every 1s). *)

val updated: unit Lwt_condition.t
(** [updated] is a condition variable that gets signaled on each
    update of the internal worker (currently every 1s). *)

type stat = {
  total: int64 ;
  average: int ;
}

val stat: t -> stat
(** [stat t] is a stat record reflecting the state of [t] at the time
    of the call. *)
