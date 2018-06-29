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
