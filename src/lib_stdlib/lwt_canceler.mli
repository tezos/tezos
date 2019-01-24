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

(** A [Canceler.t] is a three-states synchronization object with transitions
    "waiting -> canceling -> canceled", starting in waiting state. A chain
    of hooks can be attached to the canceler. Hooks are triggered when
    switching to the canceling state. The canceler switches to canceled state
    when the hooks have completed. *)

type t

(** [create t] returns a canceler in waiting state. *)
val create : unit -> t

(** If [t] is in wait state, [cancel t] triggers the cancelation process:
    1. it switches to canceling state,
    2. executes the hooks sequentially in separate Lwt threads,
    3. waits for hooks execution to complete,
    4. switches to cancel state.
    If [t] is in canceled state, [cancel t] is determined immediately.
    If [t] is in canceling state, [cancel t] is determined at the end of the
    cancelation process. *)
val cancel : t -> unit Lwt.t

(** [cancelation t] is determined when [t] is in canceling or canceled state. *)
val cancelation : t -> unit Lwt.t

(** [on_cancel t hook] adds [hook] to the end of the current chain. *)
val on_cancel : t -> (unit -> unit Lwt.t) -> unit

(** [canceled t] is [true] iff [t] is canceled or canceling. *)
val canceled : t -> bool
