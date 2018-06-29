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

type t
(** A lightweight scheduler to run tasks concurrently as well as
    special callbacks that must be run in mutual exclusion with the
    tasks (and each other). *)

val create : unit -> t
(** Creates a new task / idle callback scheduler *)

val task : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Schedule a task to be run as soon as no idle callbacks is
    running, or as soon as the next idle callback has been run if it
    was scheduled by {!force_idle}. *)

val when_idle : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Runs a callback as soon as no task is running. Does not prevent
    new tasks from being scheduled, the calling code should ensure
    that some idle time will eventually come. Calling this function
    from inside the callback will result in a dead lock. *)

val force_idle : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Runs a callback as soon as possible. Lets all current tasks
    finish, but postpones all new tasks until the end of the
    callback. Calling this function from inside the callback will
    result in a dead lock. *)
