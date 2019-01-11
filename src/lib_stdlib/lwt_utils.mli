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

val may: f:('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

val never_ending: unit -> 'a Lwt.t

(** [worker name ~on_event ~run ~cancel] runs worker [run], and logs worker
    creation, ending or failure using [~on_event].
    [cancel] is called if worker fails. *)
val worker:
  string ->
  on_event:(string ->
            [ `Ended | `Failed of string | `Started ] -> unit Lwt.t) ->
  run:(unit -> unit Lwt.t) ->
  cancel:(unit -> unit Lwt.t) ->
  unit Lwt.t

val trigger: unit -> (unit -> unit) * (unit -> unit Lwt.t)

val sort: ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t

val unless: bool -> (unit -> unit Lwt.t) -> unit Lwt.t

