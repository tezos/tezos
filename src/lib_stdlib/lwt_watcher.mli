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

(** This module implements a one-to-many publish/suscribe pattern.

    Clients can register/unregister to an [input]. Events notified to the input
    (through [notify]) are dispatched asynchronously to all registered clients
    through an [Lwt_stream]. A client receives only events sent after
    registration and before unregistration. *)

type 'a input

val create_input : unit -> 'a input

(** [notify t v] publishes value v to the input t *)
val notify : 'a input -> 'a -> unit

type stopper

(** [create_stream t] registers a new client which can read published
    values via a stream. A [stopper] is used to shutdown the client. *)
val create_stream : 'a input -> 'a Lwt_stream.t * stopper

(** A fake stream never receives any value. *)
val create_fake_stream : unit -> 'a Lwt_stream.t * stopper

(** [shutdown s] unregisters the client associated to [s]. [None] is pushed
    to the stream. *)
val shutdown : stopper -> unit

(** Shutdowns all the clients of this input *)
val shutdown_input : 'a input -> unit
