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


val sleep_until: Time.Protocol.t -> unit Lwt.t option

val wait_for_first_event:
  name:string ->
  'event tzresult Lwt_stream.t ->
  'event Lwt.t

val main :
  name:string ->
  cctxt:(#Proto_alpha.full as 'a) ->
  stream:'event tzresult Lwt_stream.t ->
  state_maker:('event -> 'state tzresult Lwt.t) ->
  pre_loop:('a -> 'state -> 'event -> unit tzresult Lwt.t) ->
  compute_timeout:('state -> 'timesup Lwt.t) ->
  timeout_k:('a -> 'state -> 'timesup -> unit tzresult Lwt.t) ->
  event_k:('a -> 'state -> 'event -> unit tzresult Lwt.t) ->
  unit tzresult Lwt.t

(** [main ~name ~cctxt ~stream ~state_maker ~pre_loop ~timeout_maker ~timeout_k
    ~event_k] is an infinitely running loop that
    monitors new events arriving on [stream]. The loop exits when the
    [stream] gives an error.

    The function [pre_loop] is called before the loop starts.

    The loop maintains a state (of type ['state]) initialized by [state_maker]
    and passed to the callbacks [timeout_maker] (used to set up waking-up
    timeouts), [timeout_k] (when a computed timeout happens), and [event_k]
    (when a new event arrives on the stream).
*)
