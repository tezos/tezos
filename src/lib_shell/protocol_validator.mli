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

val create: Distributed_db.t -> t

val validate:
  t ->
  Protocol_hash.t -> Protocol.t ->
  Registered_protocol.t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val fetch_and_compile_protocol:
  t ->
  ?peer:P2p_peer.Id.t ->
  ?timeout:Ptime.Span.t ->
  Protocol_hash.t -> Registered_protocol.t tzresult Lwt.t

val fetch_and_compile_protocols:
  t ->
  ?peer:P2p_peer.Id.t ->
  ?timeout:Ptime.Span.t ->
  State.Block.t -> unit tzresult Lwt.t

val prefetch_and_compile_protocols:
  t ->
  ?peer:P2p_peer.Id.t ->
  ?timeout:Ptime.Span.t ->
  State.Block.t -> unit

