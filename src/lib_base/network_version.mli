(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  chain_name : Distributed_db_version.name ;
  distributed_db_version : Distributed_db_version.t ;
  p2p_version : P2p_version.t ;
}

val pp: Format.formatter -> t -> unit
val encoding: t Data_encoding.t

(** [announced supported] computes the network protocol version
    announced on peer connection, given the [supported] versions for
    the higher-level messages. *)
val announced:
  chain_name: Distributed_db_version.name ->
  distributed_db_versions: Distributed_db_version.t list ->
  p2p_versions: P2p_version.t list ->
  t

(** [select acceptables remote] computes network protocol version to
    be used on a given connection where [remote] is version annouced
    by the remote peer, and [acceptables] the locally accepted
    versions for the higher-level messages. *)
val select:
  chain_name: Distributed_db_version.name ->
  distributed_db_versions: Distributed_db_version.t list ->
  p2p_versions: P2p_version.t list ->
  t -> t option
