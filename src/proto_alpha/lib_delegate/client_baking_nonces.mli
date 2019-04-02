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

open Proto_alpha.Alpha_context

type t = Nonce.t Block_hash.Map.t

val encoding: t Data_encoding.t

val empty: t

val load: #Client_context.wallet -> [ `Nonce ] Client_baking_files.location -> t tzresult Lwt.t

val save: #Client_context.wallet -> [ `Nonce ] Client_baking_files.location -> t -> unit tzresult Lwt.t

val mem: t -> Block_hash.t -> bool

val find_opt: t -> Block_hash.t -> Nonce.t option

val add: t -> Block_hash.t -> Nonce.t -> t

val add_all: t -> t -> t

val remove: t -> Block_hash.t -> t

val remove_all: t -> t -> t

(** [get_outdated_nonces] returns the nonces that cannot be associated
    to blocks (orphans) and the nonces that are older than 5 cycles. *)
val get_outdated_nonces:
  #Proto_alpha.full ->
  ?constants:Constants.t ->
  chain:Block_services.chain ->
  t ->
  (t * t) tzresult Lwt.t

(** [filter_outdated_nonces] filters nonces older than 5 cycles in the
    nonce file. *)
val filter_outdated_nonces:
  #Proto_alpha.full ->
  ?constants: Constants.t ->
  [ `Nonce ] Client_baking_files.location ->
  t ->
  t tzresult Lwt.t

(** [get_unrevealed_nonces] retrieve registered nonces *)
val get_unrevealed_nonces:
  #Proto_alpha.full ->
  [ `Nonce ] Client_baking_files.location ->
  t ->
  (Raw_level.t * Nonce.t) list tzresult Lwt.t
