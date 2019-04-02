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

type t = private raw
(** A type for sparse block locator (/à la/ Bitcoin) *)

and raw = Block_header.t * Block_hash.t list
(** Non private version of Block_store_locator.t for coercions *)

val raw: t -> raw
val pp: Format.formatter -> t -> unit
val pp_short: Format.formatter -> t -> unit
val encoding: t Data_encoding.t
val bounded_encoding:
  ?max_header_size:int ->
  ?max_length:int ->
  unit -> t Data_encoding.t

type seed = {
  sender_id: P2p_peer.Id.t ;
  receiver_id: P2p_peer.Id.t
}
(** Argument to the seed used to randomize the locator. *)

val estimated_length: seed -> t -> int
(** [estimated_length seed locator] estimate the length of the chain
    represented by [locator] using [seed]. *)

val compute:
  predecessor: (Block_hash.t -> int -> Block_hash.t option Lwt.t) ->
  genesis:Block_hash.t ->
  Block_hash.t -> Block_header.t -> seed -> size:int -> t Lwt.t
(** [compute block seed max_length] compute the sparse block locator using
    [seed] to compute random jumps for the [block]. The locator contains at
    most [max_length] elements. *)

type step = {
  block: Block_hash.t ;
  predecessor: Block_hash.t ;
  step: int ;
  strict_step: bool ;
}
(** A 'step' in a locator is a couple of consecutive hashes in the
    locator, and the expected difference of level between the two
    blocks (or an upper bounds when [strict_step = false]). *)

val pp_step: Format.formatter -> step -> unit

val to_steps: seed -> t -> step list
(** Build all the 'steps' composing the locator using a given seed,
    starting with the oldest one (typically the predecessor of the
    first step will be `genesis`).
    All steps contains [strict_step = true], except the oldest one. *)

type validity =
  | Unknown
  | Known_valid
  | Known_invalid

val unknown_prefix:
  is_known:(Block_hash.t -> validity Lwt.t) ->
  t -> (Block_hash.t * t) option Lwt.t
(** [unknown_prefix validity locator] keeps only the unknown part of
    the locator up to the first valid block. If there is no known valid
    block or there is a known invalid one, None is returned. *)
