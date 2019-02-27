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

(** A type for sparse block locator (/à la/ Bitcoin). *)
type t = private raw

(** Non private version of Block_store_locator.t for coercions. *)
and raw = Block_header.t * Block_hash.t list

val raw: t -> raw
val pp: Format.formatter -> t -> unit
val pp_short: Format.formatter -> t -> unit
val encoding: t Data_encoding.t
val bounded_encoding:
  ?max_header_size:int ->
  ?max_length:int ->
  unit -> t Data_encoding.t

(** Argument to the seed used to randomize the locator. *)
type seed = {
  sender_id: P2p_peer.Id.t ;
  receiver_id: P2p_peer.Id.t
}

(** [estimated_length seed locator] estimate the length of the chain
    represented by [locator] using [seed]. *)
val estimated_length: seed -> t -> int

(** [compute ~get_predecessor ~caboose ~size block_hash header seed] returns
    a sparse block locator whose header is the given [header] and whose
    sparse block is computed using [seed] to compute random jumps from
    the [block_hash], adding the [caboose] at the end of the sparse block.
    The sparse block locator contains at most [size + 1] elements, including the
    caboose. *)
val compute:
  get_predecessor: (Block_hash.t -> int -> Block_hash.t option Lwt.t) ->
  caboose:Block_hash.t -> size:int -> Block_hash.t -> Block_header.t ->
  seed -> t Lwt.t

(** A 'step' in a locator is a couple of consecutive hashes in the
    locator, and the expected difference of level between the two
    blocks (or an upper bounds when [strict_step = false]). *)
type step = {
  block: Block_hash.t ;
  predecessor: Block_hash.t ;
  step: int ;
  strict_step: bool ;
}

val pp_step: Format.formatter -> step -> unit

(** [to_steps seed t] builds all the 'steps' composing the locator
    using the given [seed], starting with the oldest one
    (typically the predecessor of the first step will be the `caboose`).
    All steps contains [strict_step = true], except the oldest one. *)
val to_steps: seed -> t -> step list

(** [to_steps_truncate ~limit ~save_point seed t] behaves as [to_steps]
    except that when the sum of all the steps already done, and the steps
    to do in order to reach the next block is superior to [limit],
    we return a truncated list of steps, setting the [predecessor] of the
    last step as [save_point] and its field [strict] to [false]. *)
val to_steps_truncate: limit:int -> save_point:Block_hash.t ->
  seed -> t -> step list

(** A block can either be known valid, invalid or unknown. *)
type validity =
  | Unknown
  | Known_valid
  | Known_invalid

(** [unknown_prefix ~is_known t] either returns :

    - [(Known_valid, (h, hist))] when we find a known valid block in the
      locator history (w.r.t [is_known]), where [h] is the given locator header
      and [hist] is the unknown prefix ending with the known valid block.

    - [(Known_invalid, (h, hist))] when we find a known invalid block
      (w.r.t [is_known]) in the locator history, where [h] is the given locator header
      and [hist] is the unknown prefix ending with the known invalid block.

    - [(Unknown, (h, hist))] when no block is known valid nor invalid
      (w.r.t [is_known]), where [(h, hist)] is the given [locator]. *)
val unknown_prefix:
  is_known:(Block_hash.t -> validity Lwt.t) ->
  t -> (validity * t) Lwt.t
