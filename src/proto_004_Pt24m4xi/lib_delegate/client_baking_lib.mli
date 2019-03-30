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

open Proto_alpha
open Alpha_context

(** Mine a block *)
val bake_block:
  #Proto_alpha.full ->
  ?minimal_fees: Tez.t ->
  ?minimal_nanotez_per_gas_unit: Z.t ->
  ?minimal_nanotez_per_byte: Z.t ->
  ?await_endorsements: bool ->
  ?force: bool ->
  ?max_priority: int ->
  ?minimal_timestamp: bool ->
  ?mempool: string ->
  ?context_path: string ->
  ?src_sk: Client_keys.sk_uri ->
  chain: Chain_services.chain ->
  head: Block_services.block ->
  public_key_hash ->
  unit tzresult Lwt.t

(** Endorse a block *)
val endorse_block:
  #Proto_alpha.full ->
  chain: Chain_services.chain ->
  Client_keys.Public_key_hash.t ->
  unit Error_monad.tzresult Lwt.t

(** Get the previous cycle of the given cycle *)
val get_predecessor_cycle:
  #Proto_alpha.full ->
  Cycle.t ->
  Cycle.t Lwt.t

(** Reveal the nonces used to bake each block in the given list *)
val reveal_block_nonces :
  #Proto_alpha.full ->
  chain: Chain_services.chain ->
  block: Block_services.block ->
  Block_hash.t list ->
  unit Error_monad.tzresult Lwt.t

(** Reveal all unrevealed nonces *)
val reveal_nonces :
  #Proto_alpha.full ->
  chain: Chain_services.chain ->
  block: Block_services.block ->
  unit ->
  unit Error_monad.tzresult Lwt.t
