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

val generate_seed_nonce: unit -> Nonce.t
(** [generate_seed_nonce ()] is a random nonce that is typically used
    in block headers. When baking, bakers generate random nonces whose
    hash is commited in the block they bake. They will typically
    reveal the aforementionned nonce during the next cycle. *)

val inject_block:
  #Proto_alpha.full ->
  ?force:bool ->
  ?seed_nonce_hash:Nonce_hash.t ->
  chain:Chain_services.chain ->
  shell_header:Block_header.shell_header ->
  priority:int ->
  delegate_pkh:Signature.Public_key_hash.t ->
  delegate_sk:Client_keys.sk_uri ->
  level: Raw_level.t ->
  Operation.raw list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt blk ?force ~priority ~timestamp ~fitness
    ~seed_nonce ~src_sk ops] tries to inject a block in the node. If
    [?force] is set, the fitness check will be bypassed. [priority]
    will be used to compute the baking slot (level is
    precomputed). [src_sk] is used to sign the block header. *)

type error +=
  | Failed_to_preapply of Tezos_base.Operation.t * error list

val forge_block:
  #Proto_alpha.full ->
  ?force:bool ->
  ?operations: Operation.packed list ->
  ?best_effort:bool ->
  ?sort:bool ->
  ?minimal_fees: Tez.t ->
  ?minimal_nanotez_per_gas_unit: Z.t ->
  ?minimal_nanotez_per_byte: Z.t ->
  ?await_endorsements: bool ->
  ?timestamp:Time.Protocol.t ->
  ?mempool:string ->
  ?context_path:string ->
  ?seed_nonce_hash:Nonce_hash.t ->
  chain:Chain_services.chain ->
  priority:[`Set of int | `Auto of (public_key_hash * int option)] ->
  delegate_pkh: Signature.Public_key_hash.t ->
  delegate_sk: Client_keys.sk_uri ->
  Block_services.block ->
  Block_hash.t tzresult Lwt.t
(** [forge_block cctxt ?fee_threshold ?force ?operations ?best_effort
    ?sort ?timestamp ?max_priority ?priority ~seed_nonce ~src_sk
    pk_hash parent_blk] injects a block in the node. In addition of inject_block,
    it will:

    * Operations: If [?operations] is [None], it will get pending
      operations and add them to the block. Otherwise, provided
      operations will be used. In both cases, they will be validated.

    * Baking priority: If [`Auto] is used, it will be computed from
      the public key hash of the specified contract, optionally capped
      to a maximum value, and optionnaly restricting for free baking slot.

    * Timestamp: If [?timestamp] is set, and is compatible with the
      computed baking priority, it will be used. Otherwise, it will be
      set at the best baking priority.

    * Fee Threshold: If [?fee_threshold] is given, operations with fees lower than it
      are not added to the block.
*)

val create:
  #Proto_alpha.full ->
  ?minimal_fees: Tez.t ->
  ?minimal_nanotez_per_gas_unit: Z.t ->
  ?minimal_nanotez_per_byte: Z.t ->
  ?await_endorsements: bool ->
  ?max_priority: int ->
  chain: Chain_services.chain ->
  context_path: string ->
  public_key_hash list ->
  Client_baking_blocks.block_info tzresult Lwt_stream.t ->
  unit tzresult Lwt.t
