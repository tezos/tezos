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

type t = {
  hash : Block_hash.t ;
  header : Block_header.t ;
  operations : Operation.packed list ;
  context : Tezos_protocol_environment_memory.Context.t ; (** Resulting context *)
}
type block = t

val rpc_ctxt: t Alpha_environment.RPC_context.simple

(** Policies to select the next baker:
    - [By_priority p] selects the baker at priority [p]
    - [By_account pkh] selects the first slot for baker [pkh]
    - [Excluding pkhs] selects the first baker that doesn't belong to [pkhs]
*)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

(** Returns (account, priority, timestamp) of the next baker given
    a policy, defaults to By_priority 0. *)
val get_next_baker:
  ?policy:baker_policy ->
  t -> (public_key_hash * int * Time.Protocol.t) tzresult Lwt.t

module Forge : sig

  val contents:
    ?proof_of_work_nonce:MBytes.t ->
    ?priority:int ->
    ?seed_nonce_hash: Nonce_hash.t ->
    unit -> Block_header.contents

  type header

  (** Forges a correct header following the policy.
      The header can then be modified and applied with [apply]. *)
  val forge_header:
    ?policy:baker_policy ->
    ?operations: Operation.packed list ->
    t -> header tzresult Lwt.t

  (** Sets uniquely seed_nonce_hash of a header *)
  val set_seed_nonce_hash:
    Nonce_hash.t option -> header -> header

  (** Sets the baker that will sign the header to an arbitrary pkh *)
  val set_baker:
    public_key_hash -> header -> header

  (** Signs the header with the key of the baker configured in the header.
      The header can no longer be modified, only applied. *)
  val sign_header:
    header ->
    Block_header.block_header tzresult Lwt.t

end

(** [genesis <opts> accounts] : generates an initial block with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val genesis:
  ?preserved_cycles:int ->
  ?blocks_per_cycle:int32 ->
  ?blocks_per_commitment:int32 ->
  ?blocks_per_roll_snapshot:int32 ->
  ?blocks_per_voting_period:int32 ->
  ?time_between_blocks:Period_repr.t list ->
  ?endorsers_per_block:int ->
  ?hard_gas_limit_per_operation:Z.t ->
  ?hard_gas_limit_per_block:Z.t ->
  ?proof_of_work_threshold:int64 ->
  ?tokens_per_roll:Tez_repr.tez ->
  ?michelson_maximum_type_size:int ->
  ?seed_nonce_revelation_tip:Tez_repr.tez ->
  ?origination_size:int ->
  ?block_security_deposit:Tez_repr.tez ->
  ?endorsement_security_deposit:Tez_repr.tez ->
  ?block_reward:Tez_repr.tez ->
  ?endorsement_reward:Tez_repr.tez ->
  ?cost_per_byte: Tez_repr.t ->
  ?hard_storage_limit_per_operation: Z.t ->
  ?commitments:Commitment_repr.t list ->
  ?security_deposit_ramp_up_cycles: int option ->
  ?no_reward_cycles: int option ->
  (Account.t * Tez_repr.tez) list -> block tzresult Lwt.t

(** Applies a signed header and its operations to a block and
    obtains a new block *)
val apply:
  Block_header.block_header ->
  ?operations: Operation.packed list ->
  t -> t tzresult Lwt.t

(**
   [bake b] returns a block [b'] which has as predecessor block [b].
   Optional parameter [policy] allows to pick the next baker in several ways.
   This function bundles together [forge_header], [sign_header] and [apply].
   These functions should be used instead of bake to craft unusual blocks for
   testing together with setters for properties of the headers.
   For examples see seed.ml or double_baking.ml
*)
val bake:
  ?policy: baker_policy ->
  ?operation: Operation.packed ->
  ?operations: Operation.packed list ->
  t -> t tzresult Lwt.t

(** Bakes [n] blocks. *)
val bake_n : ?policy:baker_policy -> int -> t -> block tzresult Lwt.t

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end : ?policy:baker_policy -> t -> t tzresult Lwt.t

(** Bakes enough blocks to end [n] cycles. *)
val bake_until_n_cycle_end : ?policy:baker_policy -> int -> t -> t tzresult Lwt.t

(** Bakes enough blocks to reach the cycle. *)
val bake_until_cycle : ?policy:baker_policy -> Cycle.t -> t -> t tzresult Lwt.t
