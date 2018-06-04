(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type t = {
  hash : Block_hash.t ;
  header : Block_header.t ;
  operations : Operation.t list ;
  context : Tezos_protocol_environment_memory.Context.t ; (** Resulting context *)
}
type block = t

val rpc_ctxt: t Alpha_environment.RPC_context.simple

(** Policies to select the next baker *)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

(** Returns (account, priority, timestamp) of the next baker given
    a policy, defaults to By_priority 0. *)
val get_next_baker:
  ?policy:baker_policy ->
  t -> (public_key_hash * int * Time.t) tzresult Lwt.t

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
    ?operations: Operation.t list ->
    t -> header tzresult Lwt.t

  (** Sets seed_nonce_hash of a header *)
  val set_seed_nonce_hash:
    Nonce_hash.t option ->
    header -> header

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
  ?first_free_baking_slot:int ->
  ?endorsers_per_block:int ->
  ?hard_gas_limit_per_operation:Z.t ->
  ?hard_gas_limit_per_block:Z.t ->
  ?proof_of_work_threshold:int64 ->
  ?dictator_pubkey:public_key ->
  ?max_operation_data_length:int ->
  ?tokens_per_roll:Tez_repr.tez ->
  ?michelson_maximum_type_size:int ->
  ?seed_nonce_revelation_tip:Tez_repr.tez ->
  ?origination_burn:Tez_repr.tez ->
  ?block_security_deposit:Tez_repr.tez ->
  ?endorsement_security_deposit:Tez_repr.tez ->
  ?block_reward:Tez_repr.tez ->
  ?endorsement_reward:Tez_repr.tez ->
  ?cost_per_byte: Tez_repr.t ->
  ?hard_storage_limit_per_operation: Int64.t ->
  ?hard_storage_limit_per_block: Int64.t ->
  ?commitments:Commitment_repr.t list ->
  ?security_deposit_ramp_up_cycles: int option ->
  ?no_reward_cycles: int option ->
  (Account.t * Tez_repr.tez) list -> block tzresult Lwt.t

(** Applies a header and its operations to a block and obtains a new block *)
val apply:
  Forge.header ->
  ?operations: Operation.t list ->
  t -> t tzresult Lwt.t

(**
   [bake b] returns a block [b'] which has as predecessor block [b].
   Optional parameter [policy] allows to pick the next baker in several ways.
   This function bundles together [forge_header] and [apply].
*)
val bake:
  ?policy: baker_policy ->
  ?operation: Operation.t ->
  ?operations: Operation.t list ->
  t -> t tzresult Lwt.t

(** Bakes [n] blocks. *)
val bake_n : ?policy:baker_policy -> int -> t -> block tzresult Lwt.t

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end : ?policy:baker_policy -> t -> t tzresult Lwt.t
