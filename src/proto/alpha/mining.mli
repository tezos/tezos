(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Tezos_context
open Misc

val paying_priorities: context -> int32 list

val minimal_time:
  context -> int32 -> Time.t -> Time.t tzresult Lwt.t

val pay_mining_bond:
  context ->
  Block.header ->
  public_key_hash ->
  context tzresult Lwt.t

val pay_endorsement_bond:
  context -> public_key_hash -> (context * Tez.t) tzresult Lwt.t

(** [check_mining_rights ctxt block pred_timestamp] verifies that:
    * the contract that owned the roll at cycle start has the block signer as delegate.
    * the timestamp is coherent with the announced slot.
    * the bond have been payed if the slot is below [Constants.first_free_mining_slot].
*)
val check_mining_rights:
  context -> Block.header -> Time.t -> public_key_hash tzresult Lwt.t

(** [check_signing_rights c slot contract] verifies that:
    * the slot is valid;
    * [contract] owned, at cycle start, the roll that has the right to sign
      for the slot and the current level.
*)
val check_signing_rights:
  context -> int -> public_key_hash -> unit tzresult Lwt.t

(** If this priority should have payed the bond it is the base mining
    reward and the bond, or just the base reward otherwise *)
val base_mining_reward: context -> priority:int32 -> Tez.t

val endorsement_reward: block_priority:int32 -> Tez.t tzresult Lwt.t


(** The contract owning rolls for the first mining priorities of a level. *)
val mining_priorities:
  context -> Level.t -> public_key_hash lazy_list
val endorsement_priorities:
  context -> Level.t -> public_key_hash lazy_list

val first_mining_priorities:
  context ->
  ?max_priority:int32 ->
  public_key_hash ->
  Level.t ->
  int32 list tzresult Lwt.t

val first_endorsement_slots:
  context ->
  ?max_priority:int32 ->
  public_key_hash ->
  Level.t -> int32 list tzresult Lwt.t

val check_signature:
  context -> Block.header -> public_key_hash -> unit tzresult Lwt.t

val check_hash: Block_hash.t -> int64 -> bool
val check_proof_of_work_stamp:
  context -> Block.header -> unit tzresult Lwt.t

type error += Invalid_fitness_gap

val check_fitness_gap:
  context -> Block.header -> unit tzresult Lwt.t

val dawn_of_a_new_cycle: context -> Cycle.t option tzresult Lwt.t
