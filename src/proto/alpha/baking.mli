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

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)
type error += Invalid_endorsement_slot of int * int (* `Permanent *)
type error += Timestamp_too_early of Timestamp.t * Timestamp.t (* `Permanent *)
type error += Wrong_delegate of public_key_hash * public_key_hash (* `Permanent *)
type error += Cannot_pay_baking_bond (* `Permanent *)
type error += Cannot_pay_endorsement_bond (* `Permanent *)

val paying_priorities: context -> int list

val minimal_time: context -> int -> Time.t -> Time.t tzresult Lwt.t
(** [minimal_time ctxt priority pred_block_time] returns the minimal
    time, given the predecessor block timestamp [pred_block_time],
    after which a miner with priority [priority] is allowed to
    mine. Fail with [Invalid_slot_durations_constant] if the minimal
    time cannot be computed. *)

val pay_baking_bond:
  context ->
  Block_header.proto_header ->
  public_key_hash ->
  context tzresult Lwt.t

val pay_endorsement_bond:
  context -> public_key_hash -> (context * Tez.t) tzresult Lwt.t

(** [check_baking_rights ctxt block pred_timestamp] verifies that:
    * the contract that owned the roll at cycle start has the block signer as delegate.
    * the timestamp is coherent with the announced slot.
    * the bond have been payed if the slot is below [Constants.first_free_baking_slot].
*)
val check_baking_rights:
  context -> Block_header.proto_header -> Time.t ->
  public_key_hash tzresult Lwt.t

(** [check_signing_rights c slot contract] verifies that:
    * the slot is valid;
    * [contract] owned, at cycle start, the roll that has the right to sign
      for the slot and the current level.
*)
val check_signing_rights:
  context -> int -> public_key_hash -> unit tzresult Lwt.t

(** If this priority should have payed the bond it is the base baking
    reward and the bond, or just the base reward otherwise *)
val base_baking_reward: context -> priority:int -> Tez.t

val endorsement_reward: block_priority:int -> Tez.t tzresult Lwt.t

val baking_priorities:
  context -> Level.t -> public_key_hash lazy_list
(** [baking_priorities ctxt level] is the lazy list of contract's
    public key hashes that are allowed to mine for [level]. *)

val endorsement_priorities:
  context -> Level.t -> public_key_hash lazy_list

val first_baking_priorities:
  context ->
  ?max_priority:int ->
  public_key_hash ->
  Level.t ->
  int list tzresult Lwt.t
(** [first_baking_priorities ctxt ?max_priority contract_hash level]
    is a list of priorities of max [?max_priority] elements, where the
    delegate of [contract_hash] is allowed to mine for [level]. If
    [?max_priority] is [None], a sensible number of priorities is
    returned. *)

val first_endorsement_slots:
  context ->
  ?max_priority:int ->
  public_key_hash ->
  Level.t -> int list tzresult Lwt.t

val check_signature:
  context -> Block_header.t -> public_key_hash -> unit tzresult Lwt.t

val check_hash: Block_hash.t -> int64 -> bool
val check_proof_of_work_stamp:
  context -> Block_header.t -> unit tzresult Lwt.t

val check_fitness_gap:
  context -> Block_header.t -> unit tzresult Lwt.t

val dawn_of_a_new_cycle: context -> Cycle.t option tzresult Lwt.t
