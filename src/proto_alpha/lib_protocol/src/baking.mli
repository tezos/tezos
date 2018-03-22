(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Alpha_context
open Misc

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)
type error += Invalid_endorsement_slot of int * int (* `Permanent *)
type error += Timestamp_too_early of Timestamp.t * Timestamp.t (* `Permanent *)
type error += Inconsistent_endorsement of public_key_hash list (* `Permanent *)
type error += Cannot_freeze_baking_deposit (* `Permanent *)
type error += Cannot_freeze_endorsement_deposit (* `Permanent *)
type error += Invalid_block_signature of Block_hash.t * Signature.Public_key_hash.t (* `Permanent *)

(** [minimal_time ctxt priority pred_block_time] returns the minimal
    time, given the predecessor block timestamp [pred_block_time],
    after which a baker with priority [priority] is allowed to
    bake. Fail with [Invalid_time_between_blocks_constant] if the minimal
    time cannot be computed. *)
val minimal_time: context -> int -> Time.t -> Time.t tzresult Lwt.t

(** [freeze_baking_deposit: ctxt delegate priority]
    Freeze the baking deposit (See !Constants.block_security_deposit)
    from a delegate account. No deposit is frozen if the baking
    priority of this block is greater than the maximum number
    of paying baking in the network (meaning that n. bakers
    skipped their turn).

    Raise an error if the delegate account does not have enough
    funds to claim baking rights. *)
val freeze_baking_deposit:
  context ->
  public_key_hash ->
  (context * Tez.t) tzresult Lwt.t

(** [freeze_endorsement_deposit: ctxt delegate]
    Freeze the endorsement deposit (See !Constants.endorsement_security_deposit)
    from the delegate account.

    Raise an error if the baker account does not have enough
    funds to claim endorsement rights *)
val freeze_endorsement_deposit:
  context -> public_key_hash -> int -> context tzresult Lwt.t

(** [check_baking_rights ctxt block pred_timestamp] verifies that:
    * the contract that owned the roll at cycle start has the block signer as delegate.
    * the timestamp is coherent with the announced slot.
*)
val check_baking_rights:
  context -> Block_header.contents -> Time.t ->
  public_key tzresult Lwt.t

(** [check_endorsements_rights c slots]:
    * verifies that the endorsement slots are valid ;
    * verifies that the endorsement slots correspond to the same delegate at the current level;
*)
val check_endorsements_rights:
  context -> Level.t -> int list -> public_key tzresult Lwt.t

(** Returns the endorsement reward calculated w.r.t a given priotiry.  *)
val endorsement_reward: context -> block_priority:int -> int -> Tez.t tzresult Lwt.t

(** [baking_priorities ctxt level] is the lazy list of contract's
    public key hashes that are allowed to bake for [level]. *)
val baking_priorities:
  context -> Level.t -> public_key lazy_list

(** [endorsement_priorities ctxt level] is the lazy list of contract's
    public key hashes that are allowed to endorse for [level]. *)
val endorsement_priorities:
  context -> Level.t -> public_key lazy_list

(** [first_baking_priorities ctxt ?max_priority contract_hash level]
    is a list of priorities of max [?max_priority] elements, where the
    delegate of [contract_hash] is allowed to bake for [level]. If
    [?max_priority] is [None], a sensible number of priorities is
    returned. *)
val first_baking_priorities:
  context ->
  ?max_priority:int ->
  public_key_hash ->
  Level.t ->
  int list tzresult Lwt.t

val first_endorsement_slots:
  context ->
  ?max_priority:int ->
  public_key_hash ->
  Level.t -> int list tzresult Lwt.t

(** [check_signature ctxt block id] check if the block is signed with
    the given key *)
val check_signature: Block_header.t -> public_key -> unit tzresult Lwt.t

(** Checks if the header that would be built from the given components
    is valid for the given diffculty. The signature is not passed as it
    is does not impact the proof-of-work stamp. The stamp is checked on
    the hash of a block header whose signature has been zeroed-out. *)
val check_header_proof_of_work_stamp:
  Block_header.shell_header -> Block_header.contents -> int64 -> bool

(** verify if the proof of work stamp is valid *)
val check_proof_of_work_stamp:
  context -> Block_header.t -> unit tzresult Lwt.t

(** check if the gap between the fitness of the current context
    and the given block is within the protocol parameters *)
val check_fitness_gap:
  context -> Block_header.t -> unit tzresult Lwt.t

val dawn_of_a_new_cycle: context -> Cycle.t option tzresult Lwt.t

val earlier_predecessor_timestamp: context -> Level.t -> Timestamp.t tzresult Lwt.t
