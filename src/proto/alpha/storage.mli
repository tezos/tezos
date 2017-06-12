(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Typed storage accessors

    This module hides the hierarchical (key x value) database under
    pre-allocated typed accessors for all persistent entities of the
    tezos context.

    This interface enforces no invariant on the contents of the
    database. Its goal is to centralize all accessors in order to have
    a complete view over the database contents and avoid key
    collisions. *)


(** {1 Abstract Context} *****************************************************)

(** Abstract view of the database *)
type t

(** Is first block validated with this version of the protocol ? *)
val is_first_block: Context.t -> bool tzresult Lwt.t

(** Retrieves the state of the database and gives its abstract view.
    It also returns wether this is the first block validated
    with this version of the protocol. *)
val prepare :
  level: Int32.t ->
  timestamp: Time.t ->
  fitness: Fitness.t ->
  Context.t -> (t * bool) tzresult Lwt.t

(** Returns the state of the database resulting of operations on its
    abstract view *)
val recover : t -> Context.t

val get_sandboxed : Context.t -> Data_encoding.json option tzresult Lwt.t
val set_sandboxed : Context.t -> Data_encoding.json -> Context.t Lwt.t

val current_level : t -> Level_repr.t
val current_timestamp : t -> Time.t

val current_fitness : t -> Int64.t
val set_current_fitness : t -> Int64.t -> t

val constants : t -> Constants_repr.constants
val first_level : t -> Raw_level_repr.t

(** {1 Entity Accessors} *****************************************************)

open Storage_sigs

module Roll : sig

  (** Storage from this submodule must only be accessed through the
      module `Roll`. *)

  module Owner : Indexed_data_storage
    with type key = Roll_repr.t
     and type value = Contract_repr.t
     and type context := t

  (** The next roll to be allocated. *)
  module Next : Single_data_storage
    with type value = Roll_repr.t
     and type context := t

  (** Rolls linked lists represent both account owned and free rolls.
      All rolls belongs either to the limbo list or to an owned list. *)

  (** Head of the linked list of rolls in limbo *)
  module Limbo : Single_optional_data_storage
    with type value = Roll_repr.t
     and type context := t

  (** Rolls associated to contracts, a linked list per contract *)
  module Contract_roll_list : Indexed_optional_data_storage
    with type key = Contract_repr.t
     and type value = Roll_repr.t
     and type context := t

  (** Use this to iter on a linked list of rolls *)
  module Successor : Indexed_optional_data_storage
    with type key = Roll_repr.t
     and type value = Roll_repr.t
     and type context := t

  (** The tez of a contract that are not assigned to rolls *)
  module Contract_change : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Tez_repr.t
     and type context := t

  (** Frozen rolls per cycle *)

  module Last_for_cycle : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Roll_repr.t
     and type context := t

  module Owner_for_cycle : Indexed_data_storage
    with type key = Cycle_repr.t * Roll_repr.t
     and type value = Ed25519.Public_key_hash.t
     and type context := t

end

module Contract : sig

  (** Storage from this submodule must only be accessed through the
      module `Contract`. *)

  module Global_counter : sig
    val get : t -> int32 tzresult Lwt.t
    val set : t -> int32 -> t tzresult Lwt.t
    val init : t -> int32 -> t tzresult Lwt.t
  end

  (** The domain of alive contracts *)
  module Set : Data_set_storage
    with type value = Contract_repr.t
     and type context := t

  (** All the tez possesed by a contract, including rolls and change *)
  module Balance : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Tez_repr.t
     and type context := t

  (** The manager of a contract *)
  module Manager : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Ed25519.Public_key_hash.t
     and type context := t

  (** The delegate of a contract, if any. *)
  module Delegate : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Ed25519.Public_key_hash.t
     and type context := t

  module Spendable : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = bool
     and type context := t

  module Delegatable : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = bool
     and type context := t

  module Counter : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = int32
     and type context := t

  module Code : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Script_repr.code
     and type context := t

  module Storage : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Script_repr.storage
     and type context := t

  module Code_fees : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Tez_repr.t
     and type context := t

  module Storage_fees : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Tez_repr.t
     and type context := t

end

(** Votes *)

module Vote : sig

  module Current_period_kind : Single_data_storage
    with type value = Voting_period_repr.kind
     and type context := t

  module Current_quorum : Single_data_storage
    with type value = int32 (* in centile of percentage *)
     and type context := t

  module Current_proposal : Single_data_storage
    with type value = Protocol_hash.t
     and type context := t

  module Listings_size : Single_data_storage
    with type value = int32 (* total number of rolls in the listing. *)
     and type context := t

  module Listings : Iterable_data_storage
    with type key = Ed25519.Public_key_hash.t
     and type value = int32 (* number of rolls for the key. *)
     and type context := t

  module Proposals : Data_set_storage
    with type value = Protocol_hash.t * Ed25519.Public_key_hash.t
     and type context := t

  module Ballots : Iterable_data_storage
    with type key = Ed25519.Public_key_hash.t
     and type value = Vote_repr.ballot
     and type context := t

end


(** Keys *)

module Public_key : Iterable_data_storage
  with type key = Ed25519.Public_key_hash.t
   and type value = Ed25519.Public_key.t
   and type context := t

(** Seed *)

module Seed : sig

  (** Storage from this submodule must only be accessed through the
      module `Seed`. *)

  type nonce_status =
    | Unrevealed of {
        nonce_hash: Tezos_hash.Nonce_hash.t ;
        delegate_to_reward: Ed25519.Public_key_hash.t ;
        reward_amount: Tez_repr.t ;
      }
    | Revealed of Seed_repr.nonce

  module Nonce : Indexed_data_storage
    with type key = Level_repr.t
     and type value = nonce_status
     and type context := t

  module For_cycle : sig
    val init : t -> Cycle_repr.t -> Seed_repr.seed -> t tzresult Lwt.t
    val get : t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t
    val delete : t -> Cycle_repr.t -> t tzresult Lwt.t
  end

end

(** Rewards *)

module Rewards : sig

  module Next : Single_data_storage
    with type value = Cycle_repr.t
     and type context := t

  module Date : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Time.t
     and type context := t

  module Amount : Iterable_data_storage
    with type key = Ed25519.Public_key_hash.t * Cycle_repr.t
     and type value = Tez_repr.t
     and type context := t

end

val activate: t -> Protocol_hash.t -> t Lwt.t
val fork_test_network: t -> Protocol_hash.t -> Time.t -> t Lwt.t
