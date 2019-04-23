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

(** Tezos Protocol Implementation - Typed storage

    This module hides the hierarchical (key x value) database under
    pre-allocated typed accessors for all persistent entities of the
    tezos context.

    This interface enforces no invariant on the contents of the
    database. Its goal is to centralize all accessors in order to have
    a complete view over the database contents and avoid key
    collisions. *)

open Storage_sigs

module Last_block_priority : sig
  val get : Raw_context.t -> int tzresult Lwt.t
  val set : Raw_context.t -> int -> Raw_context.t tzresult Lwt.t
  val init : Raw_context.t -> int -> Raw_context.t tzresult Lwt.t
end

module Roll : sig

  (** Storage from this submodule must only be accessed through the
      module `Roll`. *)

  module Owner : Indexed_data_snapshotable_storage
    with type key = Roll_repr.t
     and type snapshot = (Cycle_repr.t * int)
     and type value = Signature.Public_key.t
     and type t := Raw_context.t

  val clear: Raw_context.t -> Raw_context.t Lwt.t

  (** The next roll to be allocated. *)
  module Next : Single_data_storage
    with type value = Roll_repr.t
     and type t := Raw_context.t

  (** Rolls linked lists represent both account owned and free rolls.
      All rolls belongs either to the limbo list or to an owned list. *)

  (** Head of the linked list of rolls in limbo *)
  module Limbo : Single_data_storage
    with type value = Roll_repr.t
     and type t := Raw_context.t

  (** Rolls associated to contracts, a linked list per contract *)
  module Delegate_roll_list : Indexed_data_storage
    with type key = Signature.Public_key_hash.t
     and type value = Roll_repr.t
     and type t := Raw_context.t

  (** Use this to iter on a linked list of rolls *)
  module Successor : Indexed_data_storage
    with type key = Roll_repr.t
     and type value = Roll_repr.t
     and type t := Raw_context.t

  (** The tez of a contract that are not assigned to rolls *)
  module Delegate_change : Indexed_data_storage
    with type key = Signature.Public_key_hash.t
     and type value = Tez_repr.t
     and type t := Raw_context.t

  (** Index of the randomly selected roll snapshot of a given cycle. *)
  module Snapshot_for_cycle : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = int
     and type t := Raw_context.t

  (** Last roll in the snapshoted roll allocation of a given cycle. *)
  module Last_for_snapshot : Indexed_data_storage
    with type key = int
     and type value = Roll_repr.t
     and type t = Raw_context.t * Cycle_repr.t

end

module Contract : sig

  (** Storage from this submodule must only be accessed through the
      module `Contract`. *)

  module Global_counter : sig
    val get : Raw_context.t -> Z.t tzresult Lwt.t
    val set : Raw_context.t -> Z.t -> Raw_context.t tzresult Lwt.t
    val init : Raw_context.t -> Z.t -> Raw_context.t tzresult Lwt.t
  end

  (** The domain of alive contracts *)
  val fold :
    Raw_context.t ->
    init:'a -> f:(Contract_repr.t -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  val list : Raw_context.t -> Contract_repr.t list Lwt.t

  (** All the tez possesed by a contract, including rolls and change *)
  module Balance : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Tez_repr.t
     and type t := Raw_context.t

  (** Frozen balance, see 'delegate_storage.mli' for more explanation.
      Always update `Delegates_with_frozen_balance` accordingly. *)
  module Frozen_deposits : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Tez_repr.t
     and type t = Raw_context.t * Contract_repr.t

  module Frozen_fees : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Tez_repr.t
     and type t = Raw_context.t * Contract_repr.t

  module Frozen_rewards : Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Tez_repr.t
     and type t = Raw_context.t * Contract_repr.t

  (** The manager of a contract *)
  module Manager : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Manager_repr.t
     and type t := Raw_context.t

  (** The delegate of a contract, if any. *)
  module Delegate : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Signature.Public_key_hash.t
     and type t := Raw_context.t

  module Delegated : Data_set_storage
    with type elt = Contract_hash.t
     and type t = Raw_context.t * Contract_repr.t

  module Inactive_delegate : Data_set_storage
    with type elt = Contract_repr.t
     and type t = Raw_context.t

  (** The cycle where the delegate should be desactivated. *)
  module Delegate_desactivation : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Cycle_repr.t
     and type t := Raw_context.t

  module Spendable : Data_set_storage
    with type elt = Contract_repr.t
     and type t := Raw_context.t

  module Delegatable : Data_set_storage
    with type elt = Contract_repr.t
     and type t := Raw_context.t

  module Counter : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Z.t
     and type t := Raw_context.t

  module Code : Non_iterable_indexed_carbonated_data_storage
    with type key = Contract_repr.t
     and type value = Script_repr.lazy_expr
     and type t := Raw_context.t

  module Storage : Non_iterable_indexed_carbonated_data_storage
    with type key = Contract_repr.t
     and type value = Script_repr.lazy_expr
     and type t := Raw_context.t

  (** Current storage space in bytes.
      Includes code, global storage and big map elements. *)
  module Used_storage_space : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Z.t
     and type t := Raw_context.t

  (** Maximal space available without needing to burn new fees. *)
  module Paid_storage_space : Indexed_data_storage
    with type key = Contract_repr.t
     and type value = Z.t
     and type t := Raw_context.t

  type bigmap_key = Raw_context.t * Contract_repr.t

  module Big_map : Non_iterable_indexed_carbonated_data_storage
    with type key = Script_expr_hash.t
     and type value = Script_repr.expr
     and type t := bigmap_key

end

(** Set of all registered delegates. *)
module Delegates : Data_set_storage
  with type t := Raw_context.t
   and type elt = Signature.Public_key_hash.t

(** Set of all active delegates with rolls. *)
module Active_delegates_with_rolls : Data_set_storage
  with type t := Raw_context.t
   and type elt = Signature.Public_key_hash.t

(** Set of all the delegates with frozen rewards/bonds/fees for a given cycle. *)
module Delegates_with_frozen_balance : Data_set_storage
  with type t = Raw_context.t * Cycle_repr.t
   and type elt = Signature.Public_key_hash.t

(** Votes *)

module Vote : sig

  module Current_period_kind : Single_data_storage
    with type value = Voting_period_repr.kind
     and type t := Raw_context.t

  module Current_quorum : Single_data_storage
    with type value = int32 (* in centile of percentage *)
     and type t := Raw_context.t

  module Current_proposal : Single_data_storage
    with type value = Protocol_hash.t
     and type t := Raw_context.t

  module Listings_size : Single_data_storage
    with type value = int32 (* total number of rolls in the listing. *)
     and type t := Raw_context.t

  module Listings : Indexed_data_storage
    with type key = Signature.Public_key_hash.t
     and type value = int32 (* number of rolls for the key. *)
     and type t := Raw_context.t

  module Proposals : Data_set_storage
    with type elt = Protocol_hash.t * Signature.Public_key_hash.t
     and type t := Raw_context.t

  module Proposals_count : Indexed_data_storage
    with type key = Signature.Public_key_hash.t
     and type value = int
     and type t := Raw_context.t

  module Ballots : Indexed_data_storage
    with type key = Signature.Public_key_hash.t
     and type value = Vote_repr.ballot
     and type t := Raw_context.t

end

(** Seed *)

module Seed : sig

  (** Storage from this submodule must only be accessed through the
      module `Seed`. *)

  type unrevealed_nonce = {
    nonce_hash: Nonce_hash.t ;
    delegate: Signature.Public_key_hash.t ;
    rewards: Tez_repr.t ;
    fees: Tez_repr.t ;
  }

  type nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  module Nonce : Non_iterable_indexed_data_storage
    with type key := Level_repr.t
     and type value := nonce_status
     and type t := Raw_context.t

  module For_cycle : sig
    val init : Raw_context.t -> Cycle_repr.t -> Seed_repr.seed -> Raw_context.t tzresult Lwt.t
    val get : Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t
    val delete : Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
  end

end

(** Commitments *)

module Commitments : Indexed_data_storage
  with type key = Blinded_public_key_hash.t
   and type value = Tez_repr.t
   and type t := Raw_context.t

(** Ramp up security deposits... *)

module Ramp_up : sig

  module Rewards :
    Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Tez_repr.t * Tez_repr.t (* baking * endorsement *)
     and type t := Raw_context.t

  module Security_deposits :
    Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Tez_repr.t * Tez_repr.t (* baking * endorsement *)
     and type t := Raw_context.t

end
