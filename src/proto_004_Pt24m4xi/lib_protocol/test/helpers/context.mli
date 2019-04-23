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
open Alpha_environment
open Alpha_context

type t =
  | B of Block.t
  | I of Incremental.t

val branch: t -> Block_hash.t

val get_level: t -> Raw_level.t tzresult Lwt.t

val get_endorsers: t -> Alpha_services.Delegate.Endorsing_rights.t list tzresult Lwt.t

val get_endorser: t -> (public_key_hash * int list) tzresult Lwt.t

val get_bakers: t -> public_key_hash list tzresult Lwt.t

val get_seed_nonce_hash: t -> Nonce_hash.t tzresult Lwt.t

(** Returns the seed of the cycle to which the block belongs to. *)
val get_seed: t -> Seed.seed tzresult Lwt.t

(** Returns all the constants of the protocol *)
val get_constants: t -> Constants.t tzresult Lwt.t

module Vote : sig
  val get_ballots: t -> Vote.ballots tzresult Lwt.t
  val get_ballot_list: t -> (Signature.Public_key_hash.t * Vote.ballot) list tzresult Lwt.t
  val get_voting_period: t -> Voting_period.t tzresult Lwt.t
  val get_voting_period_position: t -> Int32.t tzresult Lwt.t
  val get_current_period_kind: t -> Voting_period.kind tzresult Lwt.t
  val get_current_quorum: t -> Int32.t tzresult Lwt.t
  val get_listings: t -> (Signature.Public_key_hash.t * int32) list tzresult Lwt.t
  val get_proposals: t -> Int32.t Protocol_hash.Map.t tzresult Lwt.t
  val get_current_proposal: t -> Protocol_hash.t option tzresult Lwt.t
  val get_protocol : Block.t -> Protocol_hash.t Lwt.t
end

module Contract : sig

  val pp : Format.formatter -> Contract.t -> unit
  val pkh: Contract.t -> public_key_hash tzresult Lwt.t

  type balance_kind = Main | Deposit | Fees | Rewards

  (** Returns the balance of a contract, by default the main balance.
      If the contract is implicit the frozen balances are available too:
      deposit, fees ot rewards. *)
  val balance: ?kind:balance_kind -> t -> Contract.t -> Tez.t tzresult Lwt.t

  val counter: t -> Contract.t -> Z.t tzresult Lwt.t
  val manager: t -> Contract.t -> Account.t tzresult Lwt.t
  val is_manager_key_revealed: t -> Contract.t -> bool tzresult Lwt.t

  val delegate: t -> Contract.t -> public_key_hash tzresult Lwt.t
  val delegate_opt: t -> Contract.t -> public_key_hash option tzresult Lwt.t

end

module Delegate : sig

  type info = Delegate_services.info = {
    balance: Tez.t ;
    frozen_balance: Tez.t ;
    frozen_balance_by_cycle: Delegate.frozen_balance Cycle.Map.t ;
    staking_balance: Tez.t ;
    delegated_contracts: Contract_hash.t list ;
    delegated_balance: Tez.t ;
    deactivated: bool ;
    grace_period: Cycle.t ;
  }

  val info: t -> public_key_hash -> Delegate_services.info tzresult Lwt.t

end

(** [init n] : returns an initial block with [n] initialized accounts
    and the associated implicit contracts *)
val init:
  ?slow: bool ->
  ?preserved_cycles:int ->
  ?endorsers_per_block:int ->
  ?commitments:Commitment_repr.t list ->
  ?initial_balances: int64 list ->
  int -> (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t
