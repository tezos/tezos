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

open Alpha_context

val list:
  'a #RPC_context.simple -> 'a ->
  ?active:bool ->
  ?inactive:bool ->
  unit -> Signature.Public_key_hash.t list shell_tzresult Lwt.t

type info = {
  balance: Tez.t ;
  frozen_balance: Tez.t ;
  frozen_balance_by_cycle: Delegate.frozen_balance Cycle.Map.t ;
  staking_balance: Tez.t ;
  delegated_contracts: Contract_hash.t list ;
  delegated_balance: Tez.t ;
  deactivated: bool ;
  grace_period: Cycle.t ;
}

val info_encoding: info Data_encoding.t

val info:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  info shell_tzresult Lwt.t

val balance:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val frozen_balance:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val frozen_balance_by_cycle:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Delegate.frozen_balance Cycle.Map.t shell_tzresult Lwt.t

val staking_balance:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val delegated_contracts:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Contract_hash.t list shell_tzresult Lwt.t

val delegated_balance:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val deactivated:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  bool shell_tzresult Lwt.t

val grace_period:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Cycle.t shell_tzresult Lwt.t


module Baking_rights : sig

  type t = {
    level: Raw_level.t ;
    delegate: Signature.Public_key_hash.t ;
    priority: int ;
    timestamp: Timestamp.t option ;
  }

  (** Retrieves the list of delegates allowed to bake a block.

      By default, it gives the best baking priorities for bakers
      that have at least one opportunity below the 64th priority for
      the next block.

      Parameters [levels] and [cycles] can be used to specify the
      (valid) level(s) in the past or future at which the baking rights
      have to be returned. Parameter [delegates] can be used to
      restrict the results to the given delegates. If parameter [all]
      is [true], all the baking opportunities for each baker at each level
      are returned, instead of just the first one.

      Returns the list of baking slots. Also returns the minimal
      timestamps that correspond to these slots. The timestamps are
      omitted for levels in the past, and are only estimates for levels
      later that the next block, based on the hypothesis that all
      predecessor blocks were baked at the first priority. *)
  val get:
    'a #RPC_context.simple ->
    ?levels: Raw_level.t list ->
    ?cycles: Cycle.t list ->
    ?delegates: Signature.public_key_hash list ->
    ?all: bool ->
    ?max_priority: int ->
    'a -> t list shell_tzresult Lwt.t

end

module Endorsing_rights : sig

  type t = {
    level: Raw_level.t ;
    delegate: Signature.Public_key_hash.t ;
    slots: int list ;
    estimated_time: Timestamp.t option ;
  }

  (** Retrieves the delegates allowed to endorse a block.

      By default, it gives the endorsement slots for bakers that have
      at least one in the next block.

      Parameters [levels] and [cycles] can be used to specify the
      (valid) level(s) in the past or future at which the endorsement
      rights have to be returned. Parameter [delegates] can be used to
      restrict the results to the given delegates.  Returns the list of
      endorsement slots. Also returns the minimal timestamps that
      correspond to these slots.

      Timestamps are omitted for levels in the past, and are only
      estimates for levels later that the next block, based on the
      hypothesis that all predecessor blocks were baked at the first
      priority. *)
  val get:
    'a #RPC_context.simple ->
    ?levels: Raw_level.t list ->
    ?cycles: Cycle.t list ->
    ?delegates: Signature.public_key_hash list ->
    'a -> t list shell_tzresult Lwt.t

end

(* temporary export for deprecated unit test *)
val endorsement_rights:
  Alpha_context.t ->
  Level.t ->
  public_key_hash list tzresult Lwt.t

val baking_rights:
  Alpha_context.t ->
  int option ->
  (Raw_level.t * (public_key_hash * Time.t option) list) tzresult Lwt.t

val register: unit -> unit
