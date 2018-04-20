(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val list:
  'a #RPC_context.simple -> 'a ->
  ?active:bool ->
  ?inactive:bool ->
  unit -> Signature.Public_key_hash.t list shell_tzresult Lwt.t

type info = {
  balance: Tez.t ;
  frozen_balance: Tez.t ;
  frozen_balances: Delegate.frozen_balance Cycle.Map.t ;
  delegated_balance: Tez.t ;
  delegated_contracts: Contract_hash.t list ;
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

val frozen_balances:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Delegate.frozen_balance Cycle.Map.t shell_tzresult Lwt.t

val delegated_balance:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val delegated_contracts:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Contract_hash.t list shell_tzresult Lwt.t

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

  (** Compute the baking rights. By default, it computes the baking
      rights for the next block and only returns the first available
      priority for bakers that appears in the 64 first priorities.

      The optional arguments [levels] and [cycles] allows to compute
      baking for an explicit list of levels or for all the levels of the given
      cycles.

      The optional argument [delegates] allows to filter
      the non-explicitly listed delegates out of the resulting list.

      When [all=false], the function only returns the minimal priority
      for each delegates. When [all=true], all priorities are returned. *)
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

  (** Compute the endorsing rights. By default, it computes the
      endorsing rights for the next block.

      The optional arguments [levels] and [cycles] allows to compute
      baking for an explicit list of levels or for all the levels of
      the given cycles.

      The optional argument [delegates] allows to filter the
      non-explicitly listed delegates out of the resulting list.. *)
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
