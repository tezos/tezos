(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val record:
  Raw_context.t -> Ed25519.Public_key_hash.t -> Cycle_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val discard:
  Raw_context.t -> Ed25519.Public_key_hash.t -> Cycle_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val pay_due_rewards: Raw_context.t -> Raw_context.t tzresult Lwt.t

val set_reward_time_for_cycle:
  Raw_context.t -> Cycle_repr.t -> Time.t -> Raw_context.t tzresult Lwt.t

val init: Raw_context.t -> Raw_context.t tzresult Lwt.t
