(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val record:
  Storage.t -> Ed25519.Public_key_hash.t -> Cycle_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

val discard:
  Storage.t -> Ed25519.Public_key_hash.t -> Cycle_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

val pay_due_rewards: Storage.t -> Storage.t tzresult Lwt.t

val set_reward_time_for_cycle:
  Storage.t -> Cycle_repr.t -> Time.t -> Storage.t tzresult Lwt.t

val init: Storage.t -> Storage.t tzresult Lwt.t
