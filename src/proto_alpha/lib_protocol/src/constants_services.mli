(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val preserved_cycles:
  'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t

val blocks_per_cycle:
  'a #RPC_context.simple -> 'a -> int32 shell_tzresult Lwt.t

val blocks_per_commitment:
  'a #RPC_context.simple -> 'a -> int32 shell_tzresult Lwt.t

val blocks_per_roll_snapshot:
  'a #RPC_context.simple -> 'a -> int32 shell_tzresult Lwt.t

val blocks_per_voting_period:
  'a #RPC_context.simple -> 'a -> int32 shell_tzresult Lwt.t

val time_between_blocks:
  'a #RPC_context.simple -> 'a -> Period.t list shell_tzresult Lwt.t

val first_free_baking_slot:
  'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t

val endorsers_per_block:
  'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t

val hard_gas_limits:
  'a #RPC_context.simple -> 'a -> (Z.t * Z.t) shell_tzresult Lwt.t


val cost_per_byte:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val proof_of_work_threshold:
  'a #RPC_context.simple -> 'a -> Int64.t shell_tzresult Lwt.t

val seed_nonce_revelation_tip:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val origination_burn:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val block_security_deposit:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val endorsement_security_deposit:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val block_reward:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val endorsement_reward:
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val errors:
  'a #RPC_context.simple -> 'a -> Data_encoding.json_schema shell_tzresult Lwt.t

val all:
  'a #RPC_context.simple -> 'a -> Constants.t shell_tzresult Lwt.t
