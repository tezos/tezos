(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val init:
  Raw_context.t ->
  ?ramp_up_cycles:int ->
  ?no_reward_cycles:int ->
  Parameters_repr.bootstrap_account list ->
  Raw_context.t tzresult Lwt.t

val cycle_end:
  Raw_context.t ->
  Cycle_repr.t ->
  Raw_context.t tzresult Lwt.t
