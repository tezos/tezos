(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* This is the genesis protocol: initialise the state *)
let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block
    ~level ~timestamp ~fitness ctxt >>=? fun (param, ctxt) ->
  Commitment_storage.init ctxt param.commitments >>=? fun ctxt ->
  Roll_storage.init ctxt >>=? fun ctxt ->
  Seed_storage.init ctxt >>=? fun ctxt ->
  Contract_storage.init ctxt >>=? fun ctxt ->
  Bootstrap_storage.init ctxt
    ~typecheck
    ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
    ?no_reward_cycles:param.no_reward_cycles
    param.bootstrap_accounts >>=? fun ctxt ->
  Roll_storage.init_first_cycles ctxt >>=? fun ctxt ->
  Vote_storage.init ctxt >>=? fun ctxt ->
  Storage.Last_block_priority.init ctxt 0 >>=? fun ctxt ->
  return ctxt

let prepare ctxt ~level ~timestamp ~fitness =
  Raw_context.prepare ~level ~timestamp ~fitness ctxt
