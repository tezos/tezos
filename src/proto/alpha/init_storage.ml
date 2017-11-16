(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* This is the genesis protocol: initialise the state *)
let initialize store =
  Roll_storage.init store >>=? fun store ->
  Seed_storage.init store >>=? fun store ->
  Contract_storage.init store >>=? fun store ->
  Reward_storage.init store >>=? fun store ->
  Bootstrap_storage.init store >>=? fun store ->
  Roll_storage.freeze_rolls_for_cycle
    store Cycle_repr.root >>=? fun store ->
  Roll_storage.freeze_rolls_for_cycle
    store Cycle_repr.(succ root) >>=? fun store ->
  Vote_storage.init store >>=? fun store ->
  return store

let may_initialize ctxt ~level ~timestamp ~fitness =
  Raw_context.prepare
    ~level ~timestamp ~fitness ctxt >>=? fun (ctxt, first_block) ->
  if first_block then
    initialize ctxt
  else
    return ctxt
