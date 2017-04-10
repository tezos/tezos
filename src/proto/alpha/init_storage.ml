(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

type error +=
  | Unimplemented_sandbox_migration

let may_initialize ctxt ~level ~timestamp ~fitness =
  Storage.prepare ~level ~timestamp ~fitness ctxt >>=? fun (ctxt, first_block) ->
  if first_block then
    initialize ctxt
  else
    return ctxt

let configure_sandbox ctxt json =
  let json =
    match json with
    | None -> `O []
    | Some json -> json in
  Storage.is_first_block ctxt >>=? function
  | true ->
      Storage.set_sandboxed ctxt json >>= fun ctxt ->
      return ctxt
  | false ->
      Storage.get_sandboxed ctxt >>=? function
      | None ->
          fail Unimplemented_sandbox_migration
      | Some _ ->
          (* FIXME GRGR fail if parameter changed! *)
          (* failwith "Changing sandbox parameter is not yet implemented" *)
          return ctxt
