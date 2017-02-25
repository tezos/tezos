(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let version_key = ["version"]

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_value = "alpha"

(* This is the genesis protocol: initialise the state *)
let initialize ~from_genesis (ctxt:Context.t) =
  Context.set ctxt version_key (MBytes.of_string version_value) >>= fun ctxt ->
  Storage.prepare ctxt >>=? fun store ->
  Storage.get_genesis_time store >>= fun time ->
  Storage.Current_timestamp.init_set store time >>=? fun store ->
  begin
    if from_genesis then
      Lwt.return store
    else
      Fitness_storage.init store
  end >>= fun store ->
  Level_storage.init store >>=? fun store ->
  Roll_storage.init store >>=? fun store ->
  Nonce_storage.init store >>=? fun store ->
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
  | Incompatiple_protocol_version
  | Unimplemented_sandbox_migration

let may_initialize ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      (* This is the genesis protocol: The only acceptable preceding
         version is an empty context *)
      initialize ~from_genesis:false ctxt
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value)
      then Storage.prepare ctxt
      else if Compare.String.(s = "genesis") then
        initialize ~from_genesis:true ctxt
      else fail Incompatiple_protocol_version

let configure_sandbox ctxt json =
  let json =
    match json with
    | None -> `O []
    | Some json -> json in
  Context.get ctxt version_key >>= function
  | None ->
      Storage.set_sandboxed ctxt json >>= fun ctxt ->
      initialize ~from_genesis:false ctxt >>=? fun ctxt ->
      return (Storage.recover ctxt)
  | Some _ ->
      Storage.get_sandboxed ctxt >>=? function
      | None ->
          fail Unimplemented_sandbox_migration
      | Some _ ->
          (* FIXME GRGR fail if parameter changed! *)
          (* failwith "Changing sandbox parameter is not yet implemented" *)
          return ctxt
