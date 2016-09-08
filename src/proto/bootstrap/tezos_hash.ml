(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Prefix = struct
  let random_state_hash = Base48.Prefix.protocol_prefix ^ "\001"
  let nonce_hash = Base48.Prefix.protocol_prefix ^ "\002"
  let script_expr_hash = Base48.Prefix.protocol_prefix ^ "\003"
  let proposition_hash = Base48.Prefix.protocol_prefix ^ "\004"
  let contract_hash = Base48.Prefix.protocol_prefix ^ "\005"
end

module State_hash = Hash.Make_SHA256(struct
    let name = "random"
    let title = "A random generation state"
    let prefix = Some Prefix.random_state_hash
  end)
module State_hash_set = Hash_set(State_hash)
module State_hash_map = Hash_map(State_hash)

module Nonce_hash = Hash.Make_SHA256(struct
    let name = "cycle_nonce"
    let title = "A nonce hash"
    let prefix = Some Prefix.nonce_hash
  end)
module Nonce_hash_set = Hash_set(Nonce_hash)
module Nonce_hash_map = Hash_map(Nonce_hash)

module Script_expr_hash = Hash.Make_SHA256(struct
    let name = "script_expr"
    let title = "A script expression ID"
    let prefix = Some Prefix.script_expr_hash
  end)
module Script_expr_hash_set = Hash_set(Script_expr_hash)
module Script_expr_hash_map = Hash_map(Script_expr_hash)

module Contract_hash = Hash.Make_SHA256(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let prefix = Some Prefix.contract_hash
  end)
module Contract_hash_set = Hash_set(Contract_hash)
module Contract_hash_map = Hash_map(Contract_hash)

