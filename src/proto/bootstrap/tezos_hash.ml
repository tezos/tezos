(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Prefix = struct
  let make x =
    assert (Compare.String.(Base48.Prefix.protocol_prefix = "\015")) ;
    String.make 1 (char_of_int ((x lsl 4) lor 15))
  let public_key_hash = make 0
  let contract_hash = make 1
  let nonce_hash = make 2
  let script_expr_hash = make 3
  let random_state_hash = make 15 (* never used... *)
end

module State_hash = Hash.Make_SHA256(Base48)(struct
    let name = "random"
    let title = "A random generation state"
    let b48check_prefix = Prefix.random_state_hash
  end)
module State_hash_set = Hash_set(State_hash)
module State_hash_map = Hash_map(State_hash)

module Nonce_hash = Hash.Make_SHA256(Base48)(struct
    let name = "cycle_nonce"
    let title = "A nonce hash"
    let b48check_prefix = Prefix.nonce_hash
  end)
module Nonce_hash_set = Hash_set(Nonce_hash)
module Nonce_hash_map = Hash_map(Nonce_hash)

module Script_expr_hash = Hash.Make_SHA256(Base48)(struct
    let name = "script_expr"
    let title = "A script expression ID"
    let b48check_prefix = Prefix.script_expr_hash
  end)
module Script_expr_hash_set = Hash_set(Script_expr_hash)
module Script_expr_hash_map = Hash_map(Script_expr_hash)

module Contract_hash = Hash.Make_SHA256(Base48)(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let b48check_prefix = Prefix.contract_hash
  end)
module Contract_hash_set = Hash_set(Contract_hash)
module Contract_hash_map = Hash_map(Contract_hash)
