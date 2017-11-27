(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Prefix = struct

  (* 20 *)
  let contract_hash = "\003\099\029" (* TZ(36) *)

  (* 32 *)
  let nonce_hash = "\069\220\169" (* nce(53) *)
  let script_expr_hash = "\013\044\064\027" (* expr(54) *)
  let random_state_hash = "\076\064\204" (* rng(53): never used... *)

end

module State_hash = Blake2B.Make(Base58)(struct
    let name = "random"
    let title = "A random generation state"
    let b58check_prefix = Prefix.random_state_hash
    let size = None
  end)

module Nonce_hash = Blake2B.Make(Base58)(struct
    let name = "cycle_nonce"
    let title = "A nonce hash"
    let b58check_prefix = Prefix.nonce_hash
    let size = None
  end)

module Script_expr_hash = Blake2B.Make(Base58)(struct
    let name = "script_expr"
    let title = "A script expression ID"
    let b58check_prefix = Prefix.script_expr_hash
    let size = None
  end)

module Contract_hash = Blake2B.Make(Base58)(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let b58check_prefix = Prefix.contract_hash
    let size = Some 20
  end)

let () =
  Base58.check_encoded_prefix Contract_hash.b58check_encoding "TZ1" 36 ;
  Base58.check_encoded_prefix Script_expr_hash.b58check_encoding "expr" 54 ;
  Base58.check_encoded_prefix Nonce_hash.b58check_encoding "nce" 53 ;
  Base58.check_encoded_prefix State_hash.b58check_encoding "rng" 53
