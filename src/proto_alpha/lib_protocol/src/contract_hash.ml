(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* 20 *)
let contract_hash = "\003\099\029" (* TZ(36) *)

include Blake2B.Make(Base58)(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let b58check_prefix = contract_hash
    let size = Some 20
  end)

let () =
  Base58.check_encoded_prefix b58check_encoding "TZ1" 36
