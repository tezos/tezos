(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* 32 *)
let nonce_hash = "\069\220\169" (* nce(53) *)

include Blake2B.Make(Base58)(struct
    let name = "cycle_nonce"
    let title = "A nonce hash"
    let b58check_prefix = nonce_hash
    let size = None
  end)

let () =
  Base58.check_encoded_prefix b58check_encoding "nce" 53
