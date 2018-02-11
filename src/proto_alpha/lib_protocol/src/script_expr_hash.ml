(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let script_expr_hash = "\013\044\064\027" (* expr(54) *)

include Blake2B.Make(Base58)(struct
    let name = "script_expr"
    let title = "A script expression ID"
    let b58check_prefix = script_expr_hash
    let size = None
  end)

let () =
  Base58.check_encoded_prefix b58check_encoding "expr" 54
