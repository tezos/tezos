(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Blake2B.Make (Base58) (struct
    let name = "block_hash"
    let title = "A block identifier"
    let b58check_prefix = Base58.Prefix.block_hash
    let size = None
  end)

let () =
  Base58.check_encoded_prefix b58check_encoding "B" 51
