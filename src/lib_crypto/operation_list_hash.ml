(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Blake2B.Make_merkle_tree (Base58) (struct
    let name = "Operation_list_hash"
    let title = "A list of operations"
    let b58check_prefix = Base58.Prefix.operation_list_hash
    let size = None
  end) (Operation_hash)

let () =
  Base58.check_encoded_prefix b58check_encoding "Lo" 52
