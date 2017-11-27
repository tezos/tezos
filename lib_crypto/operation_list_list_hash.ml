(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Blake2B.Make_merkle_tree (Base58) (struct
    let name = "Operation_list_list_hash"
    let title = "A list of list of operations"
    let b58check_prefix = Base58.Prefix.operation_list_list_hash
    let size = None
  end) (Operation_list_hash)

let () =
  Base58.check_encoded_prefix b58check_encoding "LLo" 53 ;
