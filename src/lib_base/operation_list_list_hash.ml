(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Blake2B.Make_merkle_tree (Tezos_crypto.Base58) (struct
    let name = "Operation_list_list_hash"
    let title = "A list of list of operations"
    let b58check_prefix = Tezos_crypto.Base58.Prefix.operation_list_list_hash
    let size = None
  end) (Operation_list_hash)

let () =
  Tezos_crypto.Base58.check_encoded_prefix b58check_encoding "LLo" 53 ;
