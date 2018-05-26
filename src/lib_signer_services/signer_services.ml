(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let sign =
  RPC_service.post_service
    ~description: "Sign a piece of data with a given remote key"
    ~query: RPC_query.empty
    ~input: Data_encoding.bytes
    ~output:  Data_encoding.(obj1 (req "signature" Signature.encoding))
    RPC_path.(root /: Signature.Public_key_hash.rpc_arg)

let public_key =
  RPC_service.get_service
    ~description: "Retrieve the public key of a given remote key"
    ~query: RPC_query.empty
    ~output: Data_encoding.(obj1 (req "public_key" Signature.Public_key.encoding))
    RPC_path.(root /: Signature.Public_key_hash.rpc_arg)
