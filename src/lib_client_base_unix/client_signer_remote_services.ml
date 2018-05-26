(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

let sign = RPC_service.post_service
    ~description: "Sign a piece of data with a given remote key"
    ~query: RPC_query.empty
    ~input: Sign.Request.encoding
    ~output: Sign.Response.encoding
    RPC_path.(root / "sign")

let public_key = RPC_service.post_service
    ~description: "Retrieve the public key of a given remote key"
    ~query: RPC_query.empty
    ~input: Public_key.Request.encoding
    ~output: Public_key.Response.encoding
    RPC_path.(root / "public_key")
