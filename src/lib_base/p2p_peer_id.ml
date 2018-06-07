(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Crypto_box.Public_key_hash

let rpc_arg =
  RPC_arg.like
    rpc_arg
    ~descr:"A cryptographic node identity (Base58Check-encoded)"
    "peer_id"

