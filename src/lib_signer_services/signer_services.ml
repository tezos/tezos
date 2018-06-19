(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let sign =
  let query =
    let open RPC_query in
    query (fun signature -> signature)
    |+ opt_field
      ~descr: "Must be provided if the signer requires \
               authentication. In this case, it must be the signature \
               of the public key hash and message concatenated, by one \
               of the keys authorized by the signer."
      "authentication" Signature.rpc_arg (fun signature -> signature)
    |> seal in
  RPC_service.post_service
    ~description: "Sign a piece of data with a given remote key"
    ~query
    ~input: Data_encoding.bytes
    ~output: Data_encoding.(obj1 (req "signature" Signature.encoding))
    RPC_path.(root / "keys" /: Signature.Public_key_hash.rpc_arg)

let public_key =
  RPC_service.get_service
    ~description: "Retrieve the public key of a given remote key"
    ~query: RPC_query.empty
    ~output: Data_encoding.(obj1 (req "public_key" Signature.Public_key.encoding))
    RPC_path.(root / "keys" /: Signature.Public_key_hash.rpc_arg)

let authorized_keys =
  RPC_service.get_service
    ~description: "Retrieve the public keys that can be used to \
                   authenticate signing commands.\n\
                   If the empty object is returned, the signer has \
                   been set to accept unsigned commands."
    ~query: RPC_query.empty
    ~output: Data_encoding.(obj1 (opt "authorized_keys" (list Signature.Public_key_hash.encoding)))
    RPC_path.(root / "authorized_keys")
