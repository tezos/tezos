(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_crypto.Crypto_box

let public_key_encoding =
  let open Data_encoding in
  conv
    public_key_to_bigarray
    public_key_of_bigarray
    (Fixed.bytes public_key_size)

let secret_key_encoding =
  let open Data_encoding in
  conv
    secret_key_to_bigarray
    secret_key_of_bigarray
    (Fixed.bytes secret_key_size)

let nonce_encoding =
  let open Data_encoding in
  conv
    nonce_to_bigarray
    nonce_of_bigarray
    (Fixed.bytes nonce_size)

