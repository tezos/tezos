(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = MBytes.t

type secret = MBytes.t

let size = Ed25519.Public_key_hash.size
let secret_size = Ed25519.Public_key_hash.size

let encoding = Data_encoding.Fixed.bytes size
let secret_encoding = Data_encoding.Fixed.bytes secret_size

let of_ed25519_pkh secret pkh =
  Ed25519.Public_key_hash.to_bytes @@
  Ed25519.Public_key_hash.hash_bytes
    ~key:secret
    [ Ed25519.Public_key_hash.to_bytes pkh ]

let compare = MBytes.compare
let (=) = MBytes.(=)

let of_hex h =
  if Compare.Int.(String.length h <> size * 2) then
    invalid_arg "Blinded_public_key_hash.of_hex" ;
  MBytes.of_hex (`Hex h)

let secret_of_hex h =
  if Compare.Int.(String.length h <> secret_size * 2) then
    invalid_arg "Blinded_public_key_hash.secret_of_hex" ;
  MBytes.of_hex (`Hex h)
