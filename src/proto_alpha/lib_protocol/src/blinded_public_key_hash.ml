(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module H = Blake2B.Make(Base58)(struct
    let name = "Blinded public key hash"
    let title = "A blinded public key hash"
    let b58check_prefix = "\001\002\049\223"
    let size = Some Ed25519.Public_key_hash.size
  end)

include H

let () =
  Base58.check_encoded_prefix b58check_encoding "btz1" 37

let of_ed25519_pkh activation_code pkh =
  hash_bytes ~key:activation_code [ Ed25519.Public_key_hash.to_bytes pkh ]

type activation_code = MBytes.t

let activation_code_size = Ed25519.Public_key_hash.size
let activation_code_encoding = Data_encoding.Fixed.bytes activation_code_size

let activation_code_of_hex h =
  if Compare.Int.(String.length h <> activation_code_size * 2) then
    invalid_arg "Blinded_public_key_hash.activation_code_of_hex" ;
  MBytes.of_hex (`Hex h)

module Index = H
