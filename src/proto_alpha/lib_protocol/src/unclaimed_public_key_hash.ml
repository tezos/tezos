(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = MBytes.t

let size = Ed25519.Public_key_hash.size / 2

let of_ed25519_pkh pkh =
  MBytes.sub (Ed25519.Public_key_hash.to_bytes pkh) 0 size

let encoding = Data_encoding.Fixed.bytes size

let of_hex h =
  if Compare.Int.(String.length h <> size * 2) then
    invalid_arg "Blinded_public_key_hash.of_hex" ;
  MBytes.of_hex (`Hex h)

module Index = struct

  type t = MBytes.t

  let path_length = 2

  let to_path half_public_key_hash l =
    let `Hex h = MBytes.to_hex half_public_key_hash in
    String.sub h 0 2 :: String.sub h 2 (size - 2) :: l

  let of_path = function
    | [ h1 ; h2 ] -> Some (MBytes.of_hex (`Hex (h1 ^ h2)))
    | _ -> None

end
