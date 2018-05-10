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

let zero = MBytes.of_string (String.make size '\000')

let to_b58check s =
  Ed25519.Public_key_hash.to_b58check
    (Ed25519.Public_key_hash.of_bytes_exn
       (MBytes.concat "" [s; zero]))

let of_b58check_exn s =
  let pkh = Ed25519.Public_key_hash.of_b58check_exn s in
  let padding = MBytes.sub (Ed25519.Public_key_hash.to_bytes pkh) size size in
  if MBytes.(<>) zero padding then
    failwith "invalid Base58Check-encoded unclaimed public-key hash" ;
  of_ed25519_pkh pkh

let encoding =
  let open Data_encoding in
  splitted
    ~binary:(Fixed.bytes size)
    ~json:
      (conv
         to_b58check
         of_b58check_exn
         string)

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
