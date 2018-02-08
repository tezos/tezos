(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Public_key_hash = Blake2B.Make(Base58)(struct
    let name = "Ed25519.Public_key_hash"
    let title = "An Ed25519 public key ID"
    let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
    let size = Some 20
  end)

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36

open Tweetnacl

module Public_key = struct

  type t = Sign.public Sign.key
  let compare a b = Cstruct.compare (Sign.to_cstruct a) (Sign.to_cstruct b)
  let (=) xs ys = compare xs ys = 0
  let (<>) xs ys = compare xs ys <> 0
  let (<) xs ys = compare xs ys < 0
  let (<=) xs ys = compare xs ys <= 0
  let (>=) xs ys = compare xs ys >= 0
  let (>) xs ys = compare xs ys > 0
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y

  type Base58.data +=
    | Public_key of t

  let to_string s = Cstruct.to_string (Sign.to_cstruct s)
  let of_string s = Sign.pk_of_cstruct (Cstruct.of_string s)

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_public_key
      ~length:Sign.pkbytes
      ~to_raw:to_string
      ~of_raw:of_string
      ~wrap:(fun x -> Public_key x)

  let of_b58check_opt s = Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Pervasives.failwith
                (Printf.sprintf "%s is not an ed25519 public key" s)
  let to_b58check s = Base58.simple_encode b58check_encoding s
  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

  let of_hex s = of_string (Hex.to_string s)
  let of_hex_exn s =
    match of_string (Hex.to_string s) with
    | Some x -> x
    | None -> invalid_arg "Public_key.of_hex_exn"
  let to_hex s = Hex.of_string (to_string s)

  let of_bytes_opt s =
    Sign.pk_of_cstruct (Cstruct.of_bigarray s)

  let of_bytes_exn s =
    match of_bytes_opt s with
    | None ->
        Pervasives.invalid_arg "Ed25519.Public_key.of_bytes_exn: argument is not a serialized public key"
    | Some pk -> pk
  let size = Sign.pkbytes

  let to_bytes pk = Cstruct.to_bigarray (Sign.to_cstruct pk)

  let () =
    Base58.check_encoded_prefix b58check_encoding "edpk" 54

  let hash v =
    Public_key_hash.hash_bytes
      [ Cstruct.to_bigarray (Sign.to_cstruct v) ]

end

module Secret_key = struct

  type t = Sign.secret Sign.key

  let to_public_key = Sign.public

  type Base58.data +=
    | Secret_key of t

  let seed_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_seed
      ~length:Sign.seedbytes
      ~to_raw:(fun sk -> Cstruct.to_string (Sign.seed sk))
      ~of_raw:(fun buf ->
          let seed = Cstruct.of_string buf in
          match Sign.keypair ~seed () with
          | exception _ -> None
          | _pk, sk -> Some sk)
      ~wrap:(fun sk -> Secret_key sk)

  let secret_key_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_secret_key
      ~length:Sign.skbytes
      ~to_raw:(fun sk -> Cstruct.to_string (Sign.to_cstruct sk))
      ~of_raw:(fun buf -> Sign.sk_of_cstruct (Cstruct.of_string buf))
      ~wrap:(fun x -> Secret_key x)

  let of_b58check_opt s =
    match Base58.simple_decode seed_encoding s with
    | Some x -> Some x
    | None -> Base58.simple_decode secret_key_encoding s

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Pervasives.failwith
                (Printf.sprintf "%s is not an ed25519 secret key" s)
  let to_b58check s = Base58.simple_encode seed_encoding s
  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

  let of_bytes_opt s =
    let s = Cstruct.of_bigarray s in
    match Cstruct.len s with
    | 32 -> let _pk, sk = Sign.keypair ~seed:s () in Some sk
    | 64 -> Sign.sk_of_cstruct s
    | _ -> None

  let of_bytes_exn s =
    match of_bytes_opt s with
    | None ->
        Pervasives.invalid_arg "Ed25519.Secret_key.of_bytes_exn: argument is not a serialized seed"
    | Some sk -> sk

  let to_bytes x = Cstruct.to_bigarray (Sign.seed x)
  let size = Sign.seedbytes

  let () =
    Base58.check_encoded_prefix seed_encoding "edsk" 54 ;
    Base58.check_encoded_prefix secret_key_encoding "edsk" 98

end

let sign key msg =
  Cstruct.(to_bigarray (Sign.detached ~key (of_bigarray msg)))

module Signature = struct

  type t = MBytes.t

  type Base58.data +=
    | Signature of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_signature
      ~length:Sign.bytes
      ~to_raw:MBytes.to_string
      ~of_raw:(fun s -> Some (MBytes.of_string s))
      ~wrap:(fun x -> Signature x)

  let of_b58check_opt s = Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Pervasives.failwith
                (Printf.sprintf "%s is not an ed25519 signature" s)
  let to_b58check s = Base58.simple_encode b58check_encoding s
  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

  let of_bytes_opt s =
    if MBytes.length s = Sign.bytes then Some s else None

  let of_bytes_exn s =
    match of_bytes_opt s with
    | None ->
        Pervasives.invalid_arg "Ed25519.Signature.of_bytes_exn: argument is not a serialized signature"
    | Some signature -> signature

  let to_bytes x = x
  let size = Sign.bytes

  let () =
    Base58.check_encoded_prefix b58check_encoding "edsig" 99

  let check public_key signature msg =
    Sign.verify_detached ~key:public_key
      ~signature:(Cstruct.of_bigarray signature)
      (Cstruct.of_bigarray msg)

  let append key msg =
    MBytes.concat msg (sign key msg)

  let concat msg signature =
    MBytes.concat msg signature

end

module Seed = struct

  type t = Cstruct.t

  let generate () = Rand.gen 32
  let extract = Sign.seed
end

let generate_key () =
  let pk, sk = Sign.keypair () in
  (Public_key.hash pk, pk, sk)

let generate_seeded_key seed =
  let pk, sk = Sign.keypair ~seed () in
  (Public_key.hash pk, pk, sk)
