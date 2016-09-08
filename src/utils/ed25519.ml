(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos - Ed25519 cryptography (simple interface to Sodium) *)

(*-- Signature ---------------------------------------------------------------*)

type secret_key = Sodium.Sign.secret_key
type public_key = Sodium.Sign.public_key
type signature = MBytes.t

let sign key msg =
  Sodium.Sign.Bigbytes.(of_signature @@ sign_detached key msg)

let check_signature public_key signature msg =
  try Sodium.Sign.Bigbytes.(verify public_key (to_signature signature) msg) ; true
  with _ -> false

let append_signature key msg =
  MBytes.concat msg (sign key msg)

(*-- Hashed public keys for user ID ------------------------------------------*)

module Public_key_hash = Hash.Make_SHA256(struct
    let name = "Ed25519.Public_key_hash"
    let title = "An Ed25519 public key ID"
    let prefix = Some Base48.Prefix.public_key_hash
  end)

type public_key_hash = Public_key_hash.t

let hash v =
  Public_key_hash.hash_bytes
    [ Sodium.Sign.Bigbytes.of_public_key v ]

let hash_path = Public_key_hash.to_path
let hash_hex = Public_key_hash.to_hex
let equal_hash = Public_key_hash.equal
let compare_hash = Public_key_hash.compare

let generate_key () =
  let secret, pub = Sodium.Sign.random_keypair () in
  (hash pub, pub, secret)

(*-- JSON Serializers --------------------------------------------------------*)

type Base48.data +=
  | Public_key of public_key
  | Secret_key of secret_key
  | Signature of signature

let () =
  Base48.register
    ~prefix:Base48.Prefix.public_key
    ~read:(function Public_key x -> Some (Bytes.to_string (Sodium.Sign.Bytes.of_public_key x)) | _ -> None)
    ~build:(fun x -> Public_key (Sodium.Sign.Bytes.to_public_key (Bytes.of_string x)))

let () =
  Base48.register
    ~prefix:Base48.Prefix.secret_key
    ~read:(function Secret_key x -> Some (Bytes.to_string (Sodium.Sign.Bytes.of_secret_key x)) | _ -> None)
    ~build:(fun x -> Secret_key (Sodium.Sign.Bytes.to_secret_key (Bytes.of_string x)))

let () =
  Base48.register
    ~prefix:Base48.Prefix.signature
    ~read:(function Signature x -> Some (MBytes.to_string x) | _ -> None)
    ~build:(fun x -> Signature (MBytes.of_string x))

let public_key_hash_encoding =
  Public_key_hash.encoding

let public_key_encoding =
  let open Data_encoding in
  splitted
    ~json:
      (describe
         ~title: "An Ed25519 public key (Base48Check encoded)" @@
       conv
         (fun s -> Base48.encode (Public_key s))
         (fun s ->
            match Base48.decode s with
            | Public_key x -> x
            | _ -> Data_encoding.Json.cannot_destruct
                     "Ed25519 public key: unexpected prefix.")
         string)
    ~binary:
      (conv
         Sodium.Sign.Bigbytes.of_public_key
         Sodium.Sign.Bigbytes.to_public_key
         bytes)

let secret_key_encoding =
  let open Data_encoding in
  splitted
    ~json:
      (describe
         ~title: "An Ed25519 secret key (Base48Check encoded)" @@
       conv
         (fun s -> Base48.encode (Secret_key s))
         (fun s ->
            match Base48.decode s with
            | Secret_key x -> x
            | _ -> Data_encoding.Json.cannot_destruct
                     "Ed25519 secret key: unexpected prefix.")
         string)
    ~binary:
      (conv
         Sodium.Sign.Bigbytes.of_secret_key
         Sodium.Sign.Bigbytes.to_secret_key
         bytes)

let signature_encoding =
  let open Data_encoding in
  splitted
    ~json:
      (describe
         ~title: "An Ed25519 signature (Base48Check encoded)" @@
       conv
         (fun s -> Base48.encode (Signature s))
         (fun s ->
            match Base48.decode s with
            | Signature x -> x
            | _ ->
                Data_encoding.Json.cannot_destruct
                  "Ed25519 signature: unexpected prefix.")
         string)
    ~binary: (Fixed.bytes 64)
