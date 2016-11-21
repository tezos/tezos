(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = struct

  type secret_key = Sodium.Sign.secret_key
  type public_key = Sodium.Sign.public_key
  type signature = MBytes.t

  let sign key msg =
    Sodium.Sign.Bigbytes.(of_signature @@ sign_detached key msg)

  let check_signature public_key signature msg =
    try
      Sodium.Sign.Bigbytes.(verify public_key (to_signature signature) msg) ;
      true
    with _ -> false

  let append_signature key msg =
    MBytes.concat msg (sign key msg)

  module Public_key_hash = Hash.Make_SHA256(Base48)(struct
      let name = "Ed25519.Public_key_hash"
      let title = "An Ed25519 public key ID"
      let b48check_prefix = Base48.Prefix.ed25519_public_key_hash
    end)

  let hash v =
    Public_key_hash.hash_bytes
      [ Sodium.Sign.Bigbytes.of_public_key v ]

  let generate_key () =
    let secret, pub = Sodium.Sign.random_keypair () in
    (hash pub, pub, secret)

  type Base48.data +=
    | Public_key of public_key
    | Secret_key of secret_key
    | Signature of signature

  let b48check_public_key_encoding =
    Base48.register_encoding
      ~prefix: Base48.Prefix.ed25519_public_key
      ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_public_key x))
      ~of_raw:(fun x ->
          try Some (Sodium.Sign.Bytes.to_public_key (Bytes.of_string x))
          with _ -> None)
      ~wrap:(fun x -> Public_key x)

  let b48check_secret_key_encoding =
    Base48.register_encoding
      ~prefix: Base48.Prefix.ed25519_secret_key
      ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_secret_key x))
      ~of_raw:(fun x ->
          try Some (Sodium.Sign.Bytes.to_secret_key (Bytes.of_string x))
          with _ -> None)
      ~wrap:(fun x -> Secret_key x)

  let b48check_signature_encoding =
    Base48.register_encoding
      ~prefix: Base48.Prefix.ed25519_signature
      ~to_raw:MBytes.to_string
      ~of_raw:(fun s -> Some (MBytes.of_string s))
      ~wrap:(fun x -> Signature x)

  let public_key_encoding =
    let open Data_encoding in
    splitted
      ~json:
        (describe
           ~title: "An Ed25519 public key (Base48Check encoded)" @@
         conv
           (fun s -> Base48.simple_encode b48check_public_key_encoding s)
           (fun s ->
              match Base48.simple_decode b48check_public_key_encoding s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
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
           (fun s -> Base48.simple_encode b48check_secret_key_encoding s)
           (fun s ->
              match Base48.simple_decode b48check_secret_key_encoding s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
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
           (fun s -> Base48.simple_encode b48check_signature_encoding s)
           (fun s ->
              match Base48.simple_decode b48check_signature_encoding s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
                          "Ed25519 signature: unexpected prefix.")
           string)
      ~binary: (Fixed.bytes 64)

end

module Make(Param : sig val name: string end)() = struct

  include Pervasives
  module Pervasives = Pervasives
  module Compare = Compare
  module Array = Array
  module List = List
  module Bytes = struct
    include Bytes
    include EndianBytes.BigEndian
    module LE = EndianBytes.LittleEndian
  end
  module String = struct
    include String
    include EndianString.BigEndian
    module LE = EndianString.LittleEndian
  end
  module Set = Set
  module Map = Map
  module Int32 = Int32
  module Int64 = Int64
  module Nativeint = Nativeint
  module Buffer = Buffer
  module Format = Format
  module Hex_encode = Hex_encode
  module Lwt_sequence = Lwt_sequence
  module Lwt = Lwt
  module Lwt_list = Lwt_list
  module MBytes = MBytes
  module Uri = Uri
  module Data_encoding = Data_encoding
  module Time = Time
  module Ed25519 = Ed25519
  module Hash = Hash
  module Persist = Persist
  module RPC = RPC
  module Fitness = Fitness
  module Updater = Updater
  module Error_monad = struct
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
  end
  module Logging = Logging.Make(Param)
  module Base48 = struct
    include Base48
    include Make(struct type context = Context.t end)
  end
  module Context = struct
    include Context
    let register_resolver = Base48.register_resolver
    let complete = Base48.complete
  end

  module type PACKED_PROTOCOL = sig
    val hash : Protocol_hash.t
    include Updater.PROTOCOL
    val error_encoding : error Data_encoding.t
    val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
    val pp : Format.formatter -> error -> unit
    val complete_b48prefix :
      ?alphabet:string -> Context.t -> string -> string list Lwt.t
  end

end
