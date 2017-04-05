(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = struct

  module Public_key_hash = Hash.Make_Blake2B(Base58)(struct
      let name = "Ed25519.Public_key_hash"
      let title = "An Ed25519 public key ID"
      let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
      let size = Some 20
    end)

  let () =
    Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36

  module Public_key = struct

    type t = Sodium.Sign.public_key
    let compare = Sodium.Sign.compare_public_keys
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

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_public_key
        ~length:Sodium.Sign.public_key_size
        ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_public_key x))
        ~of_raw:(fun x ->
            try Some (Sodium.Sign.Bytes.to_public_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Public_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 public key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = Sodium.Sign.Bytes.to_public_key s

    let param ?(name="ed25519-public") ?(desc="Ed25519 public key (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edpk" 54

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 public key (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 public key: unexpected prefix.")
             string)
        ~binary:
          (conv
             Sodium.Sign.Bigbytes.of_public_key
             Sodium.Sign.Bigbytes.to_public_key
             bytes)

    let hash v =
      Public_key_hash.hash_bytes
        [ Sodium.Sign.Bigbytes.of_public_key v ]

  end

  module Secret_key = struct

    type t = Sodium.Sign.secret_key

    type Base58.data +=
      | Secret_key of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_secret_key
        ~length:Sodium.Sign.secret_key_size
        ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_secret_key x))
        ~of_raw:(fun x ->
            try Some (Sodium.Sign.Bytes.to_secret_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Secret_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 secret key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = Sodium.Sign.Bytes.to_secret_key s

    let param ?(name="ed25519-secret") ?(desc="Ed25519 secret key (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edsk" 98

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 secret key (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 secret key: unexpected prefix.")
             string)
        ~binary:
          (conv
             Sodium.Sign.Bigbytes.of_secret_key
             Sodium.Sign.Bigbytes.to_secret_key
             bytes)

  end

  let sign key msg =
    Sodium.Sign.Bigbytes.(of_signature @@ sign_detached key msg)

  module Signature = struct

    type t = MBytes.t

    type Base58.data +=
      | Signature of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_signature
        ~length:Sodium.Sign.signature_size
        ~to_raw:MBytes.to_string
        ~of_raw:(fun s -> Some (MBytes.of_string s))
        ~wrap:(fun x -> Signature x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 signature)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = MBytes.of_string (Bytes.to_string s)

    let param ?(name="signature") ?(desc="Signature (b58check-encoded)") t =
      Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

    let () =
      Base58.check_encoded_prefix b58check_encoding "edsig" 99

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (describe
             ~title: "An Ed25519 signature (Base58Check encoded)" @@
           conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 signature: unexpected prefix.")
             string)
        ~binary: (Fixed.bytes 64)

    let check public_key signature msg =
      try
        Sodium.Sign.Bigbytes.(verify public_key (to_signature signature) msg) ;
        true
      with _ -> false

    let append key msg =
      MBytes.concat msg (sign key msg)

  end

  let generate_key () =
    let secret, pub = Sodium.Sign.random_keypair () in
    (Public_key.hash pub, pub, secret)

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
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
    include Make(struct type context = Context.t end)
    let decode s = decode s
  end
  module Context = struct
    include Context
    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end

  module type PACKED_PROTOCOL = sig
    val hash : Protocol_hash.t
    include Updater.PROTOCOL
    val error_encoding : error Data_encoding.t
    val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
    val pp : Format.formatter -> error -> unit
    val complete_b58prefix : Context.t -> string -> string list Lwt.t
  end

end
