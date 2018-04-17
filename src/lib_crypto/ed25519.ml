(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

module Public_key_hash = Blake2B.Make(Base58)(struct
    let name = "Ed25519.Public_key_hash"
    let title = "An Ed25519 public key hash"
    let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
    let size = Some 20
  end)

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36

open Tweetnacl

module Public_key = struct

  type t = Sign.public Sign.key

  let name = "Ed25519.Public_key"
  let title = "Ed25519 public key"

  let to_string s = MBytes.to_string (Sign.to_bytes s)
  let of_string_opt s = Sign.pk_of_bytes (MBytes.of_string s)

  let to_bytes = Sign.to_bytes
  let of_bytes_opt = Sign.pk_of_bytes

  let size = Sign.pkbytes

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let () =
    Base58.check_encoded_prefix b58check_encoding "edpk" 54

  let hash v =
    Public_key_hash.hash_bytes [ Sign.to_bytes v ]

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (Sign.to_bytes a) (Sign.to_bytes b)
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeB58(struct
      type nonrec t = t
      let title = title
      let name = name
      let b58check_encoding = b58check_encoding
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

module Secret_key = struct

  type t = Sign.secret Sign.key

  let name = "Ed25519.Secret_key"
  let title = "An Ed25519 secret key"

  let size = Sign.seedbytes

  let to_bytes = Sign.seed
  let of_bytes_opt s =
    match MBytes.length s with
    | 32 -> let _pk, sk = Sign.keypair ~seed:s () in Some sk
    | 64 -> Sign.sk_of_bytes s
    | _ -> None

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let to_public_key = Sign.public

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_seed
      ~length: size
      ~to_raw: (fun sk -> MBytes.to_string (Sign.seed sk))
      ~of_raw: (fun buf ->
          let seed = MBytes.of_string buf in
          match Sign.keypair ~seed () with
          | exception _ -> None
          | _pk, sk -> Some sk)
      ~wrap: (fun sk -> Data sk)

  let secret_key_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_secret_key
      ~length: Sign.skbytes
      ~to_raw: (fun sk -> MBytes.to_string (Sign.to_bytes sk))
      ~of_raw: (fun buf -> Sign.sk_of_bytes (MBytes.of_string buf))
      ~wrap: (fun x -> Data x)

  let of_b58check_opt s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> Some x
    | None -> Base58.simple_decode secret_key_encoding s
  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        generic_error
          "Failed to read a b58check_encoding data (%s): %S"
          name s

  let to_b58check s = Base58.simple_encode b58check_encoding s
  let to_short_b58check s =
    String.sub
      (to_b58check s) 0
      (10 + String.length (Base58.prefix b58check_encoding))

  let () =
    Base58.check_encoded_prefix b58check_encoding "edsk" 54 ;
    Base58.check_encoded_prefix secret_key_encoding "edsk" 98

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (Sign.to_bytes a) (Sign.to_bytes b)
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

type t = MBytes.t

let name = "Ed25519"
let title = "An Ed25519 signature"

let size = Sign.bytes

let of_bytes_opt s =
  if MBytes.length s = size then Some s else None
let to_bytes x = x

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

type Base58.data +=
  | Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.ed25519_signature
    ~length: size
    ~to_raw: MBytes.to_string
    ~of_raw: (fun s -> Some (MBytes.of_string s))
    ~wrap: (fun x -> Data x)

let () =
  Base58.check_encoded_prefix b58check_encoding "edsig" 99

include Compare.Make(struct
    type nonrec t = t
    let compare = MBytes.compare
  end)

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Helpers.MakeB58(struct
    type nonrec t = t
    let title = title
    let name = name
    let b58check_encoding = b58check_encoding
  end)

include Helpers.MakeEncoder(struct
    type nonrec t = t
    let name = name
    let title = title
    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)
    let of_b58check = of_b58check
    let of_b58check_opt = of_b58check_opt
    let of_b58check_exn = of_b58check_exn
    let to_b58check = to_b58check
    let to_short_b58check = to_short_b58check
  end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let zero = MBytes.init size '\000'

let sign key msg = Sign.detached ~key msg

let check public_key signature msg =
  Sign.verify_detached ~key:public_key ~signature msg

let append key msg =
  MBytes.concat msg (sign key msg)

let concat msg signature =
  MBytes.concat msg signature

module Seed = struct

  type t = Bigstring.t

  let generate () = Rand.gen 32
  let extract = Sign.seed

end

let generate_seeded_key seed =
  let pk, sk = Sign.keypair ~seed () in
  (Public_key.hash pk, pk, sk)

let generate_key () =
  let seed = Seed.generate () in
  generate_seeded_key seed

include Compare.Make(struct
    type nonrec t = t
    let compare = MBytes.compare
  end)

