(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t

module Public_key_hash = struct

  type t = public_key_hash =
    | Ed25519 of Ed25519.Public_key_hash.t
    | Secp256k1 of Secp256k1.Public_key_hash.t

  let name = "Signature.Public_key_hash"
  let title = "A Secp256k1 or Ed25519 public key hash"

  type Base58.data += Data of t (* unused *)
  let b58check_encoding = (* unused *)
    Base58.register_encoding
      ~prefix: "\255\255"
      ~length: 2
      ~to_raw: (fun _ -> assert false)
      ~of_raw: (fun _ -> assert false)
      ~wrap: (fun x -> Data x)

  let raw_encoding =
    let open Data_encoding in
    union [
      case (Tag 0) Ed25519.Public_key_hash.encoding
        (function Ed25519 x -> Some x | _ -> None)
        (function x -> Ed25519 x);
      case (Tag 1) Secp256k1.Public_key_hash.encoding
        (function Secp256k1 x -> Some x | _ -> None)
        (function x -> Secp256k1 x)
    ]

  let to_bytes s =
    Data_encoding.Binary.to_bytes raw_encoding s
  let of_bytes_opt s =
    Data_encoding.Binary.of_bytes raw_encoding s
  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let size = 1 + Ed25519.size

  let zero = Ed25519 Ed25519.Public_key_hash.zero

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  let of_b58check_opt s =
    match Base58.decode s with
    | Some Ed25519.Public_key_hash.Data pkh -> Some (Ed25519 pkh)
    | Some Secp256k1.Public_key_hash.Data pkh -> Some (Secp256k1 pkh)
    | _ -> None

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

  let to_b58check = function
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_b58check pkh

  let to_short_b58check = function
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_short_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_short_b58check pkh

  let to_path key l = match key with
    | Ed25519 h -> "ed25519" :: Ed25519.Public_key_hash.to_path h l
    | Secp256k1 h -> "secp256k1" :: Secp256k1.Public_key_hash.to_path h l

  let of_path = function
    | "ed25519" :: q ->  begin
        match Ed25519.Public_key_hash.of_path q with
        | Some pkh -> Some (Ed25519 pkh)
        | None -> None
      end
    | "secp256k1" :: q -> begin
        match Secp256k1.Public_key_hash.of_path q with
        | Some pkh -> Some (Secp256k1 pkh)
        | None -> None
      end
    | _ -> assert false (* FIXME classification des erreurs *)
  let of_path_exn = function
    | "ed25519" :: q -> Ed25519 (Ed25519.Public_key_hash.of_path_exn q)
    | "secp256k1" :: q -> Secp256k1 (Secp256k1.Public_key_hash.of_path_exn q)
    | _ -> assert false (* FIXME classification des erreurs *)

  let path_length =
    let l1 = Ed25519.Public_key_hash.path_length
    and l2 = Secp256k1.Public_key_hash.path_length in
    assert Compare.Int.(l1 = l2) ;
    1 + l1

  let prefix_path _ = assert false (* unused *)

  let hash = Hashtbl.hash

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        match (a, b) with
        | Ed25519 _   , Secp256k1 _ ->  1
        | Secp256k1 _, Ed25519 _  -> -1
        | Ed25519 x, Ed25519 y ->
            Ed25519.Public_key_hash.compare x y
        | Secp256k1 x, Secp256k1 y ->
            Secp256k1.Public_key_hash.compare x y
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding = raw_encoding
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  include Helpers.MakeIterator(struct
      type nonrec t = t
      let hash = hash
      let compare = compare
      let equal = equal
      let encoding = encoding
    end)

end

module Public_key = struct

  type t = public_key =
    | Ed25519 of Ed25519.Public_key.t
    | Secp256k1 of Secp256k1.Public_key.t

  let name = "Signature.Public_key"
  let title = "A Secp256k1 or Ed25519 public key"

  let hash pk =
    match pk with
    | Ed25519 pk -> Public_key_hash.Ed25519 (Ed25519.Public_key.hash pk)
    | Secp256k1 pk -> Public_key_hash.Secp256k1 (Secp256k1.Public_key.hash pk)

  include Compare.Make(struct
      type nonrec t = t
      let compare a b = match (a, b) with
        | (Ed25519 _   , Secp256k1 _) ->  1
        | (Secp256k1 _, Ed25519 _  ) -> -1
        | (Ed25519 x, Ed25519 y) -> Ed25519.Public_key.compare x y
        | (Secp256k1 x, Secp256k1 y) -> Secp256k1.Public_key.compare x y
    end)

  type Base58.data += Data of t (* unused *)
  let b58check_encoding = (* unused *)
    Base58.register_encoding
      ~prefix: "\255\255"
      ~length: 2
      ~to_raw: (fun _ -> assert false)
      ~of_raw: (fun _ -> assert false)
      ~wrap: (fun x -> Data x)

  let of_b58check_opt s =
    match Base58.decode s with
    | Some (Ed25519.Public_key.Data public_key) -> Some (Ed25519 public_key)
    | Some (Secp256k1.Public_key.Data public_key) -> Some (Secp256k1 public_key)
    | _ -> None

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

  let to_b58check = function
    | Ed25519 pk -> Ed25519.Public_key.to_b58check pk
    | Secp256k1 pk -> Secp256k1.Public_key.to_b58check pk

  let to_short_b58check = function
    | Ed25519 pk -> Ed25519.Public_key.to_short_b58check pk
    | Secp256k1 pk -> Secp256k1.Public_key.to_short_b58check pk

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        union [
          case (Tag 0) Ed25519.Public_key.encoding
            (function Ed25519 x -> Some x | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Public_key.encoding
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x)
        ]
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

module Secret_key = struct

  type t = secret_key =
    | Ed25519 of Ed25519.Secret_key.t
    | Secp256k1 of Secp256k1.Secret_key.t

  let name = "Signature.Secret_key"
  let title = "A Secp256k1 or Ed25519 secret key"

  let to_public_key = function
    | Ed25519 sk -> Public_key.Ed25519 (Ed25519.Secret_key.to_public_key sk)
    | Secp256k1 sk -> Public_key.Secp256k1 (Secp256k1.Secret_key.to_public_key sk)

  include Compare.Make(struct
      type nonrec t = t
      let compare a b = match (a, b) with
        | (Ed25519 _   , Secp256k1 _) ->  1
        | (Secp256k1 _, Ed25519 _  ) -> -1
        | (Ed25519 x, Ed25519 y) -> Ed25519.Secret_key.compare x y
        | (Secp256k1 x, Secp256k1 y) -> Secp256k1.Secret_key.compare x y
    end)

  type Base58.data += Data of t (* unused *)
  let b58check_encoding = (* unused *)
    Base58.register_encoding
      ~prefix: "\255\255"
      ~length: 2
      ~to_raw: (fun _ -> assert false)
      ~of_raw: (fun _ -> assert false)
      ~wrap: (fun x -> Data x)

  let of_b58check_opt b =
    match Base58.decode b with
    | Some (Ed25519.Secret_key.Data sk) -> Some (Ed25519 sk)
    | Some (Secp256k1.Secret_key.Data sk) -> Some (Secp256k1 sk)
    | _ -> None

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

  let to_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_b58check sk

  let to_short_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_short_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_short_b58check sk

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        union [
          case (Tag 0) Ed25519.Secret_key.encoding
            (function Ed25519 x -> Some x | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Secret_key.encoding
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x)
        ]
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

type t =
  | Ed25519 of Ed25519.t
  | Secp256k1 of Secp256k1.t
  | Unknown of MBytes.t

let name = "Signature"
let title = "A Secp256k1 or Ed25519 signature"

let () = assert (Ed25519.size = Secp256k1.size)
let size = Ed25519.size

let to_bytes = function
  | Unknown b -> b
  | Ed25519 b -> Ed25519.to_bytes b
  | Secp256k1 b -> Secp256k1.to_bytes b

let of_bytes_opt s =
  if MBytes.length s = size then Some (Unknown s) else None

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

type Base58.data += Data of t
let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.generic_signature
    ~length: Ed25519.size
    ~to_raw: to_string
    ~of_raw: of_string_opt
    ~wrap: (fun x -> Data x)

let () =
  Base58.check_encoded_prefix b58check_encoding "sig" 96

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Compare.Make(struct
    type nonrec t = t
    let compare a b =
      let a = to_bytes a
      and b = to_bytes b in
      MBytes.compare a b
  end)

let of_b58check_opt s =
  if TzString.has_prefix ~prefix:Ed25519.b58check_encoding.encoded_prefix s then
    Option.map
      (Ed25519.of_b58check_opt s)
      ~f: (fun x -> Ed25519 x)
  else if TzString.has_prefix ~prefix:Secp256k1.b58check_encoding.encoded_prefix s then
    Option.map
      (Secp256k1.of_b58check_opt s)
      ~f: (fun x -> Secp256k1 x)
  else
    Base58.simple_decode b58check_encoding s

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

let to_b58check = function
  | Ed25519 b -> Ed25519.to_b58check b
  | Secp256k1 b -> Secp256k1.to_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

let to_short_b58check = function
  | Ed25519 b -> Ed25519.to_short_b58check b
  | Secp256k1 b -> Secp256k1.to_short_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

include Helpers.MakeEncoder(struct
    type nonrec t = t
    let name = name
    let title = title
    let raw_encoding =
      Data_encoding.conv
        to_bytes
        of_bytes_exn
        (Data_encoding.Fixed.bytes size)
    let of_b58check = of_b58check
    let of_b58check_opt = of_b58check_opt
    let of_b58check_exn = of_b58check_exn
    let to_b58check = to_b58check
    let to_short_b58check = to_short_b58check
  end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let of_secp256k1 s = Secp256k1 s
let of_ed25519 s = Ed25519 s

let zero = of_ed25519 Ed25519.zero

let hash msg =
  Blake2B.(to_bytes (hash_bytes [msg]))

let sign secret_key message =
  let message = hash message in
  match secret_key with
  | Secret_key.Ed25519 sk -> of_ed25519 (Ed25519.sign sk message)
  | Secret_key.Secp256k1 sk -> of_secp256k1 (Secp256k1.sign sk message)

let check public_key signature message =
  let message = hash message in
  match public_key, signature with
  | Public_key.Ed25519 pk, Unknown signature -> begin
      match Ed25519.of_bytes_opt signature with
      | Some s -> Ed25519.check pk s message
      | None -> false
    end
  | Public_key.Secp256k1 pk, Unknown signature -> begin
      match Secp256k1.of_bytes_opt signature with
      | Some s -> Secp256k1.check pk s message
      | None -> false
    end
  | Public_key.Ed25519 pk, Ed25519 signature ->
      Ed25519.check pk signature message
  | Public_key.Secp256k1 pk, Secp256k1 signature ->
      Secp256k1.check pk signature message
  | Public_key.Ed25519 _, Secp256k1 _ -> false
  | Public_key.Secp256k1 _, Ed25519 _ -> false

let append sk msg =
  MBytes.concat "" [msg; (to_bytes (sign sk msg))]

let concat msg signature =
  MBytes.concat "" [msg; (to_bytes signature)]

type algo =
  | Ed25519
  | Secp256k1

let algo_param () =
  Clic.parameter
    ~autocomplete:(fun _ -> return [ "ed25519" ; "secp256k1" ])
    begin fun _ name ->
      match name with
      | "ed25519" -> return Ed25519
      | "secp256k1" -> return Secp256k1
      | name ->
          failwith
            "Unknown signature algorithm (%s). \
             Available: 'ed25519' or 'secp256k1'"
            name
    end

let generate_key ?(algo = Ed25519) ?seed () =
  match algo with
  | Secp256k1 ->
      let pkh, pk, sk = Secp256k1.generate_key ?seed () in
      (Public_key_hash.Secp256k1 pkh,
       Public_key.Secp256k1 pk, Secret_key.Secp256k1 sk)
  | Ed25519 ->
      let pkh, pk, sk = Ed25519.generate_key ?seed () in
      (Public_key_hash.Ed25519 pkh,
       Public_key.Ed25519 pk, Secret_key.Ed25519 sk)
