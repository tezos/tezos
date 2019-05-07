(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t

type watermark =
  | Block_header of Chain_id.t
  | Endorsement of Chain_id.t
  | Generic_operation
  | Custom of MBytes.t

module Public_key_hash = struct

  type t = public_key_hash =
    | Ed25519 of Ed25519.Public_key_hash.t
    | Secp256k1 of Secp256k1.Public_key_hash.t
    | P256 of P256.Public_key_hash.t

  let name = "Signature.Public_key_hash"
  let title = "A Ed25519, Secp256k1, or P256 public key hash"

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
    def "public_key_hash" ~description:title @@
    union [
      case (Tag 0) Ed25519.Public_key_hash.encoding
        ~title:"Ed25519"
        (function Ed25519 x -> Some x | _ -> None)
        (function x -> Ed25519 x);
      case (Tag 1) Secp256k1.Public_key_hash.encoding
        ~title:"Secp256k1"
        (function Secp256k1 x -> Some x | _ -> None)
        (function x -> Secp256k1 x) ;
      case (Tag 2)
        ~title:"P256" P256.Public_key_hash.encoding
        (function P256 x -> Some x | _ -> None)
        (function x -> P256 x)
    ]

  let to_bytes s =
    Data_encoding.Binary.to_bytes_exn raw_encoding s
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
    | Some P256.Public_key_hash.Data pkh -> Some (P256 pkh)
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
    | P256 pkh -> P256.Public_key_hash.to_b58check pkh

  let to_short_b58check = function
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_short_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_short_b58check pkh
    | P256 pkh -> P256.Public_key_hash.to_short_b58check pkh

  let to_path key l = match key with
    | Ed25519 h -> "ed25519" :: Ed25519.Public_key_hash.to_path h l
    | Secp256k1 h -> "secp256k1" :: Secp256k1.Public_key_hash.to_path h l
    | P256 h -> "p256" :: P256.Public_key_hash.to_path h l

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
    | "p256" :: q -> begin
        match P256.Public_key_hash.of_path q with
        | Some pkh -> Some (P256 pkh)
        | None -> None
      end
    | _ -> assert false (* FIXME classification des erreurs *)

  let of_path_exn = function
    | "ed25519" :: q -> Ed25519 (Ed25519.Public_key_hash.of_path_exn q)
    | "secp256k1" :: q -> Secp256k1 (Secp256k1.Public_key_hash.of_path_exn q)
    | "p256" :: q -> P256 (P256.Public_key_hash.of_path_exn q)
    | _ -> assert false (* FIXME classification des erreurs *)

  let path_length =
    let l1 = Ed25519.Public_key_hash.path_length
    and l2 = Secp256k1.Public_key_hash.path_length
    and l3 = P256.Public_key_hash.path_length in
    assert Compare.Int.(l1 = l2) ;
    assert Compare.Int.(l1 = l3) ;
    1 + l1

  let prefix_path _ = assert false (* unused *)

  let hash = Hashtbl.hash

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        match (a, b) with
        | Ed25519 x, Ed25519 y ->
            Ed25519.Public_key_hash.compare x y
        | Secp256k1 x, Secp256k1 y ->
            Secp256k1.Public_key_hash.compare x y
        | P256 x, P256 y ->
            P256.Public_key_hash.compare x y
        | _ -> Pervasives.compare a b
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

  let rpc_arg =
    RPC_arg.like
      rpc_arg
      ~descr:"A Secp256k1 of a Ed25519 public key hash (Base58Check-encoded)"
      "pkh"

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

module Public_key = struct

  type t = public_key =
    | Ed25519 of Ed25519.Public_key.t
    | Secp256k1 of Secp256k1.Public_key.t
    | P256 of P256.Public_key.t

  let name = "Signature.Public_key"
  let title = "A Ed25519, Secp256k1, or P256 public key"

  let hash pk =
    match pk with
    | Ed25519 pk -> Public_key_hash.Ed25519 (Ed25519.Public_key.hash pk)
    | Secp256k1 pk -> Public_key_hash.Secp256k1 (Secp256k1.Public_key.hash pk)
    | P256 pk -> Public_key_hash.P256 (P256.Public_key.hash pk)

  include Compare.Make(struct
      type nonrec t = t
      let compare a b = match (a, b) with
        | Ed25519 x, Ed25519 y -> Ed25519.Public_key.compare x y
        | Secp256k1 x, Secp256k1 y -> Secp256k1.Public_key.compare x y
        | P256 x, P256 y -> P256.Public_key.compare x y
        | _ -> Pervasives.compare a b
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
    | Some (P256.Public_key.Data public_key) -> Some (P256 public_key)
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
    | P256 pk -> P256.Public_key.to_b58check pk

  let to_short_b58check = function
    | Ed25519 pk -> Ed25519.Public_key.to_short_b58check pk
    | Secp256k1 pk -> Secp256k1.Public_key.to_short_b58check pk
    | P256 pk -> P256.Public_key.to_short_b58check pk

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        def "public_key" ~description:title @@
        union [
          case (Tag 0) Ed25519.Public_key.encoding
            ~title:"Ed25519"
            (function Ed25519 x -> Some x | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Public_key.encoding
            ~title:"Secp256k1"
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x) ;
          case
            ~title:"P256" (Tag 2) P256.Public_key.encoding
            (function P256 x -> Some x | _ -> None)
            (function x -> P256 x)
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
    | P256 of P256.Secret_key.t

  let name = "Signature.Secret_key"
  let title = "A Ed25519, Secp256k1 or P256 secret key"

  let to_public_key = function
    | Ed25519 sk -> Public_key.Ed25519 (Ed25519.Secret_key.to_public_key sk)
    | Secp256k1 sk -> Public_key.Secp256k1 (Secp256k1.Secret_key.to_public_key sk)
    | P256 sk -> Public_key.P256 (P256.Secret_key.to_public_key sk)

  include Compare.Make(struct
      type nonrec t = t
      let compare a b = match (a, b) with
        | Ed25519 x, Ed25519 y -> Ed25519.Secret_key.compare x y
        | Secp256k1 x, Secp256k1 y -> Secp256k1.Secret_key.compare x y
        | P256 x, P256 y -> P256.Secret_key.compare x y
        | _ -> Pervasives.compare a b
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
    | Some (P256.Secret_key.Data sk) -> Some (P256 sk)
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
    | P256 sk -> P256.Secret_key.to_b58check sk

  let to_short_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_short_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_short_b58check sk
    | P256 sk -> P256.Secret_key.to_short_b58check sk

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        def "secret_key" ~description:title @@
        union [
          case (Tag 0) Ed25519.Secret_key.encoding
            ~title:"Ed25519"
            (function Ed25519 x -> Some x | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Secret_key.encoding
            ~title:"Secp256k1"
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x) ;
          case (Tag 2)
            ~title:"P256" P256.Secret_key.encoding
            (function P256 x -> Some x | _ -> None)
            (function x -> P256 x)
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
  | P256 of P256.t
  | Unknown of MBytes.t

let name = "Signature"
let title = "A Ed25519, Secp256k1 or P256 signature"

let size =
  assert (Ed25519.size = Secp256k1.size && Secp256k1.size = P256.size) ;
  Ed25519.size

let to_bytes = function
  | Ed25519 b -> Ed25519.to_bytes b
  | Secp256k1 b -> Secp256k1.to_bytes b
  | P256 b -> P256.to_bytes b
  | Unknown b -> b

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
  else if TzString.has_prefix ~prefix:P256.b58check_encoding.encoded_prefix s then
    Option.map
      (P256.of_b58check_opt s)
      ~f: (fun x -> P256 x)
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
  | P256 b -> P256.to_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

let to_short_b58check = function
  | Ed25519 b -> Ed25519.to_short_b58check b
  | Secp256k1 b -> Secp256k1.to_short_b58check b
  | P256 b -> P256.to_short_b58check b
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

let of_ed25519 s = Ed25519 s
let of_secp256k1 s = Secp256k1 s
let of_p256 s = P256 s

let zero = of_ed25519 Ed25519.zero

let bytes_of_watermark = function
  | Block_header chain_id -> MBytes.concat "" [ MBytes.of_string "\x01" ; Chain_id.to_bytes chain_id ]
  | Endorsement chain_id -> MBytes.concat "" [ MBytes.of_string "\x02" ; Chain_id.to_bytes chain_id ]
  | Generic_operation -> MBytes.of_string "\x03"
  | Custom bytes      -> bytes

let pp_watermark ppf =
  let open Format in
  function
  | Block_header chain_id -> fprintf ppf "Block-header: %a" Chain_id.pp chain_id
  | Endorsement chain_id ->  fprintf ppf "Endorsement: %a" Chain_id.pp chain_id
  | Generic_operation -> pp_print_string ppf "Generic-operation"
  | Custom bytes      ->
      let hexed = MBytes.to_hex bytes |> Hex.show in
      fprintf ppf "Custom: 0x%s"
        (try String.sub hexed 0 10 ^ "..." with _ ->  hexed)

let sign ?watermark secret_key message =
  let watermark = Option.map ~f:bytes_of_watermark watermark in
  match secret_key with
  | Secret_key.Ed25519 sk -> of_ed25519 (Ed25519.sign ?watermark sk message)
  | Secp256k1 sk -> of_secp256k1 (Secp256k1.sign ?watermark sk message)
  | P256 sk -> of_p256 (P256.sign ?watermark sk message)

let check ?watermark public_key signature message =
  let watermark = Option.map ~f:bytes_of_watermark watermark in
  match public_key, signature with
  | Public_key.Ed25519 pk, Unknown signature -> begin
      match Ed25519.of_bytes_opt signature with
      | Some s -> Ed25519.check ?watermark pk s message
      | None -> false
    end
  | Public_key.Secp256k1 pk, Unknown signature -> begin
      match Secp256k1.of_bytes_opt signature with
      | Some s -> Secp256k1.check ?watermark pk s message
      | None -> false
    end
  | Public_key.P256 pk, Unknown signature -> begin
      match P256.of_bytes_opt signature with
      | Some s -> P256.check ?watermark pk s message
      | None -> false
    end
  | Public_key.Ed25519 pk, Ed25519 signature ->
      Ed25519.check ?watermark pk signature message
  | Public_key.Secp256k1 pk, Secp256k1 signature ->
      Secp256k1.check ?watermark pk signature message
  | Public_key.P256 pk, P256 signature ->
      P256.check ?watermark pk signature message
  | _ -> false

let append ?watermark sk msg =
  MBytes.concat "" [msg; (to_bytes (sign ?watermark sk msg))]

let concat msg signature =
  MBytes.concat "" [msg; (to_bytes signature)]

type algo =
  | Ed25519
  | Secp256k1
  | P256

let algo_param () =
  Clic.parameter
    ~autocomplete:(fun _ -> return [ "ed25519" ; "secp256k1" ; "p256"])
    begin fun _ name ->
      match name with
      | "ed25519" -> return Ed25519
      | "secp256k1" -> return Secp256k1
      | "p256" -> return P256
      | name ->
          failwith
            "Unknown signature algorithm (%s). \
             Available: 'ed25519', 'secp256k1' or 'p256'"
            name
    end

let generate_key ?(algo = Ed25519) ?seed () =
  match algo with
  | Ed25519 ->
      let pkh, pk, sk = Ed25519.generate_key ?seed () in
      (Public_key_hash.Ed25519 pkh,
       Public_key.Ed25519 pk, Secret_key.Ed25519 sk)
  | Secp256k1 ->
      let pkh, pk, sk = Secp256k1.generate_key ?seed () in
      (Public_key_hash.Secp256k1 pkh,
       Public_key.Secp256k1 pk, Secret_key.Secp256k1 sk)
  | P256 ->
      let pkh, pk, sk = P256.generate_key ?seed () in
      (Public_key_hash.P256 pkh,
       Public_key.P256 pk, Secret_key.P256 sk)

let deterministic_nonce sk msg =
  match sk with
  | Secret_key.Ed25519 sk   -> Ed25519.deterministic_nonce sk msg
  | Secret_key.Secp256k1 sk -> Secp256k1.deterministic_nonce sk msg
  | Secret_key.P256 sk      -> P256.deterministic_nonce sk msg

let deterministic_nonce_hash sk msg =
  match sk with
  | Secret_key.Ed25519 sk   -> Ed25519.deterministic_nonce_hash sk msg
  | Secret_key.Secp256k1 sk -> Secp256k1.deterministic_nonce_hash sk msg
  | Secret_key.P256 sk      -> P256.deterministic_nonce_hash sk msg
