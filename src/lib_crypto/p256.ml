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

module Public_key_hash = struct
  include Blake2B.Make(Base58)(struct
      let name  = "P256.Public_key_hash"
      let title = "A P256 public key hash"
      let b58check_prefix = Base58.Prefix.p256_public_key_hash
      let size = Some 20
    end)

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz3" 36

open Uecc

module Public_key = struct

  type t = (secp256r1, public) key

  let name  = "P256.Public_key"
  let title = "A P256 public key"

  let to_bytes = to_bytes ~compress:true
  let of_bytes_opt = pk_of_bytes secp256r1

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let size = compressed_size secp256r1

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.p256_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let () =
    Base58.check_encoded_prefix b58check_encoding "p2pk" 55

  let hash v =
    Public_key_hash.hash_bytes [to_bytes v]

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (to_bytes a) (to_bytes b)
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

  type t = (secp256r1, secret) key

  let name = "P256.Secret_key"
  let title = "A P256 secret key"

  let size = sk_size secp256r1

  let of_bytes_opt buf =
    Option.map ~f:fst (sk_of_bytes secp256r1 buf)

  let to_bytes = to_bytes ~compress:true

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let to_public_key = neuterize

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.p256_secret_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let () =
    Base58.check_encoded_prefix b58check_encoding "p2sk" 54

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (to_bytes a) (to_bytes b)
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

type t = MBytes.t

type watermark = MBytes.t

let name = "P256"
let title = "A P256 signature"

let size = pk_size secp256r1

let of_bytes_opt s =
  if MBytes.length s = size then Some s else None

let to_bytes s = s

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

type Base58.data +=
  | Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.p256_signature
    ~length: size
    ~to_raw: to_string
    ~of_raw: of_string_opt
    ~wrap: (fun x -> Data x)

let () =
  Base58.check_encoded_prefix b58check_encoding "p2sig" 98

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Helpers.MakeB58(struct
    type nonrec t = t
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

let zero = of_bytes_exn (MBytes.make size '\000')

let sign ?watermark sk msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  match sign sk msg with
  | None ->
      (* Will never happen in practice. This can only happen in case
         of RNG error. *)
      invalid_arg "P256.sign: internal error"
  | Some signature -> signature

let check ?watermark public_key signature msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  verify public_key ~msg ~signature

let generate_key ?(seed=Rand.generate 32) () =
  let seedlen = MBytes.length seed in
  if seedlen < 32 then
    invalid_arg (Printf.sprintf "P256.generate_key: seed must be at \
                                 least 32 bytes long (was %d)" seedlen) ;
  match sk_of_bytes secp256r1 seed with
  | None -> invalid_arg "P256.generate_key: invalid seed (very rare!)"
  | Some (sk, pk) ->
      let pkh = Public_key.hash pk in
      pkh, pk, sk

let deterministic_nonce sk msg =
  Hacl.Hash.SHA256.HMAC.digest ~key: (Secret_key.to_bytes sk) ~msg

let deterministic_nonce_hash sk msg =
  Blake2B.to_bytes (Blake2B.hash_bytes [deterministic_nonce sk msg])


include Compare.Make(struct
    type nonrec t = t
    let compare = MBytes.compare
  end)
