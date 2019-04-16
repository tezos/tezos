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
      let name  = "Secp256k1.Public_key_hash"
      let title = "A Secp256k1 public key hash"
      let b58check_prefix = Base58.Prefix.secp256k1_public_key_hash
      let size = Some 20
    end)
  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz2" 36

open Libsecp256k1.External

let context =
  let ctx = Context.create () in
  match Context.randomize ctx (Rand.generate 32) with
  | false -> failwith "Secp256k1 context randomization failed. Aborting."
  | true -> ctx

module Public_key = struct

  type t = Key.public Key.t

  let name  = "Secp256k1.Public_key"
  let title = "A Secp256k1 public key"

  let to_bytes pk = Key.to_bytes context pk
  let of_bytes_opt s =
    try Some (Key.read_pk_exn context s)
    with _ -> None

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let size = Key.compressed_pk_bytes

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.secp256k1_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let () =
    Base58.check_encoded_prefix b58check_encoding "sppk" 55

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

  type t = Key.secret Key.t

  let name = "Secp256k1.Secret_key"
  let title = "A Secp256k1 secret key"

  let size = Key.secret_bytes

  let of_bytes_opt s =
    match Key.read_sk context s with
    | Ok x -> Some x
    | _ -> None
  let to_bytes x = Key.to_bytes context x

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let to_public_key key = Key.neuterize_exn context key

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.secp256k1_secret_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let () =
    Base58.check_encoded_prefix b58check_encoding "spsk" 54

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (Key.buffer a) (Key.buffer b)
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

type t = Sign.plain Sign.t

type watermark = MBytes.t

let name = "Secp256k1"
let title = "A Secp256k1 signature"

let size = Sign.plain_bytes

let of_bytes_opt s =
  match Sign.read context s with Ok s -> Some s | Error _ -> None

let to_bytes = Sign.to_bytes ~der:false context

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

type Base58.data +=
  | Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.secp256k1_signature
    ~length: size
    ~to_raw: to_string
    ~of_raw: of_string_opt
    ~wrap: (fun x -> Data x)

let () =
  Base58.check_encoded_prefix b58check_encoding "spsig1" 99

include Compare.Make(struct
    type nonrec t = t
    let compare a b =
      MBytes.compare (Sign.buffer a) (Sign.buffer b)
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

let zero = of_bytes_exn (MBytes.make size '\000')

let sign ?watermark sk msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  Sign.sign_exn context ~sk msg

let check ?watermark public_key signature msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  Sign.verify_exn context ~pk:public_key ~msg ~signature

let generate_key ?(seed=Rand.generate 32) () =
  let sk = Key.read_sk_exn context seed in
  let pk = Key.neuterize_exn context sk in
  let pkh = Public_key.hash pk in
  pkh, pk, sk

let deterministic_nonce sk msg =
  Hacl.Hash.SHA256.HMAC.digest ~key: (Secret_key.to_bytes sk) ~msg

let deterministic_nonce_hash sk msg =
  Blake2B.to_bytes (Blake2B.hash_bytes [deterministic_nonce sk msg])
