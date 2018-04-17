(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Public_key_hash = Blake2B.Make(Base58)(struct
    let name  = "Secp256k1.Public_key_hash"
    let title = "A Secp256k1 public key hash"
    let b58check_prefix = Base58.Prefix.secp256k1_public_key_hash
    let size = Some 20
  end)

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz2" 36

open Secp256k1_ml.External

let context = Context.(create [Verify; Sign])

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

  let size = 33 (* TODO not hardcoded ?? *)

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
        MBytes.compare (Key.to_buffer a) (Key.to_buffer b)
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

  type t = Key.secret Key.t

  let name = "Secp256k1.Secret_key"
  let title = "A Secp256k1 secret key"

  let size = 32 (* TODO don't hardcode *)

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
        MBytes.compare (Key.to_buffer a) (Key.to_buffer b)
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

type t = MBytes.t

let name = "Secp256k1"
let title = "A Secp256k1 signature"

let size = 64 (* TODO don't hardcode? *)

let of_bytes_opt s =
  if MBytes.length s = size then Some s else None
let to_bytes x = x

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

let zero = of_bytes_exn (MBytes.init size '\000')

let sign secret_key message =
  match Sign.msg_of_bytes message with
  | None ->
      Pervasives.invalid_arg
        "Secp256k1.sign: argument message couldn't be converted"
  | Some msg ->  begin
      match Sign.sign context ~sk:secret_key ~msg with
      | Ok signature ->
          Sign.to_bytes ~der:false context signature
      | _ ->
          Pervasives.invalid_arg "Secp256k1.sign: couldn't sign"
    end


let check public_key signature msg =
  match Sign.msg_of_bytes msg, Sign.read context signature with
  | Some msg, Ok signature -> begin
      match Sign.verify context ~pk:public_key ~msg ~signature with
      | Ok b -> b
      | _ -> false
    end
  | _, _ -> false

let concat msg signature =
  MBytes.concat msg signature

let append key msg =
  let signature = sign key msg in
  concat msg signature

let generate_key () =
  let sk = Key.read_sk_exn context (Tweetnacl.Rand.gen 32) in
  let pk = Key.neuterize_exn context sk in
  let pkh = Public_key.hash pk in
  (pkh, pk, sk)

