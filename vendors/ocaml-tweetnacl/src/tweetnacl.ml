(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Rand = struct
  external randombytes : Cstruct.buffer -> int -> unit =
    "ml_randombytes" [@@noalloc]

  let gen sz =
    let cs = Cstruct.create_unsafe sz in
    randombytes (Cstruct.to_bigarray cs) sz ;
    cs

  let write cs =
    Cstruct.(randombytes (to_bigarray cs) (len cs))
end

module Hash = struct
  let bytes = 64

  external sha512 :
    Cstruct.buffer -> Cstruct.buffer -> int -> unit =
    "ml_crypto_hash" [@@noalloc]

  let sha512 msg =
    let q = Cstruct.create_unsafe bytes in
    sha512 q.buffer msg.Cstruct.buffer (Cstruct.len msg) ;
    q
end

let cs_of_z cs z =
  Cstruct.memset cs 0 ;
  let bits = Z.to_bits z in
  Cstruct.blit_from_string bits 0 cs 0 (String.length bits)

let unopt_invalid_arg1 ~msg f cs =
  match f cs with
  | Some v -> v
  | None -> invalid_arg msg

module Nonce = struct
  type t = Cstruct.t
  let bytes = 24

  let gen () =
    Rand.gen bytes

  let rec incr_byte b step byteno =
    let res = Cstruct.BE.get_uint16 b byteno + step in
    let lo = res land 0xffff in
    let hi = res asr 16 in
    Cstruct.BE.set_uint16 b byteno lo ;
    if hi = 0 || byteno = 0 then ()
    else incr_byte b hi (byteno - 2)

  let increment ?(step = 1) nonce =
    let new_nonce = Cstruct.create_unsafe 24 in
    Cstruct.blit nonce 0 new_nonce 0 24 ;
    incr_byte new_nonce step 22 ;
    new_nonce

  let of_cstruct cs =
    try Some (Cstruct.sub cs 0 bytes) with _ -> None

  let of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Box.Nonce.of_cstruct_exn" of_cstruct

  let to_cstruct nonce = nonce
end

module Secretbox = struct
  type key = Cstruct.t

  let keybytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let genkey () =
    Rand.gen 32

  let of_cstruct cs =
    if Cstruct.len cs < keybytes then None
    else Some (Cstruct.sub cs 0 keybytes)

  let of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Secret_box.of_cstruct_exn" of_cstruct

  external secretbox :
    Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> unit = "ml_secretbox" [@@noalloc]

  external secretbox_open :
    Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> int = "ml_secretbox_open" [@@noalloc]

  let box ~key ~nonce ~msg =
    let msglen = Cstruct.len msg in
    let buflen = msglen + zerobytes in
    let buf = Cstruct.create buflen in
    Cstruct.blit msg 0 buf zerobytes msglen ;
    secretbox
      buf.buffer buf.buffer nonce.Cstruct.buffer key.Cstruct.buffer ;
    Cstruct.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_noalloc ~key ~nonce ~msg =
    secretbox
      msg.Cstruct.buffer msg.buffer nonce.Cstruct.buffer key.Cstruct.buffer

  let box_open ~key ~nonce ~cmsg =
    let msglen = Cstruct.len cmsg - boxzerobytes in
    let buf = Cstruct.create (zerobytes + msglen) in
    Cstruct.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match secretbox_open buf.buffer buf.buffer
            nonce.Cstruct.buffer key.Cstruct.buffer with
    | 0 -> Some (Cstruct.sub buf zerobytes msglen)
    | _ -> None

  let box_open_noalloc ~key ~nonce ~cmsg =
    match secretbox_open cmsg.Cstruct.buffer cmsg.buffer
            nonce.Cstruct.buffer key.Cstruct.buffer with
    | 0 -> true
    | _ -> false
end

module Box = struct
  type secret
  type public
  type combined
  type _ key =
    | Sk : Cstruct.t -> secret key
    | Pk : Cstruct.t -> public key
    | Ck : Cstruct.t -> combined key

  let skbytes = 32
  let pkbytes = 32
  let beforenmbytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let to_cstruct : type a. a key -> Cstruct.t = function
    | Pk cs -> cs
    | Sk cs -> cs
    | Ck cs -> cs

  let blit_to_cstruct :
    type a. a key -> ?pos:int -> Cstruct.t -> unit = fun key ?(pos=0) cs ->
    match key with
    | Pk pk -> Cstruct.blit pk 0 cs pos pkbytes
    | Sk sk -> Cstruct.blit sk 0 cs pos skbytes
    | Ck ck -> Cstruct.blit ck 0 cs pos beforenmbytes

  let pp : type a. Format.formatter -> a key -> unit = fun ppf -> function
    | Pk cs -> Format.fprintf ppf "P %a" Hex.pp (Hex.of_cstruct cs)
    | Sk cs -> Format.fprintf ppf "S %a" Hex.pp (Hex.of_cstruct cs)
    | Ck cs -> Format.fprintf ppf "C %a" Hex.pp (Hex.of_cstruct cs)

  let show t = Format.asprintf "%a" pp t

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Cstruct.equal a b
    | Sk a, Sk b -> Cstruct.equal a b
    | Ck a, Ck b -> Cstruct.equal a b

  let sk_of_cstruct cs =
    try Some (Sk (Cstruct.sub cs 0 skbytes)) with _ -> None
  let pk_of_cstruct cs =
    try Some (Pk (Cstruct.sub cs 0 pkbytes)) with _ -> None
  let ck_of_cstruct cs =
    try Some (Ck (Cstruct.sub cs 0 beforenmbytes)) with _ -> None

  let sk_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Box.sk_of_cstruct_exn" sk_of_cstruct
  let pk_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Box.pk_of_cstruct_exn" pk_of_cstruct
  let ck_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Box.ck_of_cstruct_exn" ck_of_cstruct

  external keypair :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_box_keypair" [@@noalloc]

  let keypair () =
    let sk = Cstruct.create skbytes in
    let pk = Cstruct.create pkbytes in
    keypair pk.buffer sk.buffer ;
    Pk pk, Sk sk

  external box_stub :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_box" [@@noalloc]

  let box ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~msg =
    let msglen = Cstruct.len msg in
    let buflen = msglen + zerobytes in
    let buf = Cstruct.create buflen in
    Cstruct.blit msg 0 buf zerobytes msglen ;
    box_stub
      buf.buffer buf.buffer nonce.Cstruct.buffer pk.buffer sk.buffer ;
    Cstruct.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_noalloc ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~msg =
    box_stub
      msg.Cstruct.buffer msg.buffer nonce.Cstruct.buffer pk.buffer sk.buffer

  external box_open_stub :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_crypto_box_open" [@@noalloc]

  let box_open ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~cmsg =
    let msglen = Cstruct.len cmsg - boxzerobytes in
    let buf = Cstruct.create (zerobytes + msglen) in
    Cstruct.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match box_open_stub buf.buffer buf.buffer
            nonce.Cstruct.buffer pk.buffer sk.buffer with
    | 0 -> Some (Cstruct.sub buf zerobytes msglen)
    | _ -> None

  let box_open_noalloc ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~cmsg =
    match box_open_stub cmsg.Cstruct.buffer cmsg.buffer
            nonce.Cstruct.buffer pk.buffer sk.buffer with
    | 0 -> true
    | _ -> false

  external box_beforenm :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_box_beforenm" [@@noalloc]

  let combine (Pk pk) (Sk sk) =
    let combined = Cstruct.create_unsafe beforenmbytes in
    box_beforenm combined.buffer pk.buffer sk.buffer ;
    Ck combined

  external box_afternm :
    Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_box_afternm" [@@noalloc]

  let box_combined ~k:(Ck k) ~nonce ~msg =
    let msglen = Cstruct.len msg in
    let buflen = msglen + zerobytes in
    let buf = Cstruct.create buflen in
    Cstruct.blit msg 0 buf zerobytes msglen ;
    box_afternm buf.buffer buf.buffer nonce.Cstruct.buffer k.buffer ;
    Cstruct.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_combined_noalloc ~k:(Ck k) ~nonce ~msg =
    box_afternm msg.Cstruct.buffer msg.buffer nonce.Cstruct.buffer k.buffer

  external box_open_afternm :
    Cstruct.buffer -> Cstruct.buffer ->
    Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_crypto_box_open_afternm" [@@noalloc]

  let box_open_combined ~k:(Ck k) ~nonce ~cmsg =
    let msglen = Cstruct.len cmsg - boxzerobytes in
    let buflen = msglen + zerobytes in
    let buf = Cstruct.create buflen in
    Cstruct.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match box_open_afternm buf.buffer buf.buffer
            nonce.Cstruct.buffer k.buffer with
    | 0 -> Some (Cstruct.sub buf zerobytes msglen)
    | _ -> None

  let box_open_combined_noalloc ~k:(Ck k) ~nonce ~cmsg =
    match box_open_afternm cmsg.Cstruct.buffer cmsg.buffer
            nonce.Cstruct.buffer k.buffer with
    | 0 -> true
    | _ -> false
end

module Sign = struct
  type secret
  type extended
  type public
  type _ key =
    | Sk : Cstruct.t -> secret key
    | Ek : Cstruct.t -> extended key
    | Pk : Cstruct.t -> public key

  let bytes = 64
  let pkbytes = 32
  let skbytes = 64
  let ekbytes = 64
  let seedbytes = 32

  let sk_of_cstruct cs =
    try Some (Sk (Cstruct.sub cs 0 skbytes)) with _ -> None
  let ek_of_cstruct cs =
    try Some (Ek (Cstruct.sub cs 0 ekbytes)) with _ -> None
  let pk_of_cstruct cs =
    try Some (Pk (Cstruct.sub cs 0 pkbytes)) with _ -> None

  let sk_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Sign.sk_of_cstruct_exn" sk_of_cstruct
  let ek_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Sign.ek_of_cstruct_exn" ek_of_cstruct
  let pk_of_cstruct_exn =
    unopt_invalid_arg1 ~msg:"Sign.pk_of_cstruct_exn" pk_of_cstruct

  let to_cstruct : type a. a key -> Cstruct.t = function
    | Pk cs -> cs
    | Sk cs -> cs
    | Ek cs -> cs

  let seed (Sk cs) = Cstruct.sub cs 0 seedbytes

  let blit_to_cstruct :
    type a. a key -> ?pos:int -> Cstruct.t -> unit = fun key ?(pos=0) cs ->
    match key with
    | Pk pk -> Cstruct.blit pk 0 cs pos pkbytes
    | Sk sk -> Cstruct.blit sk 0 cs pos skbytes
    | Ek ek -> Cstruct.blit ek 0 cs pos ekbytes

  let pp : type a. Format.formatter -> a key -> unit = fun ppf -> function
    | Pk cs -> Format.fprintf ppf "P %a" Hex.pp (Hex.of_cstruct cs)
    | Sk cs -> Format.fprintf ppf "S %a" Hex.pp (Hex.of_cstruct cs)
    | Ek cs -> Format.fprintf ppf "E %a" Hex.pp (Hex.of_cstruct cs)

  let show t = Format.asprintf "%a" pp t

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Cstruct.equal a b
    | Sk a, Sk b -> Cstruct.equal a b
    | Ek a, Ek b -> Cstruct.equal a b

  external keypair :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_sign_keypair" [@@noalloc]

  external keypair_seed :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_sign_keypair_seed" [@@noalloc]

  let keypair ?seed () =
    let pk = Cstruct.create_unsafe pkbytes in
    let sk = Cstruct.create_unsafe skbytes in
    begin match seed with
      | None ->
          Cstruct.(keypair (to_bigarray pk) (to_bigarray sk))
      | Some cs ->
          if Cstruct.len cs < seedbytes then
            invalid_arg "Sign.keypair: seed must be at least 32 bytes long" ;
          Cstruct.blit cs 0 sk 0 pkbytes ;
          Cstruct.(keypair_seed (to_bigarray pk) (to_bigarray sk))
    end ;
    Pk pk, Sk sk

  let extended (Sk sk) =
    let cs = Hash.sha512 (Cstruct.sub sk 0 pkbytes) in
    Cstruct.(set_uint8 cs 0 (get_uint8 cs 0 land 248)) ;
    Cstruct.(set_uint8 cs 31 (get_uint8 cs 31 land 127)) ;
    Cstruct.(set_uint8 cs 31 (get_uint8 cs 31 lor 64)) ;
    Ek cs

  external sign :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_sign" [@@noalloc]

  external sign_extended :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_crypto_sign_extended" [@@noalloc]

  let sign ~key:(Sk sk) msg =
    let msglen = Cstruct.len msg in
    let cs = Cstruct.create_unsafe (bytes + msglen) in
    Cstruct.blit msg 0 cs bytes msglen ;
    Cstruct.(sign (to_bigarray cs) (to_bigarray sk)) ;
    cs

  let sign_extended ~key:(Ek ek) msg =
    let msglen = Cstruct.len msg in
    let cs = Cstruct.create_unsafe (bytes + msglen) in
    Cstruct.blit msg 0 cs bytes msglen ;
    Cstruct.(sign_extended (to_bigarray cs) (to_bigarray ek)) ;
    cs

  let detached ~key msg =
    Cstruct.sub (sign ~key msg) 0 bytes

  let detached_extended ~key msg =
    Cstruct.sub (sign_extended ~key msg) 0 bytes

  external verify :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_crypto_sign_open" [@@noalloc]

  let verify ~key:(Pk pk) smsg =
    let mlen = Cstruct.create_unsafe 8 in
    let msg = Cstruct.(create (len smsg)) in
    let ret = Cstruct.(verify
                         (to_bigarray msg) (to_bigarray mlen)
                         (to_bigarray smsg) (to_bigarray pk)) in
    match ret with
    | 0 ->
        let len = Cstruct.LE.get_uint64 mlen 0 |> Int64.to_int in
        Some (Cstruct.sub msg 0 len)
    | _ -> None

  let verify_detached ~key ~signature msg =
    let cs = Cstruct.create_unsafe (bytes + Cstruct.len msg) in
    Cstruct.blit signature 0 cs 0 bytes ;
    Cstruct.blit msg 0 cs bytes (Cstruct.len msg) ;
    match verify ~key cs with
    | None -> false
    | Some _ -> true

  external add :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_add" [@@noalloc]

  let add (Pk p) (Pk q) =
    let cs = Cstruct.create_unsafe pkbytes in
    Cstruct.blit p 0 cs 0 pkbytes ;
    Cstruct.(add (to_bigarray cs) (to_bigarray q)) ;
    Pk cs

  external mult :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_scalarmult" [@@noalloc]

  external base :
    Cstruct.buffer -> Cstruct.buffer -> unit =
    "ml_scalarbase" [@@noalloc]

  let mult (Pk q) s =
    let r = Cstruct.create_unsafe pkbytes in
    let scalar = Cstruct.create_unsafe pkbytes in
    cs_of_z scalar s ;
    Cstruct.(mult (to_bigarray r) (to_bigarray q) (to_bigarray scalar)) ;
    Pk r

  let base_direct s =
    let cs = Cstruct.create_unsafe pkbytes in
    Cstruct.(base (to_bigarray cs) (to_bigarray s)) ;
    cs

  let base s =
    let r = Cstruct.create_unsafe pkbytes in
    let scalar = Cstruct.create_unsafe pkbytes in
    cs_of_z scalar s ;
    Cstruct.(base (to_bigarray r) (to_bigarray scalar)) ;
    Pk r

  let public : type a. a key -> public key = function
    | Pk _ as pk -> pk
    | Sk cs -> Pk (Cstruct.sub cs 32 32)
    | Ek cs -> Pk (base_direct (Cstruct.sub cs 0 32))
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
