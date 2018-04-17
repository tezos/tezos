(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open EndianBigstring

module Rand = struct
  external randombytes : Bigstring.t -> int -> unit =
    "ml_randombytes" [@@noalloc]

  let gen sz =
    let buf = Bigstring.create sz in
    randombytes buf sz ;
    buf

  let write buf =
    randombytes buf (Bigstring.length buf)
end

module Hash = struct
  let bytes = 64

  external sha512 :
    Bigstring.t -> Bigstring.t -> int -> unit =
    "ml_crypto_hash" [@@noalloc]

  let sha512 msg =
    let q = Bigstring.create bytes in
    sha512 q msg (Bigstring.length msg) ;
    q
end

let buf_of_z buf z =
  Bigstring.fill buf '\x00' ;
  let bits = Z.to_bits z in
  Bigstring.blit_of_string bits 0 buf 0 (String.length bits)

let unopt_invalid_arg1 ~msg f buf =
  match f buf with
  | Some v -> v
  | None -> invalid_arg msg

module Nonce = struct
  type t = Bigstring.t
  let bytes = 24

  let gen () =
    Rand.gen bytes

  let rec incr_byte b step byteno =
    let res = BigEndian.get_uint16 b byteno + step in
    let lo = res land 0xffff in
    let hi = res asr 16 in
    BigEndian.set_int16 b byteno lo ;
    if hi = 0 || byteno = 0 then ()
    else incr_byte b hi (byteno - 2)

  let increment ?(step = 1) nonce =
    let new_nonce = Bigstring.create 24 in
    Bigstring.blit nonce 0 new_nonce 0 24 ;
    incr_byte new_nonce step 22 ;
    new_nonce

  let of_bytes buf =
    try Some (Bigstring.sub buf 0 bytes) with _ -> None

  let of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Box.Nonce.of_bytes_exn" of_bytes

  let to_bytes nonce = nonce
end

module Secretbox = struct
  type key = Bigstring.t

  let keybytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let genkey () =
    Rand.gen 32

  let of_bytes buf =
    if Bigstring.length buf < keybytes then None
    else Some (Bigstring.sub buf 0 keybytes)

  let of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Secret_box.of_bytes_exn" of_bytes

  external secretbox :
    Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> unit = "ml_secretbox" [@@noalloc]

  external secretbox_open :
    Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> int = "ml_secretbox_open" [@@noalloc]

  let box ~key ~nonce ~msg =
    let msglen = Bigstring.length msg in
    let buflen = msglen + zerobytes in
    let buf = Bigstring.create buflen in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit msg 0 buf zerobytes msglen ;
    secretbox buf buf nonce key ;
    Bigstring.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_noalloc ~key ~nonce ~msg =
    secretbox msg msg nonce key

  let box_open ~key ~nonce ~cmsg =
    let msglen = Bigstring.length cmsg - boxzerobytes in
    let buf = Bigstring.create (zerobytes + msglen) in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match secretbox_open buf buf nonce key with
    | 0 -> Some (Bigstring.sub buf zerobytes msglen)
    | _ -> None

  let box_open_noalloc ~key ~nonce ~cmsg =
    match secretbox_open cmsg cmsg nonce key with
    | 0 -> true
    | _ -> false
end

module Box = struct
  type secret
  type public
  type combined
  type _ key =
    | Sk : Bigstring.t -> secret key
    | Pk : Bigstring.t -> public key
    | Ck : Bigstring.t -> combined key

  let skbytes = 32
  let pkbytes = 32
  let beforenmbytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let to_bytes : type a. a key -> Bigstring.t = function
    | Pk buf -> buf
    | Sk buf -> buf
    | Ck buf -> buf

  let blit_to_bytes :
    type a. a key -> ?pos:int -> Bigstring.t -> unit = fun key ?(pos=0) buf ->
    match key with
    | Pk pk -> Bigstring.blit pk 0 buf pos pkbytes
    | Sk sk -> Bigstring.blit sk 0 buf pos skbytes
    | Ck ck -> Bigstring.blit ck 0 buf pos beforenmbytes

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Bigstring.equal a b
    | Sk a, Sk b -> Bigstring.equal a b
    | Ck a, Ck b -> Bigstring.equal a b

  let sk_of_bytes buf =
    try Some (Sk (Bigstring.sub buf 0 skbytes)) with _ -> None
  let pk_of_bytes buf =
    try Some (Pk (Bigstring.sub buf 0 pkbytes)) with _ -> None
  let ck_of_bytes buf =
    try Some (Ck (Bigstring.sub buf 0 beforenmbytes)) with _ -> None

  let sk_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Box.sk_of_bytes_exn" sk_of_bytes
  let pk_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Box.pk_of_bytes_exn" pk_of_bytes
  let ck_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Box.ck_of_bytes_exn" ck_of_bytes

  external keypair :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_box_keypair" [@@noalloc]

  let keypair () =
    let sk = Bigstring.create skbytes in
    let pk = Bigstring.create pkbytes in
    keypair pk sk ;
    Pk pk, Sk sk

  external box_stub :
    Bigstring.t -> Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_box" [@@noalloc]

  let box ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~msg =
    let msglen = Bigstring.length msg in
    let buflen = msglen + zerobytes in
    let buf = Bigstring.create buflen in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit msg 0 buf zerobytes msglen ;
    box_stub buf buf nonce pk sk ;
    Bigstring.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_noalloc ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~msg =
    box_stub msg msg nonce pk sk

  external box_open_stub :
    Bigstring.t -> Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> int =
    "ml_crypto_box_open" [@@noalloc]

  let box_open ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~cmsg =
    let msglen = Bigstring.length cmsg - boxzerobytes in
    let buf = Bigstring.create (zerobytes + msglen) in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match box_open_stub buf buf nonce pk sk with
    | 0 -> Some (Bigstring.sub buf zerobytes msglen)
    | _ -> None

  let box_open_noalloc ~pk:(Pk pk) ~sk:(Sk sk) ~nonce ~cmsg =
    match box_open_stub cmsg cmsg nonce pk sk with
    | 0 -> true
    | _ -> false

  external box_beforenm :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_box_beforenm" [@@noalloc]

  let combine (Pk pk) (Sk sk) =
    let combined = Bigstring.create beforenmbytes in
    box_beforenm combined pk sk ;
    Ck combined

  external box_afternm :
    Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_box_afternm" [@@noalloc]

  let box_combined ~k:(Ck k) ~nonce ~msg =
    let msglen = Bigstring.length msg in
    let buflen = msglen + zerobytes in
    let buf = Bigstring.create buflen in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit msg 0 buf zerobytes msglen ;
    box_afternm buf buf nonce k ;
    Bigstring.sub buf boxzerobytes (buflen - boxzerobytes)

  let box_combined_noalloc ~k:(Ck k) ~nonce ~msg =
    box_afternm msg msg nonce k

  external box_open_afternm :
    Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> int =
    "ml_crypto_box_open_afternm" [@@noalloc]

  let box_open_combined ~k:(Ck k) ~nonce ~cmsg =
    let msglen = Bigstring.length cmsg - boxzerobytes in
    let buflen = msglen + zerobytes in
    let buf = Bigstring.create buflen in
    Bigstring.fill buf '\x00' ;
    Bigstring.blit cmsg 0 buf boxzerobytes (msglen + boxzerobytes) ;
    match box_open_afternm buf buf nonce k with
    | 0 -> Some (Bigstring.sub buf zerobytes msglen)
    | _ -> None

  let box_open_combined_noalloc ~k:(Ck k) ~nonce ~cmsg =
    match box_open_afternm cmsg cmsg nonce k with
    | 0 -> true
    | _ -> false
end

module Sign = struct
  type secret
  type extended
  type public
  type _ key =
    | Sk : Bigstring.t -> secret key
    | Ek : Bigstring.t -> extended key
    | Pk : Bigstring.t -> public key

  let bytes = 64
  let pkbytes = 32
  let skbytes = 64
  let ekbytes = 64
  let seedbytes = 32

  let sk_of_bytes buf =
    try Some (Sk (Bigstring.sub buf 0 skbytes)) with _ -> None
  let ek_of_bytes buf =
    try Some (Ek (Bigstring.sub buf 0 ekbytes)) with _ -> None
  let pk_of_bytes buf =
    try Some (Pk (Bigstring.sub buf 0 pkbytes)) with _ -> None

  let sk_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Sign.sk_of_bytes_exn" sk_of_bytes
  let ek_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Sign.ek_of_bytes_exn" ek_of_bytes
  let pk_of_bytes_exn =
    unopt_invalid_arg1 ~msg:"Sign.pk_of_bytes_exn" pk_of_bytes

  let to_bytes : type a. a key -> Bigstring.t = function
    | Pk buf -> buf
    | Sk buf -> buf
    | Ek buf -> buf

  let seed (Sk buf) = Bigstring.sub buf 0 seedbytes

  let blit_to_bytes :
    type a. a key -> ?pos:int -> Bigstring.t -> unit = fun key ?(pos=0) buf ->
    match key with
    | Pk pk -> Bigstring.blit pk 0 buf pos pkbytes
    | Sk sk -> Bigstring.blit sk 0 buf pos skbytes
    | Ek ek -> Bigstring.blit ek 0 buf pos ekbytes

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Bigstring.equal a b
    | Sk a, Sk b -> Bigstring.equal a b
    | Ek a, Ek b -> Bigstring.equal a b

  external keypair :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_sign_keypair" [@@noalloc]

  external keypair_seed :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_sign_keypair_seed" [@@noalloc]

  let keypair ?seed () =
    let pk = Bigstring.create pkbytes in
    let sk = Bigstring.create skbytes in
    begin match seed with
      | None -> keypair pk sk
      | Some buf ->
        if Bigstring.length buf < seedbytes then
          invalid_arg "Sign.keypair: seed must be at least 32 bytes long" ;
        Bigstring.blit buf 0 sk 0 pkbytes ;
        keypair_seed pk sk
    end ;
    Pk pk, Sk sk

  let extended (Sk sk) =
    let buf = Hash.sha512 (Bigstring.sub sk 0 pkbytes) in
    BigEndian.(set_int8 buf 0 (get_uint8 buf 0 land 248)) ;
    BigEndian.(set_int8 buf 31 (get_uint8 buf 31 land 127)) ;
    BigEndian.(set_int8 buf 31 (get_uint8 buf 31 lor 64)) ;
    Ek buf

  external sign :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_sign" [@@noalloc]

  external sign_extended :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_crypto_sign_extended" [@@noalloc]

  let sign ~key:(Sk sk) msg =
    let msglen = Bigstring.length msg in
    let buf = Bigstring.create (bytes + msglen) in
    Bigstring.blit msg 0 buf bytes msglen ;
    sign buf sk ;
    buf

  let sign_extended ~key:(Ek ek) msg =
    let msglen = Bigstring.length msg in
    let buf = Bigstring.create (bytes + msglen) in
    Bigstring.blit msg 0 buf bytes msglen ;
    sign_extended buf ek ;
    buf

  let detached ~key msg =
    Bigstring.sub (sign ~key msg) 0 bytes

  let detached_extended ~key msg =
    Bigstring.sub (sign_extended ~key msg) 0 bytes

  external verify :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> int =
    "ml_crypto_sign_open" [@@noalloc]

  let verify ~key:(Pk pk) smsg =
    let msg = Bigstring.(create (length smsg)) in
    match verify msg smsg pk with
    | -1 -> None
    | len -> Some (Bigstring.sub msg 0 len)

  let verify_detached ~key ~signature msg =
    let msglen = Bigstring.length msg in
    let buf = Bigstring.create (bytes + msglen) in
    Bigstring.blit signature 0 buf 0 bytes ;
    Bigstring.blit msg 0 buf bytes msglen ;
    match verify ~key buf with
    | None -> false
    | Some _ -> true

  external add :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_add" [@@noalloc]

  let add (Pk p) (Pk q) =
    let buf = Bigstring.create pkbytes in
    Bigstring.blit p 0 buf 0 pkbytes ;
    add buf q ;
    Pk buf

  external mult :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_scalarmult" [@@noalloc]

  external base :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_scalarbase" [@@noalloc]

  let mult (Pk q) s =
    let r = Bigstring.create pkbytes in
    let scalar = Bigstring.create pkbytes in
    buf_of_z scalar s ;
    mult r q scalar ;
    Pk r

  let base_direct s =
    let buf = Bigstring.create pkbytes in
    base buf s ;
    buf

  let base s =
    let r = Bigstring.create pkbytes in
    let scalar = Bigstring.create pkbytes in
    buf_of_z scalar s ;
    base r scalar ;
    Pk r

  let public : type a. a key -> public key = function
    | Pk _ as pk -> pk
    | Sk buf -> Pk (Bigstring.sub buf 32 32)
    | Ek buf  -> Pk (base_direct (Bigstring.sub buf 0 32))
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
