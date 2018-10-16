(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type curve

type secp160r1
type secp192r1
type secp224r1
type secp256r1
type secp256k1

type _ t =
  | Secp160r1 : curve -> secp160r1 t
  | Secp192r1 : curve -> secp192r1 t
  | Secp224r1 : curve -> secp224r1 t
  | Secp256r1 : curve -> secp256r1 t
  | Secp256k1 : curve -> secp256k1 t

external curve : int -> curve = "uECC_curve_stub"
external sk_size : curve -> int = "uECC_curve_private_key_size_stub" [@@noalloc]
external pk_size : curve -> int = "uECC_curve_public_key_size_stub" [@@noalloc]

let sk_sizes = Hashtbl.create 5
let pk_sizes = Hashtbl.create 5

let secp160r1 =
  let c = curve 0 in
  Hashtbl.add sk_sizes c (sk_size c) ;
  Hashtbl.add pk_sizes c (pk_size c) ;
  Secp160r1 c
let secp192r1 =
  let c = curve 1 in
  Hashtbl.add sk_sizes c (sk_size c) ;
  Hashtbl.add pk_sizes c (pk_size c) ;
  Secp192r1 c
let secp224r1 =
  let c = curve 2 in
  Hashtbl.add sk_sizes c (sk_size c) ;
  Hashtbl.add pk_sizes c (pk_size c) ;
  Secp224r1 c
let secp256r1 =
  let c = curve 3 in
  Hashtbl.add sk_sizes c (sk_size c) ;
  Hashtbl.add pk_sizes c (pk_size c) ;
  Secp256r1 c
let secp256k1 =
  let c = curve 4 in
  Hashtbl.add sk_sizes c (sk_size c) ;
  Hashtbl.add pk_sizes c (pk_size c) ;
  Secp256k1 c

let to_curve : type a. a t -> curve = function
  | Secp160r1 curve -> curve
  | Secp192r1 curve -> curve
  | Secp224r1 curve -> curve
  | Secp256r1 curve -> curve
  | Secp256k1 curve -> curve

let sk_size : type a. a t -> int = function
  | Secp160r1 curve -> Hashtbl.find sk_sizes curve
  | Secp192r1 curve -> Hashtbl.find sk_sizes curve
  | Secp224r1 curve -> Hashtbl.find sk_sizes curve
  | Secp256r1 curve -> Hashtbl.find sk_sizes curve
  | Secp256k1 curve -> Hashtbl.find sk_sizes curve

let pk_size : type a. a t -> int = function
  | Secp160r1 curve -> Hashtbl.find pk_sizes curve
  | Secp192r1 curve -> Hashtbl.find pk_sizes curve
  | Secp224r1 curve -> Hashtbl.find pk_sizes curve
  | Secp256r1 curve -> Hashtbl.find pk_sizes curve
  | Secp256k1 curve -> Hashtbl.find pk_sizes curve

let compressed_size k =
  pk_size k / 2 + 1

external keypair :
  Bigstring.t -> Bigstring.t -> curve -> bool = "uECC_make_key_stub" [@@noalloc]

external pk_of_sk :
  Bigstring.t -> Bigstring.t -> curve -> bool = "uECC_compute_public_key_stub" [@@noalloc]
external valid_pk :
  Bigstring.t -> curve -> bool = "uECC_valid_public_key_stub" [@@noalloc]

external compress :
  Bigstring.t -> Bigstring.t -> curve -> unit = "uECC_compress_stub" [@@noalloc]
external decompress :
  Bigstring.t -> Bigstring.t -> curve -> unit = "uECC_decompress_stub" [@@noalloc]

type secret
type public

type (_, _) key =
  | Sk : Bigstring.t * 'a t -> ('a, secret) key
  | Pk : Bigstring.t * 'a t -> ('a, public) key

let equal : type a b. (a, b) key -> (a, b) key -> bool = fun k1 k2 ->
  match k1, k2 with
  | Sk (sk, _), Sk (sk2, _) -> Bigstring.equal sk sk2
  | Pk (pk, c), Pk (pk2, _) ->
      let len = compressed_size c in
      let cpk = Bigstring.create len in
      let cpk2 = Bigstring.create len in
      compress pk cpk (to_curve c) ;
      compress pk2 cpk2 (to_curve c) ;
      Bigstring.equal cpk cpk2

let neuterize : type a b. (a, b) key -> (a, public) key = function
  | Pk (pk, curve) -> Pk (pk, curve)
  | Sk (sk, curve) ->
      let pk = Bigstring.create (pk_size curve) in
      let pk_computed_ok = pk_of_sk sk pk (to_curve curve) in
      let pk_is_valid = valid_pk pk (to_curve curve) in
      if not pk_computed_ok && pk_is_valid then
        invalid_arg "Uecc.neuterize" ;
      Pk (pk, curve)

let pk_of_bytes :
  type a. a t -> Bigstring.t ->
  ((a, public) key) option = fun curve buf ->
  match Bigstring.length buf with
  | len when len = compressed_size curve ->
      let c = to_curve curve in
      let pk = Bigstring.create (pk_size curve) in
      decompress buf pk c ;
      if valid_pk pk c then Some (Pk (pk, curve))
      else None
  | len when len = pk_size curve + 1 ->
      let c = to_curve curve in
      let pk = Bigstring.create (pk_size curve) in
      Bigstring.blit buf 1 pk 0 (len - 1) ;
      if Bigstring.get buf 0 = '\004' && valid_pk pk c then
        Some (Pk (pk, curve))
      else None
  | _ -> None

let sk_of_bytes :
  type a. a t -> Bigstring.t ->
  ((a, secret) key * (a, public) key) option = fun curve buf ->
  if Bigstring.length buf <> sk_size curve then None
  else
    let sk = Sk (Bigstring.copy buf, curve) in
    try
      let pk = neuterize sk in
      Some (sk, pk)
    with _ -> None

let to_bytes :
  type a b. ?compress:bool -> (a, b) key -> Bigstring.t =
  fun ?compress:(comp=true) -> function
    | Sk (sk, _) -> Bigstring.copy sk
    | Pk (pk, c) ->
        if comp then
          let buf = Bigstring.create (compressed_size c) in
          compress pk buf (to_curve c) ;
          buf
        else
          let len = pk_size c in
          let buf = Bigstring.create (len + 1) in
          Bigstring.set buf 0 '\004' ;
          Bigstring.blit pk 0 buf 1 len ;
          buf

let write_key :
  type a b. ?compress:bool -> Bigstring.t -> (a, b) key -> int =
  fun ?compress:(comp=true) buf -> function
    | Sk (sk, _) ->
        let len = Bigstring.length sk in
        Bigstring.blit sk 0 buf 0 len ;
        len
    | Pk (pk, c) ->
        if comp then begin
          compress pk buf (to_curve c) ;
          compressed_size c
        end
        else
          let len = Bigstring.length pk in
          Bigstring.set buf 0 '\004' ;
          Bigstring.blit pk 0 buf 1 len ;
          len + 1

let keypair :
  type a. a t -> ((a, secret) key * (a, public) key) option = fun t ->
  let sk = Bigstring.create (sk_size t) in
  let pk = Bigstring.create (pk_size t) in
  match keypair pk sk (to_curve t) with
  | true -> Some (Sk (sk, t), Pk (pk, t))
  | false -> None

external dh :
  Bigstring.t -> Bigstring.t -> Bigstring.t -> curve -> bool =
  "uECC_shared_secret_stub" [@@noalloc]

let write_dh (Sk (sk, c)) (Pk (pk, _)) buf =
  let secret_len = pk_size c / 2 in
  if Bigstring.length buf < secret_len then 0
  else
    match dh pk sk buf (to_curve c) with
    | true -> secret_len
    | false -> 0

let dh (Sk (sk, c)) (Pk (pk, _)) =
  let secret = Bigstring.create (pk_size c / 2) in
  match dh pk sk secret (to_curve c) with
  | true -> Some secret
  | false -> None

(* external sign :
 *   Bigstring.t -> Bigstring.t -> Bigstring.t -> curve -> bool =
 *   "uECC_sign_stub" [@@noalloc] *)

external verify :
  Bigstring.t -> Bigstring.t -> Bigstring.t -> curve -> bool =
  "uECC_verify_stub" [@@noalloc]

let write_sign (Sk (_sk, _c)) _buf ~msg:_ =
  failwith "Not implemented"
(* if Bigstring.length buf < pk_size c then 0
 * else
 *   match sign sk msg buf (to_curve c) with
 *   | true -> pk_size c
 *   | false -> 0 *)

let sign (Sk (_sk, _c)) _msg =
  failwith "Not implemented"
(* let signature = Bigstring.create (pk_size c) in
 * match sign sk msg signature (to_curve c) with
 * | true -> Some signature
 * | false -> None *)

let verify (Pk (pk, c)) ~msg ~signature =
  if Bigstring.length signature <> pk_size c then false
  else verify pk msg signature (to_curve c)

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
