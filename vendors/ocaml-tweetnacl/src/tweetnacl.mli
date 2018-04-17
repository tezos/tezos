(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Rand : sig
  val gen : int -> Bigstring.t
  val write : Bigstring.t -> unit
end

module Hash : sig
  val sha512 : Bigstring.t -> Bigstring.t
end

module Nonce : sig
  type t
  val bytes : int
  val gen : unit -> t
  val increment : ?step:int -> t -> t
  val of_bytes : Bigstring.t -> t option
  val of_bytes_exn : Bigstring.t -> t
  val to_bytes : t -> Bigstring.t
end

module Secretbox : sig
  type key

  val keybytes : int
  val zerobytes : int
  val boxzerobytes : int

  val genkey : unit -> key
  val of_bytes : Bigstring.t -> key option
  val of_bytes_exn : Bigstring.t -> key

  val box : key:key -> nonce:Nonce.t -> msg:Bigstring.t -> Bigstring.t
  val box_open : key:key -> nonce:Nonce.t -> cmsg:Bigstring.t -> Bigstring.t option

  val box_noalloc : key:key -> nonce:Nonce.t -> msg:Bigstring.t -> unit
  val box_open_noalloc : key:key -> nonce:Nonce.t -> cmsg:Bigstring.t -> bool
end

module Box : sig
  type secret
  type public
  type combined

  type _ key

  val skbytes : int
  val pkbytes : int
  val beforenmbytes : int
  val zerobytes : int
  val boxzerobytes : int

  val equal : 'a key -> 'a key -> bool
  val to_bytes : _ key -> Bigstring.t
  val blit_to_bytes : _ key -> ?pos:int -> Bigstring.t -> unit

  val sk_of_bytes : Bigstring.t -> secret key option
  val pk_of_bytes : Bigstring.t -> public key option
  val ck_of_bytes : Bigstring.t -> combined key option

  val sk_of_bytes_exn : Bigstring.t -> secret key
  val pk_of_bytes_exn : Bigstring.t -> public key
  val ck_of_bytes_exn : Bigstring.t -> combined key

  val keypair : unit -> public key * secret key

  val box :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> msg:Bigstring.t -> Bigstring.t
  val box_open :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> cmsg:Bigstring.t -> Bigstring.t option

  val box_noalloc :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> msg:Bigstring.t -> unit
  val box_open_noalloc :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> cmsg:Bigstring.t -> bool

  val combine : public key -> secret key -> combined key
  val box_combined :
    k:combined key -> nonce:Nonce.t -> msg:Bigstring.t -> Bigstring.t
  val box_open_combined :
    k:combined key -> nonce:Nonce.t -> cmsg:Bigstring.t -> Bigstring.t option

  val box_combined_noalloc :
    k:combined key -> nonce:Nonce.t -> msg:Bigstring.t -> unit
  val box_open_combined_noalloc :
    k:combined key -> nonce:Nonce.t -> cmsg:Bigstring.t -> bool
end

module Sign : sig
  type secret
  type extended
  type public
  type _ key

  val bytes : int
  val pkbytes : int
  val skbytes : int
  val ekbytes : int
  val seedbytes : int

  val to_bytes : _ key -> Bigstring.t
  val blit_to_bytes : _ key -> ?pos:int -> Bigstring.t -> unit

  val sk_of_bytes : Bigstring.t -> secret key option
  val ek_of_bytes : Bigstring.t -> extended key option
  val pk_of_bytes : Bigstring.t -> public key option

  val sk_of_bytes_exn : Bigstring.t -> secret key
  val ek_of_bytes_exn : Bigstring.t -> extended key
  val pk_of_bytes_exn : Bigstring.t -> public key

  val keypair : ?seed:Bigstring.t -> unit -> public key * secret key
  val equal : 'a key -> 'a key -> bool

  val extended : secret key -> extended key
  val seed : secret key -> Bigstring.t
  val public : _ key -> public key

  val sign : key:secret key -> Bigstring.t -> Bigstring.t
  val sign_extended : key:extended key -> Bigstring.t -> Bigstring.t

  val detached : key:secret key -> Bigstring.t -> Bigstring.t
  val detached_extended : key:extended key -> Bigstring.t -> Bigstring.t

  val verify : key:public key -> Bigstring.t -> Bigstring.t option
  val verify_detached : key:public key -> signature:Bigstring.t -> Bigstring.t -> bool

  val add : public key -> public key -> public key
  val mult : public key -> Z.t -> public key
  val base : Z.t -> public key
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
