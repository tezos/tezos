(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Rand : sig
  val gen : int -> Cstruct.t
  val write : Cstruct.t -> unit
end

module Hash : sig
  val sha512 : Cstruct.t -> Cstruct.t
end

module Nonce : sig
  type t
  val bytes : int
  val gen : unit -> t
  val increment : ?step:int -> t -> t
  val of_cstruct : Cstruct.t -> t option
  val of_cstruct_exn : Cstruct.t -> t
  val to_cstruct : t -> Cstruct.t
end

module Secretbox : sig
  type key

  val keybytes : int
  val zerobytes : int
  val boxzerobytes : int

  val genkey : unit -> key
  val of_cstruct : Cstruct.t -> key option
  val of_cstruct_exn : Cstruct.t -> key

  val box : key:key -> nonce:Nonce.t -> msg:Cstruct.t -> Cstruct.t
  val box_open : key:key -> nonce:Nonce.t -> cmsg:Cstruct.t -> Cstruct.t option

  val box_noalloc : key:key -> nonce:Nonce.t -> msg:Cstruct.t -> unit
  val box_open_noalloc : key:key -> nonce:Nonce.t -> cmsg:Cstruct.t -> bool
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

  val pp : Format.formatter -> _ key -> unit
  val show : _ key -> string
  val equal : 'a key -> 'a key -> bool
  val to_cstruct : _ key -> Cstruct.t
  val blit_to_cstruct : _ key -> ?pos:int -> Cstruct.t -> unit

  val sk_of_cstruct : Cstruct.t -> secret key option
  val pk_of_cstruct : Cstruct.t -> public key option
  val ck_of_cstruct : Cstruct.t -> combined key option

  val sk_of_cstruct_exn : Cstruct.t -> secret key
  val pk_of_cstruct_exn : Cstruct.t -> public key
  val ck_of_cstruct_exn : Cstruct.t -> combined key

  val keypair : unit -> public key * secret key

  val box :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> msg:Cstruct.t -> Cstruct.t
  val box_open :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> cmsg:Cstruct.t -> Cstruct.t option

  val box_noalloc :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> msg:Cstruct.t -> unit
  val box_open_noalloc :
    pk:public key -> sk:secret key ->
    nonce:Nonce.t -> cmsg:Cstruct.t -> bool

  val combine : public key -> secret key -> combined key
  val box_combined :
    k:combined key -> nonce:Nonce.t -> msg:Cstruct.t -> Cstruct.t
  val box_open_combined :
    k:combined key -> nonce:Nonce.t -> cmsg:Cstruct.t -> Cstruct.t option

  val box_combined_noalloc :
    k:combined key -> nonce:Nonce.t -> msg:Cstruct.t -> unit
  val box_open_combined_noalloc :
    k:combined key -> nonce:Nonce.t -> cmsg:Cstruct.t -> bool
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

  val pp : Format.formatter -> _ key -> unit
  val show : _ key -> string
  val to_cstruct : _ key -> Cstruct.t
  val blit_to_cstruct : _ key -> ?pos:int -> Cstruct.t -> unit

  val sk_of_cstruct : Cstruct.t -> secret key option
  val ek_of_cstruct : Cstruct.t -> extended key option
  val pk_of_cstruct : Cstruct.t -> public key option

  val sk_of_cstruct_exn : Cstruct.t -> secret key
  val ek_of_cstruct_exn : Cstruct.t -> extended key
  val pk_of_cstruct_exn : Cstruct.t -> public key

  val keypair : ?seed:Cstruct.t -> unit -> public key * secret key
  val equal : 'a key -> 'a key -> bool

  val extended : secret key -> extended key
  val seed : secret key -> Cstruct.t
  val public : _ key -> public key

  val sign : key:secret key -> Cstruct.t -> Cstruct.t
  val sign_extended : key:extended key -> Cstruct.t -> Cstruct.t

  val detached : key:secret key -> Cstruct.t -> Cstruct.t
  val detached_extended : key:extended key -> Cstruct.t -> Cstruct.t

  val verify : key:public key -> Cstruct.t -> Cstruct.t option
  val verify_detached : key:public key -> signature:Cstruct.t -> Cstruct.t -> bool

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
