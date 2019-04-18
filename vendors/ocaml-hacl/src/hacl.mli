(* Copyright 2018 Vincent Bernardoff, Marco Stronati.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

module Rand : sig
  val write : Bigstring.t -> unit
  (** [write buf] writes random bytes on [buf]. *)

  val gen : int -> Bigstring.t
  (** [gen len] is a random buffer of length [len]. *)
end

module Hash : sig
  module type S = sig
    type state

    val bytes : int
    val blockbytes : int
    val statebytes : int

    (** Incremental Interface *)

    val init : unit -> state
    val update : state -> Bigstring.t -> unit
    val finish : state -> Bigstring.t

    (** Direct Interface *)

    val digest : Bigstring.t -> Bigstring.t

    module HMAC : sig
      val write :
        key:Bigstring.t -> msg:Bigstring.t -> Bigstring.t -> unit
      (** @raise [Invalid_argument] if argument is less than 32 bytes long *)

      val digest :
        key:Bigstring.t -> msg:Bigstring.t -> Bigstring.t
    end
  end

  module SHA256 : S
  module SHA512 : S
end

module Nonce : sig
  type t = Bigstring.t
  val bytes : int
  val gen : unit -> t
  val increment : ?step:int -> t -> t
  val of_bytes : Bigstring.t -> t option
  val of_bytes_exn : Bigstring.t -> t
end

module Secretbox : sig
  type key

  val keybytes : int
  val zerobytes : int
  val boxzerobytes : int

  val unsafe_of_bytes : Bigstring.t -> key
  (** @raise Invalid_argument if argument is not [keybytes] bytes long *)

  val blit_of_bytes : Bigstring.t -> int -> key
  (** @raise Invalid_argument if argument is not [keybytes] bytes long *)

  val genkey : unit -> key

  val box :
    key:key -> nonce:Bigstring.t ->
    msg:Bigstring.t -> cmsg:Bigstring.t -> unit

  val box_open :
    key:key -> nonce:Bigstring.t ->
    cmsg:Bigstring.t -> msg:Bigstring.t -> bool
end

type secret
type public

module Box : sig
  type combined
  type _ key

  val skbytes : int
  val pkbytes : int
  val ckbytes : int
  val zerobytes : int
  val boxzerobytes : int

  val equal : 'a key -> 'a key -> bool

  val unsafe_to_bytes : _ key -> Bigstring.t
  (** [unsafe_to_bytes k] is the internal [Bigstring.t] where the key
      is stored. DO NOT MODIFY. *)

  val blit_to_bytes : _ key -> ?pos:int -> Bigstring.t -> unit

  val unsafe_sk_of_bytes : Bigstring.t -> secret key
  (** @raise Invalid_argument if argument is not [skbytes] bytes long *)

  val unsafe_pk_of_bytes : Bigstring.t -> public key
  (** @raise Invalid_argument if argument is not [pkbytes] bytes long *)

  val unsafe_ck_of_bytes : Bigstring.t -> combined key
  (** @raise Invalid_argument if argument is not [ckbytes] bytes long *)

  val of_seed : ?pos:int -> Bigstring.t -> secret key
  (** @raise Invalid_argument if [pos] is outside the buffer or the buffer
      is less than [skbytes] bytes long *)

  val neuterize : secret key -> public key
  val keypair : unit -> public key * secret key
  val dh : public key -> secret key -> combined key

  val box :
    k:combined key -> nonce:Bigstring.t ->
    msg:Bigstring.t -> cmsg:Bigstring.t -> unit

  val box_open :
    k:combined key -> nonce:Bigstring.t ->
    cmsg:Bigstring.t -> msg:Bigstring.t -> bool
end

module Sign : sig
  type _ key

  val bytes : int
  val pkbytes : int
  val skbytes : int

  val equal : 'a key -> 'a key -> bool

  val unsafe_sk_of_bytes : Bigstring.t -> secret key
  (** @raise Invalid_argument if argument is less than [skbytes] bytes long *)

  val unsafe_pk_of_bytes : Bigstring.t -> public key
  (** @raise Invalid_argument if argument is less than [pkbytes] bytes long *)

  val unsafe_to_bytes : _ key -> Bigstring.t
  (** [unsafe_to_bytes k] is the internal [Bigstring.t] where the key
      is stored. DO NOT MODIFY. *)

  val blit_to_bytes : _ key -> ?pos:int -> Bigstring.t -> unit

  val neuterize : _ key -> public key
  val keypair : unit -> public key * secret key

  val sign :
    sk:secret key -> msg:Bigstring.t -> signature:Bigstring.t -> unit
  (** [sign sk msg buf] writes the signature of [msg] with [sk] at
      [buf].

      @raise Invalid_argument if [buf] is smaller than [bytes]
      bytes long. *)

  val verify :
    pk:public key -> msg:Bigstring.t -> signature:Bigstring.t -> bool
end
