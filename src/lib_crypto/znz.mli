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

(** Type for a module representing the ℤ/nℤ ring*)
module type ZN = sig

  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t

  val zero : t
  val one  : t
  val n : Z.t
  val (+)       : t -> t -> t
  val ( * )     : t -> t -> t
  val (-)       : t -> t -> t
  val (=)    : t -> t -> bool

  (** Converts an integer to a ring element *)
  val of_int    : int -> t

  (** Converts a Zarith integer to a ring element *)
  val of_Z      : Z.t -> t

  (** Provides an integer representation between 0 and n-1 of an element *)
  val to_Z      : t -> Z.t

  (** Converts a string of bytes to an integer modulo n, requires the string of
      byte to represent an integer between 0 and n-1 and checks the length of
      the string for sanity*)
  val of_bits_exn   : String.t -> t

  (** Converts a ring element to a byte representation *)
  val to_bits : t -> String.t

  (** Modular exponentiation *)
  val pow       : t -> Z.t -> t

  (** Returns the inverse of a in ℤ/nℤ, maybe *)
  val inv       : t -> t option

end


(** Type of a module wrapping an integer. *)
module type INT = sig
  val n : Z.t
end

(** Functor to build the ℤ/nℤ ring given n*)
module MakeZn : functor (N : INT) (B : sig val b58_prefix : string end) -> ZN
