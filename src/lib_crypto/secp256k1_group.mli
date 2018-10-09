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

(** Type for the group of integers modulo the order of the curve ℤ/pℤ *)
module type SCALAR_SIG = sig

  (** Element of the scalar group *)
  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t

  val zero : t
  val one : t
  val of_Z : Z.t -> t
  val to_Z : t -> Z.t
  val of_int : int -> t
  val add: t -> t -> t
  val mul: t -> t -> t
  val negate: t -> t
  val sub: t -> t -> t
  val of_bits_exn: string -> t
  val to_bits: t -> string
  val inverse: t -> t option

  (** Modular exponentiation*)
  val pow: t -> Z.t -> t
  val equal: t -> t -> bool
end

module Group : sig

  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t

  val order: Z.t
  module Scalar : SCALAR_SIG
  val e: t
  val g : t
  val h : t
  val of_coordinates: x:Z.t -> y:Z.t  -> t
  val of_bits_exn: string -> t
  val to_bits: t -> string

  val mul: Scalar.t -> t -> t
  val (+): t -> t -> t
  val (-): t -> t -> t
  val (=): t -> t -> bool

end
