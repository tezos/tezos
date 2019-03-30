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

type t =
  | Unaccounted
  | Limited of { remaining : Z.t }

val encoding : t Data_encoding.encoding
val pp : Format.formatter -> t -> unit

type cost

val cost_encoding : cost Data_encoding.encoding
val pp_cost : Format.formatter -> cost -> unit

type error += Block_quota_exceeded (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)

val consume : Z.t -> t -> cost -> (Z.t * t) tzresult
val check_enough : Z.t -> t -> cost -> unit tzresult

val free : cost
val step_cost : int -> cost
val alloc_cost : int -> cost
val alloc_bytes_cost : int -> cost
val alloc_mbytes_cost : int -> cost
val alloc_bits_cost : int -> cost
val read_bytes_cost : Z.t -> cost
val write_bytes_cost : Z.t -> cost

val ( *@ ) : int -> cost -> cost
val ( +@ ) : cost -> cost -> cost
