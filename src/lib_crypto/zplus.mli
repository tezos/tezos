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

val serialize: Z.t -> string
val deserialize: string -> Z.t

val leq: Z.t -> Z.t -> bool
(** Less than or equal. *)

val geq: Z.t -> Z.t -> bool
(** Greater than or equal. *)

val lt: Z.t -> Z.t -> bool
(** Less than (and not equal). *)

val gt: Z.t -> Z.t -> bool
(** Greater than (and not equal). *)

val (<=): Z.t -> Z.t -> bool
(** Less than or equal. *)

val (>=): Z.t -> Z.t -> bool
(** Greater than or equal. *)

val (<): Z.t -> Z.t -> bool
(** Less than (and not equal). *)

val (>): Z.t -> Z.t -> bool
(** Greater than (and not equal). *)

val zero: Z.t

val one: Z.t

val invert: Z.t -> Z.t -> Z.t option
(** Invert the first argument modulo the second. Returns
    none if there is no inverse *)
