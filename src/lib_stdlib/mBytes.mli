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

(** Low-level byte array querying and manipulation.

    Default layout for numeric operations is big-endian.
    Little-endian operations in the LE submodule. **)

include module type of Bigstring
include Compare.S with type t := t

include EndianBigstring.EndianBigstringSig
module LE : EndianBigstring.EndianBigstringSig

val make : int -> char -> t
val of_hex : Hex.t -> t
val to_hex : t -> Hex.t
val pp_hex : Format.formatter -> t -> unit

(** [cut ?copy size bytes] cut [bytes] the in a list of successive
    chunks of length [size] at most.

    If [copy] is false (default), the blocks of the list
    can be garbage-collected only when all the blocks are
    unreachable (because of the 'optimized' implementation of
    [sub] used internally. *)
val cut: ?copy:bool -> int  -> t -> t list
