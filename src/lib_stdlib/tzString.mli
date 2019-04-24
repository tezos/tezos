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

module Set : Set.S with type elt = string
module Map : Map.S with type key = string

(** Splits a string on slashes, grouping multiple slashes, and
    ignoring slashes at the beginning and end of string. *)
val split_path: string -> string list

(** Splits a string on a delimiter character, grouping multiple
    delimiters, and ignoring delimiters at the beginning and end of
    string, if [limit] is passed, stops after [limit] split(s). *)
val split: char -> ?dup:bool -> ?limit: int -> string -> string list

(** [true] if input has prefix **)
val has_prefix: prefix:string -> string -> bool

(** Some (input with [prefix] removed), if string has [prefix], else [None] **)
val remove_prefix: prefix:string -> string -> string option

(** Length of common prefix of input strings *)
val common_prefix: string -> string -> int

(** Test whether a string contains a given character *)
val mem_char: string -> char -> bool

(** Functional iteration over the characters of a string from first to last *)
val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
