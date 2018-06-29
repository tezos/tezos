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

open Error_monad

type 'a parsing_result = 'a * error list

val no_parsing_error : 'a parsing_result -> 'a tzresult

type point =
  { point : int ;
    byte : int ;
    line : int ;
    column : int }

val point_zero : point

type location =
  { start : point ;
    stop : point }

val location_zero : location

val point_encoding : point Data_encoding.encoding

val location_encoding : location Data_encoding.encoding

type token_value =
  | String of string
  | Bytes of string
  | Int of string
  | Ident of string
  | Annot of string
  | Comment of string
  | Eol_comment of string
  | Semi
  | Open_paren | Close_paren
  | Open_brace | Close_brace

type token =
  { token : token_value ;
    loc : location }

val tokenize : string -> token list parsing_result

type node = (location, string) Micheline.node

(** Beginning of a sequence of consecutive primitives *)
val min_point : node list -> point

(** End of a sequence of consecutive primitives *)
val max_point : node list -> point

val max_annot_length : int

val node_encoding : node Data_encoding.encoding

type error += Invalid_utf8_sequence of point * string
type error += Unexpected_character of point * string
type error += Undefined_escape_sequence of point * string
type error += Missing_break_after_number of point
type error += Unterminated_string of location
type error += Unterminated_integer of location
type error += Odd_lengthed_bytes of location
type error += Unterminated_comment of location
type error += Unclosed of token
type error += Unexpected of token
type error += Extra of token
type error += Misaligned of node
type error += Empty
type error += Annotation_length of location

val parse_toplevel : ?check:bool -> token list -> node list parsing_result

val parse_expression : ?check:bool -> token list -> node parsing_result

val print_location : Format.formatter -> location -> unit

val print_point : Format.formatter -> point -> unit
