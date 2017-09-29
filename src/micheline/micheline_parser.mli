(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

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

val tokenize : string -> token list tzresult

type node = (location, string) Micheline.node

(** Beginning of a sequence of consecutive primitives *)
val min_point : node list -> point

(** End of a sequence of consecutive primitives *)
val max_point : node list -> point

val node_encoding : node Data_encoding.encoding

type error += Invalid_utf8_sequence of point * string
type error += Unexpected_character of point * string
type error += Undefined_escape_sequence of point * string
type error += Missing_break_after_number of point
type error += Unterminated_string of location
type error += Unterminated_integer of location
type error += Unterminated_comment of location
type error += Unclosed of token
type error += Unexpected of token
type error += Extra of token
type error += Misaligned of node
type error += Empty

val parse_toplevel : ?check:bool -> token list -> node list tzresult

val parse_expression : ?check:bool -> token list -> node tzresult

val print_location : Format.formatter -> location -> unit

val print_point : Format.formatter -> point -> unit
