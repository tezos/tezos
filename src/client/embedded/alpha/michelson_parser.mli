(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Script_located_ir

exception Invalid_utf8_sequence of point * string
exception Unexpected_character of point * string
exception Undefined_escape_character of point * string
exception Missing_break_after_number of point
exception Unterminated_string of location
exception Unterminated_integer of location
exception Unterminated_comment of location

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

val tokenize : string -> token list

exception Unclosed of token
exception Unexpected of token
exception Extra of token
exception Misaligned of node
exception Empty

val parse_toplevel : ?expand:bool -> ?check:bool -> token list -> node list
val parse_expression : ?expand:bool -> ?check:bool -> token list -> node
