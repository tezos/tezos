(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Map : Map.S with type key = string

(** Splits a string on slashes, grouping multiple slashes, and
    ignoring slashes at the beginning and end of string. *)
val split_path: string -> string list

(** Splits a string on a delimier character, grouping multiple
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
