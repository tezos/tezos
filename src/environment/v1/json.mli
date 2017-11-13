(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** In memory JSON data *)
type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

(** Read a JSON document from a string. *)
val from_string : string -> (json, string) result

(** Write a JSON document to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)
val to_string : json -> string

(** Helpers for [Data_encoding] *)
val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a
val wrap_error : ('a -> 'b) -> 'a -> 'b
