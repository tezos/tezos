(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

(** Read a JSON document from a string. *)
val from_string : string -> (Data_encoding.json, string) result

(** Read a stream of JSON documents from a stream of strings.
    A single JSON document may be represented in multiple consecutive
    strings. But only the first document of a string is considered. *)
val from_stream : string Lwt_stream.t -> (Data_encoding.json, string) result Lwt_stream.t

(** Write a JSON document to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)
val to_string : ?minify:bool -> Data_encoding.json -> string

val pp : Format.formatter -> Data_encoding.json -> unit

(** Loads a JSON file in memory *)
val read_file : string -> Data_encoding.json tzresult Lwt.t

(** (Over)write a JSON file from in memory data *)
val write_file : string -> Data_encoding.json -> unit tzresult Lwt.t
