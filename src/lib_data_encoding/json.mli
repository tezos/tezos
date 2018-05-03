(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Type-safe serialization and deserialization of data structures. *)

(** In memory JSON data, compatible with [Ezjsonm]. *)
type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]
type t = json

type schema = Json_schema.schema


(** Create a {!Json_encoding.encoding} from an {encoding}. *)
val convert : 'a Encoding.t -> 'a Json_encoding.encoding

(** Generate a schema from an {!encoding}. *)
val schema : 'a Encoding.t -> schema

val encoding: json Encoding.t
val schema_encoding: schema Encoding.t


(** Construct a JSON object from an encoding. *)
val construct : 't Encoding.t -> 't -> json

(** Destruct a JSON object into a value.
    Fail with an exception if the JSON object and encoding do not match.. *)
val destruct : 't Encoding.t -> json -> 't

(** JSON Error. *)

type path = path_item list

(** A set of accessors that point to a location in a JSON object. *)
and path_item =
  [ `Field of string
  (** A field in an object. *)
  | `Index of int
  (** An index in an array. *)
  | `Star
  (** Any / every field or index. *)
  | `Next
    (** The next element after an array. *) ]

(** Exception raised by destructors, with the location in the original
    JSON structure and the specific error. *)
exception Cannot_destruct of (path * exn)

(** Unexpected kind of data encountered (w/ the expectation). *)
exception Unexpected of string * string

(** Some {!union} couldn't be destructed, w/ the reasons for each {!case}. *)
exception No_case_matched of exn list

(** Array of unexpected size encountered  (w/ the expectation). *)
exception Bad_array_size of int * int

(** Missing field in an object. *)
exception Missing_field of string

(** Supernumerary field in an object. *)
exception Unexpected_field of string

val print_error :
  ?print_unknown: (Format.formatter -> exn -> unit) ->
  Format.formatter -> exn -> unit

(** Helpers for writing encoders. *)
val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a
val wrap_error : ('a -> 'b) -> 'a -> 'b

(** Read a JSON document from a string. *)
val from_string : string -> (json, string) result

(** Read a stream of JSON documents from a stream of strings.
    A single JSON document may be represented in multiple consecutive
    strings. But only the first document of a string is considered. *)
val from_stream : string Lwt_stream.t -> (json, string) result Lwt_stream.t

(** Write a JSON document to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)
val to_string : ?minify:bool -> json -> string

val pp : Format.formatter -> json -> unit
