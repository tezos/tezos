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

type json_schema = Json_schema.schema

exception No_case_matched
exception Unexpected_tag of int
exception Duplicated_tag of int
exception Invalid_tag of int * [ `Uint8 | `Uint16 ]
exception Unexpected_enum of string * string list

type 'a t
type 'a encoding = 'a t

val classify : 'a encoding -> [ `Fixed of int | `Dynamic | `Variable ]

val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

val null : unit encoding
val empty : unit encoding
val unit : unit encoding
val constant : string -> unit encoding
val int8 : int encoding
val uint8 : int encoding
val int16 : int encoding
val uint16 : int encoding
val int31 : int encoding
val int32 : int32 encoding
val int64 : int64 encoding
val bool : bool encoding
val string : string encoding
val bytes : MBytes.t encoding
val float : float encoding
val option : 'a encoding -> 'a option encoding
val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding
val string_enum : (string * 'a) list -> 'a encoding

module Fixed : sig
  val string : int -> string encoding
  val bytes : int -> MBytes.t encoding
end

module Variable : sig
  val string : string encoding
  val bytes : MBytes.t encoding
  val array : 'a encoding -> 'a array encoding
  val list : 'a encoding -> 'a list encoding
end

val dynamic_size : 'a encoding -> 'a encoding

val json : json encoding
val json_schema : json_schema encoding

type 'a field
val req :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't field
val opt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val varopt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val dft :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't -> 't field

val obj1 :
  'f1 field -> 'f1 encoding
val obj2 :
  'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
val obj3 :
  'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
val obj4 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val obj5 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val obj6 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val obj7 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val obj8 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val obj9 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val obj10 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field -> 'f10 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val tup1 :
  'f1 encoding ->
  'f1 encoding
val tup2 :
  'f1 encoding -> 'f2 encoding ->
  ('f1 * 'f2) encoding
val tup3 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding ->
  ('f1 * 'f2 * 'f3) encoding
val tup4 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val tup5 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val tup6 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val tup7 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val tup8 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val tup9 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val tup10 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding -> 'f10 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding
val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

val array : 'a encoding -> 'a array encoding
val list : 'a encoding -> 'a list encoding

val assoc : 'a encoding -> (string * 'a) list encoding

type 't case
val case :
  ?tag:int ->
  'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case
val union :
  ?tag_size:[ `Uint8 | `Uint16 ] -> 't case list -> 't encoding

val describe :
  ?title:string -> ?description:string ->
  't encoding ->'t encoding

val def : string -> 'a encoding -> 'a encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) ->
  ?schema:Json_schema.schema ->
  'b encoding -> 'a encoding

val mu : string -> ('a encoding -> 'a encoding) -> 'a encoding

module Json : sig

  val convert : 'a encoding -> 'a Json_encoding.encoding

  val schema : 'a encoding -> json_schema
  val construct : 't encoding -> 't -> json
  val destruct : 't encoding -> json -> 't

  (** JSON Error *)

  type path = path_item list
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

end

module Binary : sig

  val length : 'a encoding -> 'a -> int
  val read : 'a encoding -> MBytes.t -> int -> int -> (int * 'a) option
  val write : 'a encoding -> 'a -> MBytes.t -> int -> int option
  val to_bytes : 'a encoding -> 'a -> MBytes.t
  val of_bytes : 'a encoding -> MBytes.t -> 'a option

  val fixed_length : 'a encoding -> int option
  val fixed_length_exn : 'a encoding -> int

end
