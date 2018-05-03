(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)
module Encoding: sig
  type 'a t = 'a Encoding.t
  type 'a encoding = 'a t
  exception No_case_matched
  exception Unexpected_tag of int
  exception Duplicated_tag of int
  exception Invalid_tag of int * [ `Uint8 | `Uint16 ]
  exception Unexpected_enum of string * string list
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
  val ranged_int : int -> int -> int encoding
  val ranged_float : float -> float -> float encoding
  val bool : bool encoding
  val string : string encoding
  val bytes : MBytes.t encoding
  val float : float encoding
  val option : 'a encoding -> 'a option encoding
  val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding
  val string_enum : (string * 'a) list -> 'a encoding
  val is_obj : 'a encoding -> bool
  val is_tup : 'a encoding -> bool
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
  val delayed : (unit -> 'a encoding) -> 'a encoding
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
  type case_tag = Tag of int | Json_only
  val case :
    ?name:string ->
    case_tag ->
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
  val classify : 'a encoding -> [ `Fixed of int | `Dynamic | `Variable ]
  val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding
end = struct
  include Encoding
  let splitted ~json ~binary = raw_splitted ~json:(Json.convert json) ~binary
  let assoc enc =
    let json = Json_encoding.assoc (Json.convert enc) in
    let binary = list (tup2 string enc) in
    raw_splitted ~json ~binary
end

include Encoding

module Json: sig
  type json =
    [ `O of (string * json) list
    | `Bool of bool
    | `Float of float
    | `A of json list
    | `Null
    | `String of string ]
  type t = json
  type schema = Json_schema.schema
  val encoding : json Encoding.t
  val schema_encoding : schema Encoding.t
  val convert : 'a Encoding.t -> 'a Json_encoding.encoding
  val schema : 'a Encoding.t -> schema
  val construct : 't Encoding.t -> 't -> json
  val destruct : 't Encoding.t -> json -> 't
  type path = path_item list
  and path_item =
    [ `Field of string
    | `Index of int
    | `Star
    | `Next ]
  exception Cannot_destruct of (path * exn)
  exception Unexpected of string * string
  exception No_case_matched of exn list
  exception Bad_array_size of int * int
  exception Missing_field of string
  exception Unexpected_field of string
  val print_error :
    ?print_unknown: (Format.formatter -> exn -> unit) ->
    Format.formatter -> exn -> unit
  val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a
  val wrap_error : ('a -> 'b) -> 'a -> 'b
  val from_string : string -> (json, string) result
  val from_stream : string Lwt_stream.t -> (json, string) result Lwt_stream.t
  val to_string : ?minify:bool -> json -> string
  val pp : Format.formatter -> json -> unit
end = Json
module Bson: sig
  type bson = Json_repr_bson.bson
  type t = bson
  val construct : 't encoding -> 't -> bson
  val destruct : 't encoding -> bson -> 't
end = Bson
module Binary: sig
  val length : 'a encoding -> 'a -> int
  val read : 'a encoding -> MBytes.t -> int -> int -> (int * 'a) option
  val write : 'a encoding -> 'a -> MBytes.t -> int -> int option
  val to_bytes : 'a encoding -> 'a -> MBytes.t
  val of_bytes : 'a encoding -> MBytes.t -> 'a option
  val of_bytes_exn : 'a encoding -> MBytes.t -> 'a
  val to_bytes_list : ?copy_blocks:bool -> int  -> 'a encoding -> 'a -> MBytes.t list
  type 'a status =
    | Success of { res : 'a ; res_len : int ; remaining : MBytes.t list }
    | Await of (MBytes.t -> 'a status)
    | Error
  val read_stream_of_bytes : ?init:MBytes.t list -> 'a encoding -> 'a status
  val check_stream_of_bytes : ?init:MBytes.t list -> 'a encoding -> unit status
  val fixed_length : 'a encoding -> int option
  val fixed_length_exn : 'a encoding -> int
end = Binary

type json = Json.t
let json = Json.encoding
type json_schema = Json.schema
let json_schema = Json.schema_encoding
type bson = Bson.t
