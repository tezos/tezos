(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

(** Typed path argument. *)
module Arg : sig

  type 'a t = 'a Resto.Arg.arg
  type 'a arg = 'a t
  val make:
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit -> 'a arg

  type descr = Resto.Arg.descr = {
    name: string ;
    descr: string option ;
  }
  val descr: 'a arg -> descr

  val int: int arg
  val int32: int32 arg
  val int64: int64 arg
  val float: float arg

end

(** Parametrized path to services. *)
module Path : sig

  type 'params t = (unit, 'params) Resto.Path.path
  type 'params path = 'params t

  val root: unit path

  val add_suffix: 'params path -> string -> 'params path
  val (/): 'params path -> string -> 'params path

  val add_arg: 'params path -> 'a Arg.arg -> ('params * 'a) path
  val (/:): 'params path -> 'a Arg.arg -> ('params * 'a) path

end

module Query : sig

  type 'a t
  type 'a query = 'a t

  val empty: unit query

  type ('a, 'b) field
  val field:
    ?descr: string ->
    string -> 'a Arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field

  type ('a, 'b, 'c) open_query
  val query: 'b -> ('a, 'b, 'b) open_query
  val (|+):
    ('a, 'b, 'c -> 'd) open_query ->
    ('a, 'c) field -> ('a, 'b, 'd) open_query
  val seal: ('a, 'b, 'a) open_query -> 'a t

  type untyped = (string * string) list
  exception Invalid of string
  val parse: 'a query -> untyped -> 'a

end

(** Services. *)
type ('meth, 'params, 'query, 'input, 'output, 'error) service =
  ('meth, unit, 'params, 'query, 'input, 'output, 'error) Resto.MakeService(Resto_json.Encoding).service

val get_service:
  ?description: string ->
  query: 'query Query.t ->
  output: 'output Json_encoding.encoding ->
  error: 'error Json_encoding.encoding ->
  'params Path.t ->
  ([ `GET ], 'params, 'query, unit, 'output, 'error) service

val post_service:
  ?description: string ->
  query: 'query Query.t ->
  input: 'input Json_encoding.encoding ->
  output: 'output Json_encoding.encoding ->
  error: 'error Json_encoding.encoding ->
  'params Path.t ->
  ([ `POST ], 'params, 'query, 'input, 'output, 'error) service

val delete_service:
  ?description: string ->
  query: 'query Query.t ->
  output: 'output Json_encoding.encoding ->
  error: 'error Json_encoding.encoding ->
  'params Path.t ->
  ([ `DELETE ], 'params, 'query, unit, 'output, 'error) service


val put_service:
  ?description: string ->
  query: 'query Query.t ->
  input: 'input Json_encoding.encoding ->
  output: 'output Json_encoding.encoding ->
  error: 'error Json_encoding.encoding ->
  'params Path.t ->
  ([ `PUT ], 'params, 'query, 'input, 'output, 'error) service

val patch_service:
  ?description: string ->
  query: 'query Query.t ->
  input: 'input Json_encoding.encoding ->
  output: 'output Json_encoding.encoding ->
  error: 'error Json_encoding.encoding ->
  'params Path.t ->
  ([ `PATCH ], 'params, 'query, 'input, 'output, 'error) service

type 'input input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input

type 'input request = {
  meth: meth ;
  uri: Uri.t ;
  input: 'input input ;
}

val forge_request:
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  ?base:Uri.t -> 'params -> 'query -> 'input request

val query:
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'query Query.t

val input_encoding:
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'input input

val output_encoding:
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'output Json_encoding.encoding

val error_encoding:
  ('meth, 'params, 'query, 'input, 'output, 'error) service ->
  'error Json_encoding.encoding

module Description = Resto.Description

type description_service =
  ([`GET], unit * string list, Description.request,
   unit, Json_schema.schema Description.directory, unit) service

val description_service:
  ?description:string -> unit Path.path -> description_service

