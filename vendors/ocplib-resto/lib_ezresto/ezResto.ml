(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)


open Resto
module Service = Resto.MakeService(Resto_json.Encoding)
open Service

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]
module Arg = Arg
module Path = struct
  type 'params t = (unit, 'params) Path.path
  type 'params path = (unit, 'params) Path.path
  let root = Path.root
  let add_suffix = Path.add_suffix
  let add_arg = Path.add_arg
  let (/) = add_suffix
  let (/:) = add_arg
end
module Query = Query
type ('meth, 'params, 'query, 'input, 'output, 'error) service =
  ('meth, unit, 'params, 'query, 'input, 'output, 'error) Service.t
let get_service = get_service
let post_service = post_service
let delete_service = delete_service
let put_service = put_service
let patch_service = patch_service
type 'input input = 'input Service.input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input
type 'input request = 'input Service.request = {
  meth: meth ;
  uri: Uri.t ;
  input: 'input input ;
}
let forge_request = forge_request
let query = query
let input_encoding = input_encoding
let output_encoding = output_encoding
let error_encoding = error_encoding
module Description = Resto.Description
type description_service =
  ([`GET], unit * string list, Description.request,
   unit, Json_schema.schema Description.directory, unit) service
let description_service ?description path =
  description_service ?description Json_encoding.empty path
