(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

val string_of_meth: [< meth ] -> string
val meth_of_string: string -> [> meth ] option
val meth_encoding: meth Data_encoding.t

module MethMap = Resto.MethMap

type (+'m,'pr,'p,'q,'i,'o, 'e) raw =
  ('m,'pr,'p,'q,'i,'o, 'e) Resto.MakeService(RPC_encoding).t
  constraint 'meth = [< meth ]

type error = Error_monad.error list

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth ]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth ]

include (module type of struct include Resto.MakeService(RPC_encoding) end
    with type (+'m,'pr,'p,'q,'i,'o, 'e) t := ('m,'pr,'p,'q,'i,'o, 'e) raw
     and type (+'m,'pr,'p,'q,'i,'o, 'e) service := ('m,'pr,'p,'q,'i,'o, 'e) raw)

val get_service:
  ?description: string ->
  query: 'query RPC_query.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `GET ], 'prefix, 'params, 'query, unit, 'output) service

val post_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `POST ], 'prefix, 'params, 'query, 'input, 'output) service

val delete_service:
  ?description: string ->
  query:'query RPC_query.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `DELETE ], 'prefix, 'params, 'query, unit, 'output) service

val patch_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `PATCH ], 'prefix, 'params, 'query, 'input, 'output) service

val put_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `PUT ], 'prefix, 'params, 'query, 'input, 'output) service


(**/**)

val description_service:
  ([ `GET ], unit, unit * string list, Resto.Description.request,
   unit, RPC_encoding.schema Resto.Description.directory) service

val error_service:
  ([ `GET ], unit, unit, unit, unit, Json_schema.schema) service

val error_encoding: error Data_encoding.t
