(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
   unit, Json_schema.schema Resto.Description.directory) service

val error_service:
  ([ `POST ], unit, unit, unit, unit, Json_schema.schema) service

val error_encoding: error Data_encoding.t
