(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** HTTP methods. *)
type meth = [
  | `GET
  | `POST
  | `DELETE
  | `PUT
  | `PATCH
]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t
  constraint 'meth = [< meth ]
type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) t

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
