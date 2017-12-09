(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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

module MethMap : Map.S with type key = meth

type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t
  constraint 'meth = [< meth ]
type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t

val query:
  ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
  'query RPC_query.t

type _ input =
  | No_input : unit input
  | Input : 'input Data_encoding.t -> 'input input

val input_encoding:
  ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
  'input input

val output_encoding:
  ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
  'output Data_encoding.t

val error_encoding:
  ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
  'error Data_encoding.t

val prefix:
  ('prefix, 'inner_prefix) RPC_path.t ->
  ('meth, 'inner_prefix, 'params, 'query,
   'input, 'output, 'error) service ->
  ('meth, 'prefix, 'params,
   'query, 'input, 'output, 'error) service

val map:
  ('a -> 'b) ->
  ('b -> 'a) ->
  ('meth, 'pr, 'a, 'q, 'i, 'o, 'e) service ->
  ('meth, 'pr, 'b, 'q, 'i, 'o, 'e) service

val get_service:
  ?description: string ->
  query: 'query RPC_query.t ->
  output: 'output Data_encoding.t ->
  error: 'error Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `GET ], 'prefix, 'params, 'query, unit, 'output, 'error) service

val post_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  error: 'error Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `POST ], 'prefix, 'params, 'query, 'input, 'output, 'error) service

val delete_service:
  ?description: string ->
  query:'query RPC_query.t ->
  output: 'output Data_encoding.t ->
  error: 'error Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `DELETE ], 'prefix, 'params, 'query, unit, 'output, 'error) service

val patch_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  error: 'error Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `PATCH ], 'prefix, 'params, 'query, 'input, 'output, 'error) service

val put_service:
  ?description: string ->
  query:'query RPC_query.t ->
  input: 'input Data_encoding.t ->
  output: 'output Data_encoding.t ->
  error: 'error Data_encoding.t ->
  ('prefix, 'params) RPC_path.t ->
  ([ `PUT ], 'prefix, 'params, 'query, 'input, 'output, 'error) service
