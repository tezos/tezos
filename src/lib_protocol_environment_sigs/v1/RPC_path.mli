(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('prefix, 'params) t
type ('prefix, 'params) path = ('prefix, 'params) t
type 'prefix context = ('prefix, 'prefix) path

val root: unit context
val open_root: 'a context

val add_suffix:
  ('prefix, 'params) path -> string -> ('prefix, 'params) path
val (/):
  ('prefix, 'params) path -> string -> ('prefix, 'params) path

val add_arg:
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path
val (/:):
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a) path

val add_final_args:
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path
val (/:*):
  ('prefix, 'params) path -> 'a RPC_arg.t -> ('prefix, 'params * 'a list) path
