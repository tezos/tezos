(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a t
type 'a query = 'a t

val empty: unit query

type ('a, 'b) field
val field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field
val opt_field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> ('b -> 'a option) -> ('b, 'a option) field
val flag:
  ?descr: string ->
  string -> ('b -> bool) -> ('b, bool) field
val multi_field:
  ?descr: string ->
  string -> 'a RPC_arg.t -> ('b -> 'a list) -> ('b, 'a list) field

type ('a, 'b, 'c) open_query
val query: 'b -> ('a, 'b, 'b) open_query
val (|+):
  ('a, 'b, 'c -> 'd) open_query ->
  ('a, 'c) field -> ('a, 'b, 'd) open_query
val seal: ('a, 'b, 'a) open_query -> 'a t

type untyped = (string * string) list
exception Invalid of string
val parse: 'a query -> untyped -> 'a
