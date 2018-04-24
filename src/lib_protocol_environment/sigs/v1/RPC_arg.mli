(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a t
type 'a arg = 'a t
val make:
  ?descr:string ->
  name:string ->
  destruct:(string -> ('a, string) result) ->
  construct:('a -> string) ->
  unit -> 'a arg

type descr = {
  name: string ;
  descr: string option ;
}
val descr: 'a arg -> descr

val int: int arg
val int32: int32 arg
val int64: int64 arg
val float: float arg
val string: string arg

val like: 'a arg -> ?descr:string -> string -> 'a arg

type ('a, 'b) eq = Eq : ('a, 'a) eq
val eq: 'a arg -> 'b arg -> ('a, 'b) eq option
