(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val map: f:('a -> 'b) -> 'a option -> 'b option

val apply: f:('a -> 'b option) -> 'a option -> 'b option

val iter: f:('a -> unit) -> 'a option -> unit

val unopt: default:'a -> 'a option -> 'a

val unopt_map: f:('a -> 'b) -> default:'b -> 'a option -> 'b

val first_some: 'a option -> 'a option -> 'a option

val try_with : (unit -> 'a) -> 'a option

val some : 'a -> 'a option
