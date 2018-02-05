(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** [Some (f x)] if input is [Some x], or [None] if it's [None] **)
val map: f:('a -> 'b) -> 'a option -> 'b option

(** [(f x)] if input is [Some x], or [None] if it's [None] **)
val apply: f:('a -> 'b option) -> 'a option -> 'b option

val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
val (>>|) : 'a option -> ('a -> 'b) -> 'b option

(** Call [(f x)] if input is [Some x], noop if it's [None] **)
val iter: f:('a -> unit) -> 'a option -> unit

(** [x] if input is [Some x], default if it's [None] **)
val unopt: default:'a -> 'a option -> 'a

(** [unopt_map f d x] is [y] if [x] is [Some y], [d] if [x] is [None] **)
val unopt_map: f:('a -> 'b) -> default:'b -> 'a option -> 'b

(** [unopt_exn exn x] is [y] if [x] is [Some y], or raises [exn] if [x] is [None] *)
val unopt_exn : exn -> 'a option -> 'a

(** First input of form [Some x], or [None] if none **)
val first_some: 'a option -> 'a option -> 'a option

(** [Some (f ())] if [f] does not raise, [None] otherwise *)
val try_with : (unit -> 'a) -> 'a option

(** Make an option of a value *)
val some : 'a -> 'a option
