(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** [Some (f x)] if input is [Some x], or [None] if it's [None] **)
val map: f:('a -> 'b) -> 'a option -> 'b option

(** [(f x)] if input is [Some x], or [None] if it's [None] **)
val apply: f:('a -> 'b option) -> 'a option -> 'b option

(** Call [(f x)] if input is [Some x], noop if it's [None] **)
val iter: f:('a -> unit) -> 'a option -> unit

(** [x] if input is [Some x], default if it's [None] **)
val unopt: default:'a -> 'a option -> 'a

(** [unopt_map f d x] is [y] if [x] is [Some y], [d] if [x] is [None] **)
val unopt_map: f:('a -> 'b) -> default:'b -> 'a option -> 'b

(** First input of form [Some x], or [None] if none **)
val first_some: 'a option -> 'a option -> 'a option

