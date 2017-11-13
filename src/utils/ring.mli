(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Imperative Ring Buffer *)

type 'a t
val create : int -> 'a t
val add : 'a t -> 'a -> unit
val add_list : 'a t -> 'a list -> unit
val last : 'a t -> 'a option
exception Empty
val last_exn : 'a t -> 'a
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val elements : 'a t -> 'a list
