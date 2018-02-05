(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context

exception Tez_error

(** Common casts between Tezos_context types *)

val tez_of_int : int -> Tez.tez
val cents_of_int : int -> Tez.tez

(** Tez.(+?) with a top - level error instead *)
val tez_add : Tez.tez -> Tez.tez -> Tez.tez
val tez_add_int : Tez.tez -> int -> Tez.tez

(** Tez.(-?) with a top - level error instead *)
val tez_sub : Tez.tez -> Tez.tez -> Tez.tez
val tez_sub_int : Tez.tez -> int -> Tez.tez
val ctxt_of_tc : context -> Mem_context.t

