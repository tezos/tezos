(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val create: init:int -> alpha:float -> t
val destroy: t -> unit

val add: t -> int -> unit

val on_update: (unit -> unit) -> unit
val updated: unit Lwt_condition.t

type stat = {
  total: int64 ;
  average: int ;
}
val stat: t -> stat
