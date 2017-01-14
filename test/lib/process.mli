(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
exception Exited of int

val detach: ?prefix:string -> (unit -> unit Lwt.t) -> unit Lwt.t
val handle_error: (unit -> (unit, error list) result Lwt.t) -> unit Lwt.t
val wait: unit Lwt.t list -> unit Lwt.t
