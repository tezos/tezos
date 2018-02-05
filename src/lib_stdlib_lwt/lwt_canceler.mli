(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
val create : unit -> t
val cancel : t -> unit Lwt.t
val cancelation : t -> unit Lwt.t
val on_cancel : t -> (unit -> unit Lwt.t) -> unit
val canceled : t -> bool
