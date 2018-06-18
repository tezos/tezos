(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val may: f:('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

val never_ending: unit -> 'a Lwt.t

val worker:
  string ->
  run:(unit -> unit Lwt.t) ->
  cancel:(unit -> unit Lwt.t) ->
  unit Lwt.t

val trigger: unit -> (unit -> unit) * (unit -> unit Lwt.t)
val sort: ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t


val unless: bool -> (unit -> unit Lwt.t) -> unit Lwt.t

