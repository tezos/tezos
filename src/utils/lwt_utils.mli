(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val never_ending: 'a Lwt.t

val canceler : unit ->
  (unit -> unit Lwt.t) *
  (unit -> unit Lwt.t) *
  ((unit -> unit Lwt.t) -> unit)

val worker:
  ?safe:bool ->
  string ->
  run:(unit -> unit Lwt.t) ->
  cancel:(unit -> unit Lwt.t) ->
  unit Lwt.t

val trigger: unit -> (unit -> unit) * (unit -> unit Lwt.t)
val queue: unit -> ('a -> unit) * (unit -> 'a list Lwt.t)
val sort: ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t
