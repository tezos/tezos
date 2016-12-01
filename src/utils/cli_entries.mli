(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos: a small Command Line Parsing library *)
(* Only used in the client. *)

exception Command_not_found
exception Bad_argument of int * string * string
exception Command_failed of string

type 'a params
type command

and desc = string
and group = string
and tag = string

val param:
  name: string ->
  desc: string ->
  (string -> 'a Lwt.t) -> 'b params -> ('a -> 'b) params
val prefix: string -> 'a params -> 'a params
val prefixes: string list -> 'a params -> 'a params
val string: string -> string -> 'a params -> (string -> 'a) params
val fixed: string list -> (unit -> unit Lwt.t) params
val stop: (unit -> unit Lwt.t) params
val seq:
  name: string ->
  desc: string ->
  (string -> 'p Lwt.t) ->
  ('p list -> unit -> unit Lwt.t) params

val seq_of_param:
  ((unit -> unit Lwt.t) params ->
   ('a -> unit -> unit Lwt.t) params) ->
  ('a list -> unit -> unit Lwt.t) params

val command:
  ?desc:desc ->
  ?tags:tag list ->
  ?group:group ->
  ?args:(Arg.key * Arg.spec * Arg.doc) list ->
  'a params -> 'a -> command

val register_group: group -> group -> unit
val register_tag: tag -> string -> unit

val usage:
  command list -> (string * Arg.spec * string) list -> string
val inline_dispatch:
  command list -> unit ->
  [ `Arg of string | `End ] ->
  [ `Args of (Arg.key * Arg.spec * Arg.doc) list
  | `Fail of exn
  | `Nop
  | `Res of unit -> unit Lwt.t ]

val dispatch:
  command list -> unit -> string list -> unit Lwt.t

val log_hook : (string -> string -> unit Lwt.t) option ref

val error : ('a, Format.formatter, unit, 'b Lwt.t) format4 -> 'a
val warning : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val message : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val answer : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val log : string -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
