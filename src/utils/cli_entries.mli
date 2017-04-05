(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

(* Tezos: a small Command Line Parsing library *)
(* Only used in the client. *)

type error += Command_not_found
type error += Bad_argument of int * string * string
type error += Command_failed of string

type ('a, 'arg, 'ret) params
type ('arg, 'ret) command

val param:
  name: string ->
  desc: string ->
  ('arg -> string -> 'a tzresult Lwt.t) ->
  ('b, 'arg, 'ret) params ->
  ('a -> 'b, 'arg, 'ret) params
val prefix:
  string ->
  ('a, 'arg, 'ret) params ->
  ('a, 'arg, 'ret) params
val prefixes:
  string list ->
  ('a, 'arg, 'ret) params ->
  ('a, 'arg, 'ret) params
val fixed:
  string list ->
  ('arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params
val stop:
  ('arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params
val seq:
  name: string ->
  desc: string ->
  ('arg -> string -> 'p tzresult Lwt.t) ->
  ('p list -> 'arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params

val string:
  name: string ->
  desc: string ->
  ('a, 'arg, 'ret) params ->
  (string -> 'a, 'arg, 'ret) params

val seq_of_param:
  (('arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params ->
   ('a -> 'arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params) ->
  ('a list -> 'arg -> 'ret tzresult Lwt.t, 'arg, 'ret) params

type group =
  { name : string ;
    title : string }

val command:
  ?group: group ->
  ?args: (Arg.key * Arg.spec * Arg.doc) list ->
  desc: string ->
  ('a, 'arg, 'ret) params -> 'a -> ('arg, 'ret) command

val usage:
  commands: ('arg, 'ret) command list ->
  (string * Arg.spec * string) list -> string

val inline_dispatch:
  ('arg, 'ret) command list -> unit ->
  [ `Arg of string | `End ] ->
  [ `Args of (Arg.key * Arg.spec * Arg.doc) list
  | `Fail of error list
  | `Nop
  | `Res of 'arg -> 'ret tzresult Lwt.t ]

val dispatch:
  ('arg, 'ret) command list -> 'arg -> string list -> 'ret tzresult Lwt.t
