(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

type context =
  { error : 'a 'b. ('a, 'b) lwt_format -> 'a ;
    warning : 'a. ('a, unit) lwt_format -> 'a ;
    message : 'a. ('a, unit) lwt_format -> 'a ;
    answer : 'a. ('a, unit) lwt_format -> 'a ;
    log : 'a. string -> ('a, unit) lwt_format -> 'a }

val make_context : (string -> string -> unit Lwt.t) -> context

val ignore_context : context

type command = (context, unit) Cli_entries.command

exception Version_not_found

val register: Protocol_hash.t -> command list -> unit
val commands_for_version: Protocol_hash.t -> command list
val get_versions: unit -> (Protocol_hash.t * (command list)) list
