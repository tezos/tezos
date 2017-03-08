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
(** This [context] allows the client {!command} handlers to work in
     various modes (command line, batch mode, web client, etc.) by
     abstracting some basic operations such as logging and reading
     configuration options. It is passed as parameter to the command
     handler when running a command, and must be transmitted to all
     basic operations, also making client commands reantrant. *)

val make_context : (string -> string -> unit Lwt.t) -> context
(** [make_context log_fun] builds a context whose logging callbacks
    call [log_fun section msg], and whose [error] function calls
    [Lwt.fail_with]. *)

val ignore_context : context
(** [ignore_context] is a context whose logging callbacks do nothing,
    and whose [error] function calls [Lwt.fail_with]. *)

type command = (context, unit) Cli_entries.command

exception Version_not_found

val register: Protocol_hash.t -> command list -> unit
val commands_for_version: Protocol_hash.t -> command list
val get_versions: unit -> (Protocol_hash.t * (command list)) list
