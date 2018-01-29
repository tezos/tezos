(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

class type logger_sig = object
  method error : ('a, 'b) lwt_format -> 'a
  method warning : ('a, unit) lwt_format -> 'a
  method message : ('a, unit) lwt_format -> 'a
  method answer :  ('a, unit) lwt_format -> 'a
  method log : string -> ('a, unit) lwt_format -> 'a
end

val default_log : base_dir:string -> string -> string -> unit Lwt.t

class logger : (string -> string -> unit Lwt.t) -> logger_sig

class type wallet = object
  method load : string -> default:'a -> 'a Data_encoding.encoding -> 'a tzresult Lwt.t
  method write : string -> 'a -> 'a Data_encoding.encoding -> unit tzresult Lwt.t
end

class type block = object
  method block : Block_services.block
end

class type logging_wallet = object
  inherit logger_sig
  inherit wallet
end

class type logging_rpcs = object
  inherit logger_sig
  inherit Client_rpcs.ctxt
end

class type full_context = object
  inherit logger_sig
  inherit wallet
  inherit Client_rpcs.ctxt
  inherit block
end
(** The [full_context] allows the client {!command} handlers to work in
     various modes (command line, batch mode, web client, etc.) by
     abstracting some basic operations such as logging and reading
     configuration options. It is passed as parameter to the command
     handler when running a command, and must be transmitted to all
     basic operations, also making client commands reantrant. *)

val make_context :
  ?base_dir:string ->
  ?block:Block_services.block ->
  ?rpc_config:Client_rpcs.config ->
  (string -> string -> unit Lwt.t) -> full_context
(** [make_context ?config log_fun] builds a context whose logging
    callbacks call [log_fun section msg], and whose [error] function
    fails with [Failure] and the given message. If not passed,
    [config] is {!default_cfg}. *)

val ignore_context : full_context
(** [ignore_context] is a context whose logging callbacks do nothing,
    and whose [error] function calls [Lwt.fail_with]. *)

type command = (full_context, unit) Cli_entries.command

exception Version_not_found

val register: Protocol_hash.t -> command list -> unit
val commands_for_version: Protocol_hash.t -> command list
val get_versions: unit -> (Protocol_hash.t * (command list)) list

(** Have a command execute ignoring warnings.
    Default doc is ["Silence any warnings and some checks."]. *)
val force_switch : ?doc:string -> unit -> (bool, full_context) Cli_entries.arg

val default_base_dir : string
val default_block : Block_services.block
