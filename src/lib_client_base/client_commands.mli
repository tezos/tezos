(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
  inherit RPC_context.json
end

class type full_context = object
  inherit logger_sig
  inherit wallet
  inherit RPC_context.json
  inherit block
end
(** The [full_context] allows the client {!command} handlers to work in
     various modes (command line, batch mode, web client, etc.) by
     abstracting some basic operations such as logging and reading
     configuration options. It is passed as parameter to the command
     handler when running a command, and must be transmitted to all
     basic operations, also making client commands reantrant. *)

class proxy_context : full_context -> full_context

type command = full_context Cli_entries.command

exception Version_not_found

val register: Protocol_hash.t -> command list -> unit
val commands_for_version: Protocol_hash.t -> command list
val get_versions: unit -> (Protocol_hash.t * (command list)) list

(** Have a command execute ignoring warnings.
    Default doc is ["Silence any warnings and some checks."]. *)
val force_switch : ?doc:string -> unit -> (bool, #full_context) Cli_entries.arg
