(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val make_context :
  ?base_dir:string ->
  ?block:Block_services.block ->
  ?rpc_config:RPC_client.config ->
  (string -> string -> unit Lwt.t) -> Client_context.full_context
(** [make_context ?config log_fun] builds a context whose logging
    callbacks call [log_fun section msg], and whose [error] function
    fails with [Failure] and the given message. If not passed,
    [config] is {!default_cfg}. *)

val ignore_context : Client_context.full_context
(** [ignore_context] is a context whose logging callbacks do nothing,
    and whose [error] function calls [Lwt.fail_with]. *)

val default_log : base_dir:string -> string -> string -> unit Lwt.t
val default_base_dir : string
val default_block : Block_services.block
