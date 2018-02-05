(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_shell
open Proto_alpha.Tezos_context

exception Unknown_protocol

(** Miscellaneous self-descriptive functions *)

val no_ops_hash : Operation_list_list_hash.t
val get_protocol : Protocol_hash.t -> (module Tezos_protocol_updater.Registred_protocol.T)
val get_shell_header :
  State.Block.t -> Tezos_base.Operation.shell_header
val get_block_header :
  State.Block.t -> Operation_list_list_hash.t ->
  Tezos_stdlib.MBytes.t list -> Context_hash.t ->
  Tezos_base.Time.t -> Block_header.shell_header
val find_account : Helpers_account.t list -> Ed25519.Public_key_hash.t -> Helpers_account.t
val get_dummy_tezos_context :
  Proto_alpha.Environment.Context.t -> Proto_alpha.Tezos_context.context Proto_alpha.Environment.Error_monad.tzresult Lwt.t
val read_file : string -> string

