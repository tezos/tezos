(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type M =
(* This module type lists the parameters you can give to the function [run]
   defined below; most calls use and will use the default value for this module
   type, which is module [Client_config] (client_config.ml).
   Another instance of this module type is in main_signer.ml *)
sig
  type t
  val global_options :
    unit -> (t, Client_context_unix.unix_full) Clic.options
  val parse_config_args :
    #Tezos_client_base.Client_context.full ->
    string list ->
    (Client_config.parsed_config_args * string list) tzresult Lwt.t
  val default_block : [> `Head of int ]
  val default_base_dir : string
  val other_registrations :
    (Client_config.Cfg_file.t -> (module Client_config.Remote_params) -> unit)
      option
  val clic_commands :
    base_dir:string ->
    config_commands:Tezos_client_base.Client_context.full Clic.command list ->
    builtin_commands:Tezos_client_base.Client_context.full Clic.command list ->
    other_commands:Tezos_client_base.Client_context.full Clic.command list ->
    require_auth:bool ->
    Tezos_client_base.Client_context.full Clic.command list
  val logger : RPC_client.logger option
end

val run :
  (module M) ->
  select_commands :
    (RPC_client.http_ctxt ->
     Client_config.cli_args ->
     Client_context.full Clic.command list tzresult Lwt.t) ->
  unit
