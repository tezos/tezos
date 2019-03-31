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

open Proto_alpha
open Alpha_context
open Clic

module RawContractAlias :
  Client_aliases.Alias with type t = Contract.t

module ContractAlias : sig
  val get_contract:
    #Client_context.wallet ->
    string -> (string * Contract.t) tzresult Lwt.t
  val alias_param:
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) params ->
    (string * Contract.t -> 'a, 'wallet) params
  val destination_param:
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) params ->
    (string * Contract.t -> 'a, 'wallet) params
  val rev_find:
    #Client_context.wallet ->
    Contract.t -> string option tzresult Lwt.t
  val name:
    #Client_context.wallet ->
    Contract.t -> string tzresult Lwt.t
  val autocomplete: #Client_context.wallet -> string list tzresult Lwt.t
end

val list_contracts:
  #Client_context.wallet ->
  (string * string * RawContractAlias.t) list tzresult Lwt.t

val get_manager:
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val get_delegate:
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  public_key_hash option tzresult Lwt.t
