(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
    (Lwt_io.file_name * Contract.t -> 'a, 'wallet) params
  val destination_param:
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) params ->
    (Lwt_io.file_name * Contract.t -> 'a, 'wallet) params
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
  Block_services.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val get_delegate:
  #Proto_alpha.rpc_context ->
  Block_services.block ->
  Contract.t ->
  public_key_hash option tzresult Lwt.t

module Contract_tags : module type of Client_tags.Tags (struct
    let name = "contract"
  end)
