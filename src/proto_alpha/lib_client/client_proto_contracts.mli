(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

module RawContractAlias :
  Client_aliases.Alias with type t = Contract.t

module ContractAlias : sig
  val get_contract:
    #Client_commands.wallet ->
    string -> (string * Contract.t) tzresult Lwt.t
  val alias_param:
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_commands.wallet as 'wallet), 'ret) Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a, 'wallet, 'ret) Cli_entries.params
  val destination_param:
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_commands.wallet as 'wallet), 'ret) Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a, 'wallet, 'ret) Cli_entries.params
  val rev_find:
    #Client_commands.wallet ->
    Contract.t -> string option tzresult Lwt.t
  val name:
    #Client_commands.wallet ->
    Contract.t -> string tzresult Lwt.t
  val autocomplete: #Client_commands.wallet -> string list tzresult Lwt.t
end

val list_contracts:
  #Client_commands.wallet ->
  (string * string * RawContractAlias.t) list tzresult Lwt.t

val get_manager:
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val get_delegate:
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val check_public_key :
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  ?src_pk:public_key ->
  public_key_hash ->
  public_key option tzresult Lwt.t

module Contract_tags : module type of Client_tags.Tags (struct
    let name = "contract"
  end)
