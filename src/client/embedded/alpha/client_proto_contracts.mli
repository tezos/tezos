(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module RawContractAlias :
  Client_aliases.Alias with type t = Contract.t

module ContractAlias : sig
  val get_contract:
    Client_commands.context ->
    string -> (string * Contract.t) tzresult Lwt.t
  val alias_param:
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val destination_param:
    ?name:string ->
    ?desc:string ->
    ('a, Client_commands.context, 'ret) Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a, Client_commands.context, 'ret) Cli_entries.params
  val rev_find:
    Client_commands.context ->
    Contract.t -> string option tzresult Lwt.t
  val name:
    Client_commands.context ->
    Contract.t -> string tzresult Lwt.t
end

val list_contracts:
  Client_commands.context ->
  (string * string * Contract.t) list tzresult Lwt.t

val get_manager:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val get_delegate:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val check_public_key :
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  ?src_pk:public_key ->
  public_key_hash ->
  public_key option tzresult Lwt.t

val commands: unit -> Client_commands.command list
