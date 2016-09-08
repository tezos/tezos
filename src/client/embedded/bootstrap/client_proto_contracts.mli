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
  val get_contract: string -> (string * Contract.t) Lwt.t
  val alias_param:
    ?n:string ->
    ?desc:string ->
    'a Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a) Cli_entries.params
  val destination_param:
    ?n:string ->
    ?desc:string ->
    'a Cli_entries.params ->
    (Lwt_io.file_name * Contract.t -> 'a) Cli_entries.params
  val rev_find: Contract.t -> string option Lwt.t
  val name: Contract.t -> string Lwt.t
end

val get_manager:
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val get_delegate:
  Client_proto_rpcs.block ->
  Contract.t ->
  public_key_hash tzresult Lwt.t

val check_public_key :
  Client_proto_rpcs.block ->
  ?src_pk:public_key ->
  public_key_hash ->
  public_key option tzresult Lwt.t

val commands: unit -> Cli_entries.command list
