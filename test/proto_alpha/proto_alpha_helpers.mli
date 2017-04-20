(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_embedded_proto_alpha
open Tezos_context
open Client_alpha

val init : ?sandbox:string -> unit -> (int * Block_hash.t) tzresult Lwt.t
(** [init ()] sets up the test environment, and return the PID of
    forked Tezos node and the block info of the block from where the
    tests will begin. *)

val level : Client_proto_rpcs.block -> Tezos_context.Level.t tzresult Lwt.t

module Account : sig

  type t = {
    alias : string ;
    sk : Ed25519.Secret_key.t ;
    pk : Ed25519.Public_key.t ;
    pkh : Ed25519.Public_key_hash.t ;
    contract : Contract.t ;
  }

  val encoding : t Data_encoding.t
  val pp_account : Format.formatter -> t -> unit
  val create : ?keys:(secret_key * public_key) -> string -> t
  (** [create ?keys alias] is an account with alias [alias]. If
      [?keys] is [None], a pair of keys will be minted. *)

  type destination = {
    alias : string ;
    contract : Contract.t ;
    pk : public_key ;
    pkh : public_key_hash ;
  }

  val destination_encoding : destination Data_encoding.t
  val pp_destination : Format.formatter -> destination -> unit
  val create_destination :
    alias:string ->
    contract:Contract.t ->
    pk:public_key -> destination
  (** [create_destination ~alias ~contract ~pk] is a destination
      contract [contract] with manager's publick key [pk]. *)

  type bootstrap_accounts = { b1 : t ; b2 : t ; b3 : t ; b4 : t ; b5 : t }

  val bootstrap_accounts : bootstrap_accounts
  (** The hardcoded bootstrap accounts. *)

  val transfer :
    ?block:Client_proto_rpcs.block ->
    ?fee:int64 ->
    account:t ->
    destination:Contract.t ->
    amount:int64 ->
    unit ->
    (Operation_hash.t * Contract.t list) tzresult Lwt.t

  val originate :
    ?block:Client_proto_rpcs.block ->
    ?delegate:public_key_hash ->
    ?fee:int64 ->
    src:t ->
    manager_pkh:public_key_hash ->
    spendable:bool ->
    balance:int64 ->
    unit -> (Operation_hash.t * Contract.t) tzresult Lwt.t

  val set_delegate :
    ?block:Client_proto_rpcs.block ->
    ?fee:int64 ->
    contract:Contract.t ->
    manager_sk:secret_key ->
    public_key_hash option ->
    Operation_hash.t tzresult Lwt.t

  val balance : ?block:Client_proto_rpcs.block -> t -> Tez.t tzresult Lwt.t

  val delegate :
    ?block:Client_proto_rpcs.block ->
    Contract.t ->
    public_key_hash option tzresult Lwt.t

end

module Mining : sig

  val mine:
    Client_node_rpcs.Blocks.block ->
    Account.t ->
    Operation.raw list ->
    Block_hash.t tzresult Lwt.t

  val endorsement_reward:
    Client_node_rpcs.Blocks.block -> int64 tzresult Lwt.t

end

module Endorse : sig

  val endorse :
    ?slot:int ->
    Account.t ->
    Client_alpha.Client_proto_rpcs.block ->
    Operation.raw tzresult Lwt.t

  val endorsers_list :
    Client_alpha.Client_proto_rpcs.block ->
    Account.t array tzresult Lwt.t

  val endorsement_rights :
    ?max_priority:int ->
    Account.t ->
    Client_proto_rpcs.block ->
    Client_proto_rpcs.Helpers.Rights.endorsement_slot list tzresult Lwt.t

end

module Protocol : sig

  val proposals :
    ?block:Client_node_rpcs.Blocks.block ->
    src:Account.t ->
    Protocol_hash.t list ->
    Operation.raw tzresult Lwt.t

  val ballot :
    ?block:Client_node_rpcs.Blocks.block ->
    src:Account.t ->
    proposal:Protocol_hash.t ->
    Vote.ballot ->
    Operation.raw tzresult Lwt.t

end

module Assert : sig

  include module type of Assert

  val balance_equal:
    ?block:Client_node_rpcs.Blocks.block ->
    msg:string -> Account.t -> int64 -> unit tzresult Lwt.t
  val delegate_equal:
    ?block:Client_node_rpcs.Blocks.block ->
    msg:string -> Contract.t -> public_key_hash option -> unit tzresult Lwt.t

  val failed_to_preapply:
    msg:string ->
    ?op:Client_node_rpcs.operation ->
    (Register_client_embedded_proto_alpha.Packed_protocol.error ->
     bool) ->
    'a tzresult -> unit

  val ecoproto_error:
    (Register_client_embedded_proto_alpha.Packed_protocol.error -> bool) ->
    error -> bool

  val generic_economic_error : msg:string -> 'a tzresult -> unit

  (** Transaction assertions *)

  val unknown_contract : msg:string -> 'a tzresult -> unit
  (** [unknown_contract ~msg result] raises if result is not a
      [Storage_error]. *)
  val non_existing_contract : msg:string -> 'a tzresult -> unit
  val balance_too_low : msg:string -> 'a tzresult -> unit
  val non_spendable : msg:string -> 'a tzresult -> unit
  val inconsistent_pkh : msg:string -> 'a tzresult -> unit

  (** Origination assertions *)

  val initial_amount_too_low : msg:string -> 'a tzresult -> unit
  val non_delegatable : msg:string -> 'a tzresult -> unit

  (** Endorsement / mining assertions *)

  val wrong_delegate : msg:string -> 'a tzresult -> unit

  val check_protocol :
    ?msg:string -> block:Client_node_rpcs.Blocks.block ->
    Protocol_hash.t -> unit tzresult Lwt.t

  val check_voting_period_kind :
    ?msg:string -> block:Client_node_rpcs.Blocks.block ->
    Voting_period.kind -> unit tzresult Lwt.t

end

val rpc_config: Client_rpcs.config

val display_level: Client_proto_rpcs.block -> unit tzresult Lwt.t
