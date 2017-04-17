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

  val get_first_priority :
    ?max_priority:int ->
    Raw_level.t ->
    Account.t ->
    Client_proto_rpcs.block ->
    int tzresult Lwt.t
    (** [get_first_priority ?max_prio level account block] is the
        best (first) mining priority on [block] for [account] at
        [level]. *)

  val mine_stamp :
    Client_proto_rpcs.block ->
    secret_key ->
    Updater.shell_block_header ->
    int ->
    Nonce_hash.t ->
    MBytes.t tzresult Lwt.t

  val inject_block :
    Client_node_rpcs.Blocks.block ->
    ?force:bool ->
    ?proto_level:int ->
    priority:int ->
    timestamp:Time.t ->
    fitness:Fitness.t ->
    seed_nonce:Nonce.nonce ->
    src_sk:secret_key ->
    Operation_hash.t list -> Block_hash.t tzresult Lwt.t

  val mine :
    ?force:bool ->
    ?operations:Operation_hash.t list ->
    ?fitness_gap:int ->
    ?proto_level:int ->
    Account.t ->
    Client_node_rpcs.Blocks.block ->
    Block_hash.t tzresult Lwt.t

  val endorsement_reward :
    Account.t ->
    Client_node_rpcs.Blocks.block ->
    int64 tzresult Lwt.t
end

module Endorse : sig

  val endorse :
    ?force:bool ->
    ?slot:int ->
    Account.t ->
    Client_alpha.Client_proto_rpcs.block ->
    Operation_hash.t tzresult Lwt.t

  val endorsers_list :
    Client_alpha.Client_proto_rpcs.block ->
    Account.bootstrap_accounts ->
    Account.t array tzresult Lwt.t

  val endorsement_rights :
    ?max_priority:int ->
    Account.t ->
    Client_proto_rpcs.block ->
    Client_proto_rpcs.Helpers.Rights.endorsement_slot list tzresult Lwt.t

end

module Protocol : sig

  val inject_proposals :
    ?async:bool ->
    ?force:bool ->
    ?block:Client_node_rpcs.Blocks.block ->
    src:Account.t ->
    Hash.Protocol_hash.t list ->
    Hash.Operation_list_hash.elt tzresult Lwt.t

  val inject_ballot :
    ?async:bool ->
    ?force:bool ->
    ?block:Client_node_rpcs.Blocks.block ->
    src:Account.t ->
    proposal:Hash.Protocol_hash.t ->
    Vote.ballot ->
    Hash.Operation_list_hash.elt tzresult Lwt.t

end

module Assert : sig

  include module type of Assert

  val balance_equal:
    msg:string -> Account.t -> int64 -> unit tzresult Lwt.t
  val delegate_equal:
    msg:string -> Contract.t -> public_key_hash option -> unit tzresult Lwt.t

  val ecoproto_error:
    (Register_client_embedded_proto_alpha.Packed_protocol.error -> bool) ->
    Error_monad.error -> bool

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

  val invalid_endorsement_slot : msg:string -> 'a tzresult -> unit

  val check_protocol :
    ?msg:string -> block:Client_node_rpcs.Blocks.block ->
    Hash.Protocol_hash.t -> unit tzresult Lwt.t

end

val rpc_config: Client_rpcs.config

val display_level: Client_proto_rpcs.block -> unit tzresult Lwt.t
