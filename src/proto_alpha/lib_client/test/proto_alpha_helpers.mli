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

val init :
  ?exe:string ->
  ?sandbox:string ->
  ?rpc_port:int ->
  unit -> (int * Block_hash.t) tzresult Lwt.t
(** [init ()] sets up the test environment, and return the PID of
    forked Tezos node and the block info of the block from where the
    tests will begin. *)

val level : Block_services.block -> Alpha_context.Level.t tzresult Lwt.t

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
    ?block:Block_services.block ->
    ?fee: Tez.t ->
    account:t ->
    destination:Contract.t ->
    amount: Tez.t ->
    unit ->
    (Operation_hash.t * Contract.t list) tzresult Lwt.t

  val originate :
    ?block:Block_services.block ->
    ?delegate:public_key_hash ->
    ?fee: Tez.t ->
    src:t ->
    manager_pkh:public_key_hash ->
    balance: Tez.t ->
    unit -> (Operation_hash.t * Contract.t) tzresult Lwt.t

  val set_delegate :
    ?block:Block_services.block ->
    ?fee: Tez.t ->
    contract:Contract.t ->
    manager_sk:Client_keys.Secret_key_locator.t ->
    src_pk:public_key ->
    public_key_hash option ->
    Operation_hash.t tzresult Lwt.t

  val balance : ?block:Block_services.block -> t -> Tez.t tzresult Lwt.t

  val delegate :
    ?block:Block_services.block ->
    Contract.t ->
    public_key_hash option tzresult Lwt.t

end

module Baking : sig

  val bake:
    Block_services.block ->
    Account.t ->
    Operation.raw list ->
    Block_hash.t tzresult Lwt.t

  val endorsement_reward:
    Block_services.block -> int64 tzresult Lwt.t

end

module Endorse : sig

  val endorse :
    ?slot:int ->
    Account.t ->
    Block_services.block ->
    Operation.raw tzresult Lwt.t

  val endorsers_list :
    Block_services.block ->
    Account.t array tzresult Lwt.t

  val endorsement_rights :
    ?max_priority:int ->
    Account.t ->
    Block_services.block ->
    (Raw_level.t * int) list tzresult Lwt.t

end

module Protocol : sig

  val proposals :
    ?block:Block_services.block ->
    src:Account.t ->
    Protocol_hash.t list ->
    Operation.raw tzresult Lwt.t

  val ballot :
    ?block:Block_services.block ->
    src:Account.t ->
    proposal:Protocol_hash.t ->
    Vote.ballot ->
    Operation.raw tzresult Lwt.t

end

module Assert : sig

  val fail : string -> string -> string -> 'a

  val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a

  val equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit
  val is_none : ?msg:string -> 'a option -> unit
  val is_some : ?msg:string -> 'a option -> unit
  val equal_int : ?msg:string -> int -> int -> unit
  val equal_bool : ?msg:string -> bool -> bool -> unit

  val balance_equal:
    ?block:Block_services.block ->
    msg:string -> Account.t -> int64 -> unit tzresult Lwt.t
  val delegate_equal:
    ?block:Block_services.block ->
    msg:string -> Contract.t -> public_key_hash option -> unit tzresult Lwt.t

  val failed_to_preapply:
    msg:string ->
    ?op:Tezos_base.Operation.t ->
    (Alpha_environment.Error_monad.error ->
     bool) ->
    'a tzresult -> unit

  val ecoproto_error:
    (Alpha_environment.Error_monad.error -> bool) ->
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
  val inconsistent_public_key : msg:string -> 'a tzresult -> unit
  val missing_public_key : msg:string -> 'a tzresult -> unit

  (** Origination assertions *)

  val initial_amount_too_low : msg:string -> 'a tzresult -> unit
  val non_delegatable : msg:string -> 'a tzresult -> unit

  (** Endorsement / baking assertions *)

  val wrong_delegate : msg:string -> 'a tzresult -> unit

  val check_protocol :
    ?msg:string -> block:Block_services.block ->
    Protocol_hash.t -> unit tzresult Lwt.t

  val check_voting_period_kind :
    ?msg:string -> block:Block_services.block ->
    Voting_period.kind -> unit tzresult Lwt.t

end

val display_level: Block_services.block -> unit tzresult Lwt.t
