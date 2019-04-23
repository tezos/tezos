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

val init :
  ?exe:string ->
  ?vote:bool ->
  ?rpc_port:int ->
  unit -> (int * Block_hash.t) tzresult Lwt.t
(** [init ()] sets up the test environment, and return the PID of
    forked Tezos node and the block info of the block from where the
    tests will begin. *)

val level:
  Chain_services.chain * Block_services.block -> Alpha_context.Level.t tzresult Lwt.t

(** Calls the rpc service raw_context using the right rpc context *)
val rpc_raw_context : Block_services.block -> string list -> int ->
  Block_services.raw_context tzresult Lwt.t

module Account : sig

  type t = {
    alias : string ;
    sk : Signature.Secret_key.t ;
    pk : Signature.Public_key.t ;
    pkh : Signature.Public_key_hash.t ;
    contract : Contract.t ;
  }

  val encoding : t Data_encoding.t
  val pp_account : Format.formatter -> t -> unit
  val create : ?keys:(Signature.secret_key * public_key) -> string -> t
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
    ?fee_parameter:Injection.fee_parameter ->
    unit ->
    (Operation_hash.t * Contract.t list) tzresult Lwt.t

  val originate :
    ?block:Block_services.block ->
    ?delegate:public_key_hash ->
    ?fee: Tez.t ->
    src:t ->
    manager_pkh:public_key_hash ->
    balance: Tez.t ->
    ?fee_parameter:Injection.fee_parameter ->
    unit -> (Operation_hash.t * Contract.t) tzresult Lwt.t

  val set_delegate :
    ?block:Block_services.block ->
    ?fee: Tez.t ->
    contract:Contract.t ->
    manager_sk:Client_keys.sk_uri ->
    src_pk:public_key ->
    ?fee_parameter:Injection.fee_parameter ->
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
    Operation.packed list ->
    Block_hash.t tzresult Lwt.t

end

module Endorse : sig

  val endorse :
    Account.t ->
    Block_services.block ->
    Operation.packed tzresult Lwt.t

  val endorsers_list :
    Block_services.block ->
    Account.t array tzresult Lwt.t

  val endorsement_rights :
    Account.t ->
    Block_services.block ->
    (Raw_level.t * int) list tzresult Lwt.t

end

module Protocol : sig

  val proposals :
    ?block:Block_services.block ->
    src:Account.t ->
    Protocol_hash.t list ->
    Operation.packed tzresult Lwt.t

  val ballot :
    ?block:Block_services.block ->
    src:Account.t ->
    proposal:Protocol_hash.t ->
    Vote.ballot ->
    Operation.packed tzresult Lwt.t

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
    ?op:Operation.packed ->
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

  val non_delegatable : msg:string -> 'a tzresult -> unit

  (** Endorsement / baking assertions *)

  val check_protocol :
    ?msg:string -> block:Block_services.block ->
    Protocol_hash.t -> unit tzresult Lwt.t

  val check_voting_period_kind :
    ?msg:string -> block:Block_services.block ->
    Voting_period.kind -> unit tzresult Lwt.t

end

val display_level: Block_services.block -> unit tzresult Lwt.t

val endorsement_security_deposit: Block_services.block -> Tez.t tzresult Lwt.t
