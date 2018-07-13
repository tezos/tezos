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

(* Tezos Protocol Implementation - Low level Repr. of Operations *)

module Kind : sig
  type seed_nonce_revelation = Seed_nonce_revelation_kind
  type double_endorsement_evidence = Double_endorsement_evidence_kind
  type double_baking_evidence = Double_baking_evidence_kind
  type activate_account = Activate_account_kind
  type endorsement = Endorsement_kind
  type proposals = Proposals_kind
  type ballot = Ballot_kind
  type reveal = Reveal_kind
  type transaction = Transaction_kind
  type origination = Origination_kind
  type delegation = Delegation_kind
  type 'a manager =
    | Reveal_manager_kind : reveal manager
    | Transaction_manager_kind : transaction manager
    | Origination_manager_kind : origination manager
    | Delegation_manager_kind : delegation manager

end

type raw = Operation.t = {
  shell: Operation.shell_header ;
  proto: MBytes.t ;
}

val raw_encoding: raw Data_encoding.t

type 'kind operation = {
  shell: Operation.shell_header ;
  protocol_data: 'kind protocol_data ;
}

and 'kind protocol_data = {
  contents: 'kind contents_list ;
  signature: Signature.t option ;
}

and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons : 'kind Kind.manager contents * 'rest Kind.manager contents_list ->
    (('kind * 'rest) Kind.manager ) contents_list

and _ contents =
  | Endorsement : {
      level: Raw_level_repr.t ;
    } -> Kind.endorsement contents
  | Seed_nonce_revelation : {
      level: Raw_level_repr.t ;
      nonce: Seed_repr.nonce ;
    } -> Kind.seed_nonce_revelation contents
  | Double_endorsement_evidence : {
      op1: Kind.endorsement operation ;
      op2: Kind.endorsement operation ;
    } -> Kind.double_endorsement_evidence contents
  | Double_baking_evidence : {
      bh1: Block_header_repr.t ;
      bh2: Block_header_repr.t ;
    } -> Kind.double_baking_evidence contents
  | Activate_account : {
      id: Ed25519.Public_key_hash.t ;
      activation_code: Blinded_public_key_hash.activation_code ;
    } -> Kind.activate_account contents
  | Proposals : {
      source: Signature.Public_key_hash.t ;
      period: Voting_period_repr.t ;
      proposals: Protocol_hash.t list ;
    } -> Kind.proposals contents
  | Ballot : {
      source: Signature.Public_key_hash.t ;
      period: Voting_period_repr.t ;
      proposal: Protocol_hash.t ;
      ballot: Vote_repr.ballot ;
    } -> Kind.ballot contents
  | Manager_operation : {
      source: Contract_repr.contract ;
      fee: Tez_repr.tez ;
      counter: counter ;
      operation: 'kind manager_operation ;
      gas_limit: Z.t;
      storage_limit: Z.t;
    } -> 'kind Kind.manager contents

and _ manager_operation =
  | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
  | Transaction : {
      amount: Tez_repr.tez ;
      parameters: Script_repr.lazy_expr option ;
      destination: Contract_repr.contract ;
    } -> Kind.transaction manager_operation
  | Origination : {
      manager: Signature.Public_key_hash.t ;
      delegate: Signature.Public_key_hash.t option ;
      script: Script_repr.t option ;
      spendable: bool ;
      delegatable: bool ;
      credit: Tez_repr.tez ;
      preorigination: Contract_repr.t option ;
    } -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option -> Kind.delegation manager_operation

and counter = Z.t

type 'kind internal_operation = {
  source: Contract_repr.contract ;
  operation: 'kind manager_operation ;
  nonce: int ;
}

type packed_manager_operation =
  | Manager : 'kind manager_operation -> packed_manager_operation

type packed_contents =
  | Contents : 'kind contents -> packed_contents

type packed_contents_list =
  | Contents_list : 'kind contents_list -> packed_contents_list

val of_list: packed_contents list -> packed_contents_list
val to_list: packed_contents_list -> packed_contents list

type packed_protocol_data =
  | Operation_data : 'kind protocol_data -> packed_protocol_data

type packed_operation = {
  shell: Operation.shell_header ;
  protocol_data: packed_protocol_data ;
}

val pack: 'kind operation -> packed_operation

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

val manager_kind: 'kind manager_operation -> 'kind Kind.manager

val encoding: packed_operation Data_encoding.t
val contents_encoding: packed_contents Data_encoding.t
val contents_list_encoding: packed_contents_list Data_encoding.t
val protocol_data_encoding: packed_protocol_data Data_encoding.t
val unsigned_operation_encoding: (Operation.shell_header * packed_contents_list) Data_encoding.t

val raw: _ operation -> raw

val hash_raw: raw -> Operation_hash.t
val hash: _ operation -> Operation_hash.t
val hash_packed: packed_operation -> Operation_hash.t

val acceptable_passes: packed_operation -> int list

type error += Missing_signature (* `Permanent *)
type error += Invalid_signature (* `Permanent *)

val check_signature:
  Signature.Public_key.t -> Chain_id.t -> _ operation -> unit tzresult Lwt.t
val check_signature_sync:
  Signature.Public_key.t -> Chain_id.t -> _ operation -> unit tzresult


val internal_operation_encoding:
  packed_internal_operation Data_encoding.t

type ('a, 'b) eq = Eq : ('a, 'a) eq
val equal: 'a operation -> 'b operation -> ('a, 'b) eq option

module Encoding : sig

  type 'b case =
      Case : { tag: int ;
               name: string ;
               encoding: 'a Data_encoding.t ;
               select: packed_contents -> 'b contents option ;
               proj: 'b contents -> 'a ;
               inj: 'a -> 'b contents } -> 'b case

  val endorsement_case: Kind.endorsement case
  val seed_nonce_revelation_case: Kind.seed_nonce_revelation case
  val double_endorsement_evidence_case: Kind.double_endorsement_evidence case
  val double_baking_evidence_case: Kind.double_baking_evidence case
  val activate_account_case: Kind.activate_account case
  val proposals_case: Kind.proposals case
  val ballot_case: Kind.ballot case
  val reveal_case: Kind.reveal Kind.manager case
  val transaction_case: Kind.transaction Kind.manager case
  val origination_case: Kind.origination Kind.manager case
  val delegation_case: Kind.delegation Kind.manager case

  module Manager_operations : sig

    type 'b case =
        MCase : { tag: int ;
                  name: string ;
                  encoding: 'a Data_encoding.t ;
                  select: packed_manager_operation -> 'kind manager_operation option ;
                  proj: 'kind manager_operation -> 'a ;
                  inj: 'a -> 'kind manager_operation } -> 'kind case

    val reveal_case: Kind.reveal case
    val transaction_case: Kind.transaction case
    val origination_case: Kind.origination case
    val delegation_case: Kind.delegation case

  end

end
