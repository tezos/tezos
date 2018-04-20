(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Low level Repr. of Operations *)

type raw = Operation.t = {
  shell: Operation.shell_header ;
  proto: MBytes.t ;
}

val raw_encoding: raw Data_encoding.t

type operation = {
  shell: Operation.shell_header ;
  protocol_data: protocol_data ;
}

and protocol_data = {
  contents: contents ;
  signature: Signature.t option ;
}

and contents =
  | Anonymous_operations of anonymous_operation list
  | Sourced_operation of sourced_operation

and anonymous_operation =
  | Seed_nonce_revelation of {
      level: Raw_level_repr.t ;
      nonce: Seed_repr.nonce ;
    }
  | Double_endorsement_evidence of {
      op1: operation ;
      op2: operation ;
    }
  | Double_baking_evidence of {
      bh1: Block_header_repr.t ;
      bh2: Block_header_repr.t ;
    }
  | Activation of {
      id: Ed25519.Public_key_hash.t ;
      secret: Blinded_public_key_hash.secret ;
    }

and sourced_operation =
  | Consensus_operation of consensus_operation
  | Amendment_operation of {
      source: Signature.Public_key_hash.t ;
      operation: amendment_operation ;
    }
  | Manager_operations of {
      source: Contract_repr.contract ;
      fee: Tez_repr.tez ;
      counter: counter ;
      operations: manager_operation list ;
      gas_limit: Z.t ;
      storage_limit: Int64.t;
    }
  | Dictator_operation of dictator_operation

and consensus_operation =
  | Endorsements of {
      block: Block_hash.t ;
      level: Raw_level_repr.t ;
      slots: int list ;
    }

and amendment_operation =
  | Proposals of {
      period: Voting_period_repr.t ;
      proposals: Protocol_hash.t list ;
    }
  | Ballot of {
      period: Voting_period_repr.t ;
      proposal: Protocol_hash.t ;
      ballot: Vote_repr.ballot ;
    }

and manager_operation =
  | Reveal of Signature.Public_key.t
  | Transaction of {
      amount: Tez_repr.tez ;
      parameters: Script_repr.lazy_expr option ;
      destination: Contract_repr.contract ;
    }
  | Origination of {
      manager: Signature.Public_key_hash.t ;
      delegate: Signature.Public_key_hash.t option ;
      script: Script_repr.t option ;
      spendable: bool ;
      delegatable: bool ;
      credit: Tez_repr.tez ;
      preorigination: Contract_repr.t option ;
    }
  | Delegation of Signature.Public_key_hash.t option

and dictator_operation =
  | Activate of Protocol_hash.t
  | Activate_testchain of Protocol_hash.t

and counter = Int32.t

val encoding: operation Data_encoding.t
val contents_encoding: contents Data_encoding.t
val protocol_data_encoding: protocol_data Data_encoding.t
val unsigned_operation_encoding: (Operation.shell_header * contents) Data_encoding.t

val hash_raw: raw -> Operation_hash.t
val hash: operation -> Operation_hash.t

val acceptable_passes: operation -> int list

type error += Missing_signature (* `Permanent *)
type error += Invalid_signature (* `Permanent *)


val check_signature:
  Signature.Public_key.t -> operation -> unit tzresult Lwt.t

type internal_operation = {
  source: Contract_repr.contract ;
  operation: manager_operation ;
  nonce: int ;
}

val internal_operation_encoding:
  internal_operation Data_encoding.t
