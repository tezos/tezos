(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
  hash: Operation_hash.t ;
  shell: Operation.shell_header ;
  contents: proto_operation ;
  signature: Ed25519.Signature.t option ;
}

and proto_operation =
  | Anonymous_operations of anonymous_operation list
  | Sourced_operations of sourced_operations

and anonymous_operation =
  | Seed_nonce_revelation of {
      level: Raw_level_repr.t ;
      nonce: Seed_repr.nonce ;
    }
  | Faucet of {
      id: Ed25519.Public_key_hash.t ;
      nonce: MBytes.t ;
    }

and sourced_operations =
  | Manager_operations of {
      source: Contract_repr.contract ;
      public_key: Ed25519.Public_key.t option ;
      fee: Tez_repr.tez ;
      counter: counter ;
      operations: manager_operation list ;
    }
  | Delegate_operations of {
      source: Ed25519.Public_key.t ;
      operations: delegate_operation list ;
    }
  | Dictator_operation of dictator_operation

and manager_operation =
  | Transaction of {
      amount: Tez_repr.tez ;
      parameters: Script_repr.expr option ;
      destination: Contract_repr.contract ;
    }
  | Origination of {
      manager: Ed25519.Public_key_hash.t ;
      delegate: Ed25519.Public_key_hash.t option ;
      script: Script_repr.t option ;
      spendable: bool ;
      delegatable: bool ;
      credit: Tez_repr.tez ;
    }
  | Delegation of Ed25519.Public_key_hash.t option

and delegate_operation =
  | Endorsement of {
      block: Block_hash.t ;
      slot: int ;
    }
  | Proposals of {
      period: Voting_period_repr.t ;
      proposals: Protocol_hash.t list ;
    }
  | Ballot of {
      period: Voting_period_repr.t ;
      proposal: Protocol_hash.t ;
      ballot: Vote_repr.ballot ;
    }

and dictator_operation =
  | Activate of Protocol_hash.t
  | Activate_testnet of Protocol_hash.t

and counter = Int32.t

type error += Cannot_parse_operation (* `Branch *)

val encoding: operation Data_encoding.t

val parse:
  Operation_hash.t -> Operation.t -> operation tzresult

val parse_proto:
  MBytes.t ->
  (proto_operation * Ed25519.Signature.t option) tzresult Lwt.t

type error += Missing_signature (* `Permanent *)
type error += Invalid_signature (* `Permanent *)

val check_signature:
  Ed25519.Public_key.t -> operation -> unit tzresult Lwt.t

val forge: Operation.shell_header -> proto_operation -> MBytes.t

val proto_operation_encoding:
  proto_operation Data_encoding.t

val unsigned_operation_encoding:
  (Operation.shell_header * proto_operation) Data_encoding.t

val max_operation_data_length: int
