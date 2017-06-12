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

let raw_encoding = Operation.encoding

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

module Encoding = struct

  open Data_encoding

  let transaction_encoding =
    (obj4
       (req "kind" (constant "transaction"))
       (req "amount" Tez_repr.encoding)
       (req "destination" Contract_repr.encoding)
       (opt "parameters" Script_repr.expr_encoding))

  let transaction_case tag =
    case ~tag transaction_encoding
      (function
        | Transaction { amount ; destination ; parameters } ->
            Some ((), amount, destination, parameters)
        | _ -> None)
      (fun ((), amount, destination, parameters) ->
         Transaction { amount ; destination ; parameters })

  let origination_encoding =
    (obj7
       (req "kind" (constant "origination"))
       (req "managerPubkey" Ed25519.Public_key_hash.encoding)
       (req "balance" Tez_repr.encoding)
       (opt "spendable" bool)
       (opt "delegatable" bool)
       (opt "delegate" Ed25519.Public_key_hash.encoding)
       (opt "script" Script_repr.encoding))

  let origination_case tag =
    case ~tag origination_encoding
      (function
        | Origination { manager ; credit ; spendable ;
                        delegatable ; delegate ; script } ->
            Some ((), manager, credit, Some spendable,
                  Some delegatable, delegate, script)
        | _ -> None)
      (fun ((), manager, credit, spendable, delegatable, delegate, script) ->
         let delegatable =
           match delegatable with None -> true | Some b -> b in
         let spendable =
           match spendable with None -> true | Some b -> b in
         Origination
           {manager ; credit ; spendable ; delegatable ; delegate ; script })

  let delegation_encoding =
    (obj2
       (req "kind" (constant "delegation"))
       (opt "delegate" Ed25519.Public_key_hash.encoding))

  let delegation_case tag =
    case ~tag delegation_encoding
      (function Delegation key -> Some ((), key) | _ -> None)
      (fun ((), key) -> Delegation key)

  let manager_kind_encoding =
    (obj5
       (req "source" Contract_repr.encoding)
       (opt "public_key" Ed25519.Public_key.encoding)
       (req "fee" Tez_repr.encoding)
       (req "counter" int32)
       (req "operations"
          (list (union ~tag_size:`Uint8 [
               transaction_case 0 ;
               origination_case 1 ;
               delegation_case 2 ;
             ]))))

  let manager_kind_case tag =
    case ~tag manager_kind_encoding
      (function
        | Manager_operations { source; public_key ; fee ; counter ;operations } ->
            Some (source, public_key, fee, counter, operations)
        | _ -> None)
      (fun (source, public_key, fee, counter, operations) ->
         Manager_operations { source; public_key ; fee ; counter ; operations })

  let endorsement_encoding =
    (obj3
       (req "kind" (constant "endorsement"))
       (req "block" Block_hash.encoding)
       (req "slot" int31))

  let endorsement_case tag =
    case ~tag endorsement_encoding
      (function
        | Endorsement { block ; slot } ->
            Some ((), block, slot)
        | _ -> None)
      (fun ((), block, slot) ->
         Endorsement { block ; slot })

  let proposal_encoding =
    (obj3
       (req "kind" (constant "proposal"))
       (req "period" Voting_period_repr.encoding)
       (req "proposals" (list Protocol_hash.encoding)))

  let proposal_case tag =
    case ~tag proposal_encoding
      (function
        | Proposals { period ; proposals } ->
            Some ((), period, proposals)
        | _ -> None)
      (fun ((), period, proposals) ->
         Proposals { period ; proposals })

  let ballot_encoding =
    (obj4
       (req "kind" (constant "ballot"))
       (req "period" Voting_period_repr.encoding)
       (req "proposal" Protocol_hash.encoding)
       (req "ballot" Vote_repr.ballot_encoding))

  let ballot_case tag =
    case ~tag ballot_encoding
      (function
        | Ballot { period ; proposal ; ballot } ->
            Some ((), period, proposal, ballot)
        | _ -> None)
      (fun ((), period, proposal, ballot) ->
         Ballot { period ; proposal ; ballot })

  let delegate_kind_encoding =
    (obj2
       (req "source" Ed25519.Public_key.encoding)
       (req "operations"
          (list (union [
               endorsement_case 0 ;
               proposal_case 1 ;
               ballot_case 2 ;
             ]))))

  let delegate_kind_case tag =
    case ~tag delegate_kind_encoding
      (function
        | Delegate_operations { source ; operations } ->
            Some (source, operations)
        | _ -> None)
      (fun (source, operations) -> Delegate_operations { source ; operations })

  let dictator_kind_encoding =
    let mk_case name args =
      let open Data_encoding in
      conv
        (fun o -> ((), o))
        (fun ((), o) -> o)
        (merge_objs
           (obj1 (req "network" (constant name)))
           args) in
    let open Data_encoding in
    union ~tag_size:`Uint8 [
      case ~tag:0
        (mk_case "activate"
           (obj1 (req "hash" Protocol_hash.encoding)))
        (function (Activate hash) -> Some hash | _ -> None)
        (fun hash -> Activate hash) ;
      case ~tag:1
        (mk_case "activate_testnet"
           (obj1 (req "hash" Protocol_hash.encoding)))
        (function (Activate_testnet hash) -> Some hash | _ -> None)
        (fun hash -> Activate_testnet hash) ;
    ]

  let dictator_kind_case tag =
    case ~tag dictator_kind_encoding
      (function Dictator_operation op -> Some op | _ -> None)
      (fun op -> Dictator_operation op)

  let signed_operations_case tag =
    case ~tag
      (union [
          manager_kind_case 0 ;
          delegate_kind_case 1 ;
          dictator_kind_case 2 ;
        ])
      (function Sourced_operations ops -> Some ops | _ -> None)
      (fun ops -> Sourced_operations ops)

  let seed_nonce_revelation_encoding =
    (obj3
       (req "kind" (constant "seed_nonce_revelation"))
       (req "level" Raw_level_repr.encoding)
       (req "nonce" Seed_repr.nonce_encoding))

  let seed_nonce_revelation_case tag =
    case ~tag seed_nonce_revelation_encoding
      (function
        | Seed_nonce_revelation { level ; nonce } -> Some ((), level, nonce)
        | _ -> None
      )
      (fun ((), level, nonce) -> Seed_nonce_revelation { level ; nonce })

  let faucet_encoding =
    (obj3
       (req "kind" (constant "faucet"))
       (req "id" Ed25519.Public_key_hash.encoding)
       (req "nonce" (Fixed.bytes 16)))

  let faucet_case tag =
    case ~tag faucet_encoding
      (function
        | Faucet { id ; nonce } -> Some ((), id, nonce)
        | _ -> None
      )
      (fun ((), id, nonce) -> Faucet { id ; nonce })

  let unsigned_operation_case tag =
    case ~tag
      (obj1
         (req "operations"
            (list
               (union [
                   seed_nonce_revelation_case 0 ;
                   faucet_case 1 ;
                 ]))))
      (function Anonymous_operations ops -> Some ops | _ -> None)
      (fun ops -> Anonymous_operations ops)

  let proto_operation_encoding =
    union [
      signed_operations_case 0 ;
      unsigned_operation_case 1 ;
    ]

  let unsigned_operation_encoding =
    merge_objs
      Operation.shell_header_encoding
      proto_operation_encoding

  let signed_proto_operation_encoding =
    merge_objs
      proto_operation_encoding
      (obj1 (varopt "signature" Ed25519.Signature.encoding))

end

type error += Cannot_parse_operation

let encoding =
  let open Data_encoding in
  conv
    (fun { hash ; shell ; contents ; signature } ->
       (hash, (shell, (contents, signature))))
    (fun (hash, (shell, (contents, signature))) ->
       { hash ; shell ; contents ; signature })
    (merge_objs
       (obj1 (req "hash" Operation_hash.encoding))
       (merge_objs
         Operation.shell_header_encoding
         Encoding.signed_proto_operation_encoding))

let () =
  register_error_kind
    `Branch
    ~id:"operation.cannot_parse"
    ~title:"Cannot parse operation"
    ~description:"The operation is ill-formed \
                  or for another protocol version"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "The operation cannot be parsed")
    Data_encoding.unit
    (function Cannot_parse_operation -> Some () | _ -> None)
    (fun () -> Cannot_parse_operation)

let parse hash (op: Operation.t) =
  if not (Compare.Int.(MBytes.length op.proto <= Constants_repr.max_operation_data_length)) then
    error Cannot_parse_operation
  else
    match Data_encoding.Binary.of_bytes
            Encoding.signed_proto_operation_encoding
            op.proto with
    | Some (contents, signature) ->
        ok { hash ; shell = op.shell ; contents ; signature }
    | None -> error Cannot_parse_operation

type error += Invalid_signature (* `Permanent *)
type error += Missing_signature (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"operation.invalid_signature"
    ~title:"Invalid operation signature"
    ~description:"The operation signature is ill-formed \
                  or has been made with the wrong public key"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "The operation signature is invalid")
    Data_encoding.unit
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature) ;
  register_error_kind
    `Permanent
    ~id:"operation.missing_signature"
    ~title:"Missing operation signature"
    ~description:"The operation is of a kind that must be signed, \
                  but the signature is missing"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "The operation requires a signature")
    Data_encoding.unit
    (function Missing_signature -> Some () | _ -> None)
    (fun () -> Missing_signature)

let forge shell proto =
  Data_encoding.Binary.to_bytes
    Encoding.unsigned_operation_encoding (shell, proto)

let check_signature key { shell ; contents ; signature }  =
  match contents, signature with
  | Anonymous_operations _, _ -> return ()
  | Sourced_operations _, None ->
      fail Missing_signature
  | Sourced_operations _, Some signature ->
      let unsigned_operation = forge shell contents in
      if Ed25519.Signature.check key signature unsigned_operation then
        return ()
      else
        fail Invalid_signature

let parse_proto bytes =
  match Data_encoding.Binary.of_bytes
          Encoding.signed_proto_operation_encoding
          bytes with
  | Some (proto, signature) -> return (proto, signature)
  | None -> fail Cannot_parse_operation

include Encoding

let max_operation_data_length = Constants_repr.max_operation_data_length
