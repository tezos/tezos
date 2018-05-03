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

let raw_encoding = Operation.encoding

type operation = {
  shell: Operation.shell_header ;
  contents: proto_operation ;
  signature: Signature.t option ;
}

and proto_operation =
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
      gas_limit: Z.t;
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

type internal_operation = {
  source: Contract_repr.contract ;
  operation: manager_operation ;
}

module Encoding = struct

  open Data_encoding

  let reveal_encoding =
    describe ~title:"Reveal operation" @@
    (obj2
       (req "kind" (constant "reveal"))
       (req "public_key" Signature.Public_key.encoding))

  let reveal_case tag =
    case tag ~name:"Reveal" reveal_encoding
      (function
        | Reveal pkh -> Some ((), pkh)
        | _ -> None)
      (fun ((), pkh) -> Reveal pkh)

  let transaction_encoding =
    describe ~title:"Transaction operation" @@
    obj4
      (req "kind" (constant "transaction"))
      (req "amount" Tez_repr.encoding)
      (req "destination" Contract_repr.encoding)
      (opt "parameters" Script_repr.lazy_expr_encoding)

  let transaction_case tag =
    case tag ~name:"Transaction" transaction_encoding
      (function
        | Transaction { amount ; destination ; parameters } ->
            Some ((), amount, destination, parameters)
        | _ -> None)
      (fun ((), amount, destination, parameters) ->
         Transaction { amount ; destination ; parameters })

  let origination_encoding =
    describe ~title:"Origination operation" @@
    (obj7
       (req "kind" (constant "origination"))
       (req "managerPubkey" Signature.Public_key_hash.encoding)
       (req "balance" Tez_repr.encoding)
       (opt "spendable" bool)
       (opt "delegatable" bool)
       (opt "delegate" Signature.Public_key_hash.encoding)
       (opt "script" Script_repr.encoding))

  let origination_case tag =
    case tag ~name:"Origination" origination_encoding
      (function
        | Origination { manager ; credit ; spendable ;
                        delegatable ; delegate ; script ;
                        preorigination = _
                        (* the hash is only used internally
                           when originating from smart
                           contracts, don't serialize it *) } ->
            Some ((), manager, credit, Some spendable,
                  Some delegatable, delegate, script)
        | _ -> None)
      (fun ((), manager, credit, spendable, delegatable, delegate, script) ->
         let delegatable =
           match delegatable with None -> true | Some b -> b in
         let spendable =
           match spendable with None -> true | Some b -> b in
         Origination
           {manager ; credit ; spendable ; delegatable ;
            delegate ; script ; preorigination = None })

  let delegation_encoding =
    describe ~title:"Delegation operation" @@
    (obj2
       (req "kind" (constant "delegation"))
       (opt "delegate" Signature.Public_key_hash.encoding))


  let delegation_case tag =
    case tag ~name:"Delegation" delegation_encoding
      (function Delegation key -> Some ((), key) | _ -> None)
      (fun ((), key) -> Delegation key)

  let manager_kind_encoding =
    obj7
      (req "kind" (constant "manager"))
      (req "source" Contract_repr.encoding)
      (req "fee" Tez_repr.encoding)
      (req "counter" int32)
      (req "operations"
         (list (union ~tag_size:`Uint8 [
              reveal_case (Tag 0) ;
              transaction_case (Tag 1) ;
              origination_case (Tag 2) ;
              delegation_case (Tag 3) ;
            ])))
      (req "gas_limit" z)
      (req "storage_limit" int64)

  let manager_kind_case tag =
    case tag ~name:"Manager operations" manager_kind_encoding
      (function
        | Manager_operations { source; fee ; counter ; operations ; gas_limit ; storage_limit } ->
            Some ((), source, fee, counter, operations, gas_limit, storage_limit)
        | _ -> None)
      (fun ((), source, fee, counter, operations, gas_limit, storage_limit) ->
         Manager_operations { source; fee ; counter ; operations ; gas_limit ; storage_limit })

  let endorsement_encoding =
    (* describe ~title:"Endorsement operation" @@ *)
    obj4
      (req "kind" (constant "endorsement"))
      (req "block" Block_hash.encoding)
      (req "level" Raw_level_repr.encoding)
      (req "slots" (list int31))

  let consensus_kind_encoding =
    conv
      (function
        | Endorsements { block ; level ; slots } ->
            ((), block, level, slots))
      (fun ((), block, level, slots) ->
         Endorsements { block ; level ; slots })
      endorsement_encoding

  let consensus_kind_case tag =
    case tag consensus_kind_encoding
      (function
        | Consensus_operation op ->
            Some op
        | _ -> None)
      (fun op -> Consensus_operation op)

  let proposal_encoding =
    (obj3
       (req "kind" (constant "proposal"))
       (req "period" Voting_period_repr.encoding)
       (req "proposals" (list Protocol_hash.encoding)))

  let proposal_case tag =
    case tag proposal_encoding
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
    case tag ballot_encoding
      (function
        | Ballot { period ; proposal ; ballot } ->
            Some ((), period, proposal, ballot)
        | _ -> None)
      (fun ((), period, proposal, ballot) ->
         Ballot { period ; proposal ; ballot })

  let amendment_kind_encoding =
    merge_objs
      (obj1 (req "source" Signature.Public_key_hash.encoding))
      (union [
          proposal_case (Tag 0) ;
          ballot_case (Tag 1) ;
        ])

  let amendment_kind_case tag =
    case tag amendment_kind_encoding
      (function
        | Amendment_operation { source ; operation } ->
            Some (source, operation)
        | _ -> None)
      (fun (source, operation) -> Amendment_operation { source ; operation })

  let dictator_kind_encoding =
    let mk_case name args =
      let open Data_encoding in
      conv
        (fun o -> ((), o))
        (fun ((), o) -> o)
        (merge_objs
           (obj1 (req "chain" (constant name)))
           args) in
    let open Data_encoding in
    union ~tag_size:`Uint8 [
      case (Tag 0)
        (mk_case "activate"
           (obj1 (req "hash" Protocol_hash.encoding)))
        (function (Activate hash) -> Some hash | _ -> None)
        (fun hash -> Activate hash) ;
      case (Tag 1)
        (mk_case "activate_testchain"
           (obj1 (req "hash" Protocol_hash.encoding)))
        (function (Activate_testchain hash) -> Some hash | _ -> None)
        (fun hash -> Activate_testchain hash) ;
    ]

  let dictator_kind_case tag =
    case tag dictator_kind_encoding
      (function Dictator_operation op -> Some op | _ -> None)
      (fun op -> Dictator_operation op)

  let signed_operations_case tag =
    case tag
      (union [
          consensus_kind_case (Tag 0) ;
          amendment_kind_case (Tag 1) ;
          manager_kind_case (Tag 2) ;
          dictator_kind_case (Tag 3) ;
        ])
      (function Sourced_operation op -> Some op | _ -> None)
      (fun op -> Sourced_operation op)

  let seed_nonce_revelation_encoding =
    (obj3
       (req "kind" (constant "seed_nonce_revelation"))
       (req "level" Raw_level_repr.encoding)
       (req "nonce" Seed_repr.nonce_encoding))

  let seed_nonce_revelation_case tag =
    case tag seed_nonce_revelation_encoding
      (function
        | Seed_nonce_revelation { level ; nonce } -> Some ((), level, nonce)
        | _ -> None
      )
      (fun ((), level, nonce) -> Seed_nonce_revelation { level ; nonce })

  let double_endorsement_evidence_encoding op_encoding =
    (obj3
       (req "kind" (constant "double_endorsement_evidence"))
       (req "op1" (dynamic_size op_encoding))
       (req "op2" (dynamic_size op_encoding)))

  let double_endorsement_evidence_case tag op_encoding =
    case tag (double_endorsement_evidence_encoding op_encoding)
      (function
        | Double_endorsement_evidence { op1 ; op2 } -> Some ((), op1, op2)
        | _ -> None
      )
      (fun ((), op1, op2) -> Double_endorsement_evidence { op1 ; op2 })

  let double_baking_evidence_encoding =
    (obj3
       (req "kind" (constant "double_baking_evidence"))
       (req "op1" (dynamic_size Block_header_repr.encoding))
       (req "op2" (dynamic_size Block_header_repr.encoding)))

  let double_baking_evidence_case tag =
    case tag double_baking_evidence_encoding
      (function
        | Double_baking_evidence { bh1 ; bh2 } -> Some ((), bh1, bh2)
        | _ -> None
      )
      (fun ((), bh1, bh2) -> Double_baking_evidence { bh1 ; bh2 })

  let activation_encoding =
    (obj3
       (req "kind" (constant "activation"))
       (req "pkh" Ed25519.Public_key_hash.encoding)
       (req "secret" Blinded_public_key_hash.secret_encoding))

  let activation_case tag =
    case tag activation_encoding
      (function
        | Activation { id ; secret } -> Some ((), id, secret)
        | _ -> None
      )
      (fun ((), id, secret) -> Activation { id ; secret })

  let unsigned_operation_case tag op_encoding =
    case tag
      (obj1
         (req "operations"
            (list
               (union [
                   seed_nonce_revelation_case (Tag 0) ;
                   double_endorsement_evidence_case (Tag 1) op_encoding ;
                   double_baking_evidence_case (Tag 2) ;
                   activation_case (Tag 3) ;
                 ]))))
      (function Anonymous_operations ops -> Some ops | _ -> None)
      (fun ops -> Anonymous_operations ops)

  let mu_proto_operation_encoding op_encoding =
    union [
      signed_operations_case (Tag 0) ;
      unsigned_operation_case (Tag 1) op_encoding ;
    ]

  let mu_signed_proto_operation_encoding op_encoding =
    merge_objs
      (mu_proto_operation_encoding op_encoding)
      (obj1 (varopt "signature" Signature.encoding))

  let operation_encoding =
    mu "operation"
      (fun encoding ->
         conv
           (fun { shell ; contents ; signature } ->
              (shell, (contents, signature)))
           (fun (shell, (contents, signature)) ->
              { shell ; contents ; signature })
           (merge_objs
              Operation.shell_header_encoding
              (mu_signed_proto_operation_encoding encoding)))

  let proto_operation_encoding =
    mu_proto_operation_encoding operation_encoding

  let signed_proto_operation_encoding =
    describe ~title:"Signed alpha operation" @@
    mu_signed_proto_operation_encoding operation_encoding

  let unsigned_operation_encoding =
    describe ~title:"Unsigned Alpha operation" @@
    merge_objs
      Operation.shell_header_encoding
      proto_operation_encoding

  let internal_operation_encoding =
    conv
      (fun { source ; operation } -> (source, operation))
      (fun (source, operation) -> { source ; operation })
      (merge_objs
         (obj1
            (req "source" Contract_repr.encoding))
         (union ~tag_size:`Uint8 [
             reveal_case (Tag 0) ;
             transaction_case (Tag 1) ;
             origination_case (Tag 2) ;
             delegation_case (Tag 3) ;
           ]))
end

type error += Cannot_parse_operation

let encoding = Encoding.operation_encoding

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

let parse (op: Operation.t) =
  match Data_encoding.Binary.of_bytes
          Encoding.signed_proto_operation_encoding
          op.proto with
  | Some (contents, signature) ->
      ok { shell = op.shell ; contents ; signature }
  | None -> error Cannot_parse_operation

let acceptable_passes op =
  match op.contents with
  | Sourced_operation (Consensus_operation _) -> [0]
  | Sourced_operation (Amendment_operation _ | Dictator_operation _) -> [1]
  | Anonymous_operations _ -> [2]
  | Sourced_operation (Manager_operations _) -> [3]

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
  Data_encoding.Binary.to_bytes_exn
    Encoding.unsigned_operation_encoding (shell, proto)

let check_signature key { shell ; contents ; signature } =
  match contents, signature with
  | Anonymous_operations _, _ -> return ()
  | Sourced_operation _, None ->
      fail Missing_signature
  | Sourced_operation (Consensus_operation _), Some signature ->
      (* Safe for baking *)
      let unsigned_operation = forge shell contents in
      if Signature.check
          ~watermark:Endorsement
          key signature unsigned_operation then
        return ()
      else
        fail Invalid_signature
  | Sourced_operation _, Some signature ->
      (* Unsafe for baking *)
      let unsigned_operation = forge shell contents in
      if Signature.check
          ~watermark:Generic_operation
          key signature unsigned_operation then
        return ()
      else
        fail Invalid_signature

let parse_proto bytes =
  match Data_encoding.Binary.of_bytes
          Encoding.signed_proto_operation_encoding
          bytes with
  | Some (proto, signature) -> return (proto, signature)
  | None -> fail Cannot_parse_operation

let hash_raw = Operation.hash
let hash o =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      Encoding.signed_proto_operation_encoding
      (o.contents, o.signature) in
  Operation.hash { shell = o.shell ; proto }

include Encoding
