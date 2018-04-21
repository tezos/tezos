(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Data_encoding

type balance =
  | Contract of Contract.t
  | Rewards of Signature.Public_key_hash.t * Cycle.t
  | Fees of Signature.Public_key_hash.t * Cycle.t
  | Deposits of Signature.Public_key_hash.t * Cycle.t

let balance_encoding =
  union
    [ case (Tag 0)
        (obj2
           (req "kind" (constant "contract"))
           (req "contract" Contract.encoding))
        (function Contract c -> Some ((), c) | _ -> None )
        (fun ((), c) -> (Contract c)) ;
      case (Tag 1)
        (obj4
           (req "kind" (constant "freezer"))
           (req "category" (constant "rewards"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "level" Cycle.encoding))
        (function Rewards (d, l) -> Some ((), (), d, l) | _ -> None)
        (fun ((), (), d, l) -> Rewards (d, l)) ;
      case (Tag 2)
        (obj4
           (req "kind" (constant "freezer"))
           (req "category" (constant "fees"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "level" Cycle.encoding))
        (function Fees (d, l) -> Some ((), (), d, l) | _ -> None)
        (fun ((), (), d, l) -> Fees (d, l)) ;
      case (Tag 3)
        (obj4
           (req "kind" (constant "freezer"))
           (req "category" (constant "deposits"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "level" Cycle.encoding))
        (function Deposits (d, l) -> Some ((), (), d, l) | _ -> None)
        (fun ((), (), d, l) -> Deposits (d, l)) ]

type balance_update =
  | Debited of Tez.t
  | Credited of Tez.t

let balance_update_encoding =
  union
    [ case (Tag 0)
        (obj1 (req "credited" Tez.encoding))
        (function Credited v -> Some v | Debited _ -> None)
        (fun v -> Credited v) ;
      case (Tag 1)
        (obj1 (req "debited" Tez.encoding))
        (function Debited v -> Some v | Credited _ -> None)
        (fun v -> Debited v) ]

type balance_updates = (balance * balance_update) list

let balance_updates_encoding =
  list (merge_objs balance_encoding balance_update_encoding)

type anonymous_operation_result =
  | Seed_nonce_revelation_result of balance_updates
  | Double_endorsement_evidence_result of balance_updates
  | Double_baking_evidence_result of balance_updates
  | Activation_result of balance_updates

let anonymous_operation_result_encoding =
  union
    [ case (Tag 0)
        (obj2
           (req "kind" (constant "revelation"))
           (req "balance_updates" balance_updates_encoding))
        (function Seed_nonce_revelation_result bus -> Some ((), bus) | _ -> None)
        (fun ((), bus) -> Seed_nonce_revelation_result bus) ;
      case (Tag 1)
        (obj2
           (req "kind" (constant "double_endorsement"))
           (req "balance_updates" balance_updates_encoding))
        (function Double_endorsement_evidence_result bus -> Some ((), bus) | _ -> None)
        (fun ((), bus) -> Double_endorsement_evidence_result bus) ;
      case (Tag 2)
        (obj2
           (req "kind" (constant "double_baking"))
           (req "balance_updates" balance_updates_encoding))
        (function Double_baking_evidence_result bus -> Some ((), bus) | _ -> None)
        (fun ((), bus) -> Double_baking_evidence_result bus) ;
      case (Tag 3)
        (obj2
           (req "kind" (constant "activation"))
           (req "balance_updates" balance_updates_encoding))
        (function Activation_result bus -> Some ((), bus) | _ -> None)
        (fun ((), bus) -> Activation_result bus) ]

type successful_manager_operation_result =
  | Reveal_result
  | Transaction_result of
      { operations : internal_operation list ;
        storage : Script.expr option ;
        balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t }
  | Origination_result of
      { balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t }
  | Delegation_result

type manager_operation_kind =
  | External
  | Internal of internal_operation

let manager_operation_kind_encoding =
  union
    [ case (Tag 0) (constant "external")
        (function External -> Some () | _ -> None)
        (fun () -> External) ;
      case (Tag 1) Operation.internal_operation_encoding
        (function Internal op -> Some op | _ -> None)
        (fun op -> Internal op) ]

type manager_operation_result =
  | Applied of successful_manager_operation_result
  | Failed of error list
  | Skipped

let manager_operation_result_encoding =
  union
    [ case (Tag 0)
        (obj2
           (req "status" (constant "applied"))
           (req "operation_kind" (constant "reveal")))
        (function Applied Reveal_result -> Some ((),()) | _ -> None)
        (fun ((),()) -> Applied Reveal_result) ;
      case (Tag 1)
        (obj8
           (req "status" (constant "applied"))
           (req "operation_kind" (constant "transaction"))
           (dft "emitted" (list Operation.internal_operation_encoding) [])
           (opt "storage" Script.expr_encoding)
           (dft "balance_updates" balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" z Z.zero)
           (dft "storage_size_diff" int64 0L))
        (function
          | Applied (Transaction_result
                       { operations ; storage ; balance_updates ;
                         originated_contracts ; consumed_gas ;
                         storage_size_diff }) ->
              Some ((), (), operations, storage, balance_updates,
                    originated_contracts, consumed_gas,
                    storage_size_diff)
          | _ -> None)
        (fun ((), (), operations, storage, balance_updates,
              originated_contracts, consumed_gas,
              storage_size_diff) ->
          Applied (Transaction_result
                     { operations ; storage ; balance_updates ;
                       originated_contracts ; consumed_gas ;
                       storage_size_diff })) ;
      case (Tag 2)
        (obj6
           (req "status" (constant "applied"))
           (req "operation_kind" (constant "origination"))
           (dft "balance_updates" balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" z Z.zero)
           (dft "storage_size_diff" int64 0L))
        (function
          | Applied (Origination_result
                       { balance_updates ;
                         originated_contracts ; consumed_gas ;
                         storage_size_diff }) ->
              Some ((), (), balance_updates,
                    originated_contracts, consumed_gas,
                    storage_size_diff)
          | _ -> None)
        (fun ((), (), balance_updates,
              originated_contracts, consumed_gas,
              storage_size_diff) ->
          Applied (Origination_result
                     { balance_updates ;
                       originated_contracts ; consumed_gas ;
                       storage_size_diff })) ;
      case (Tag 3)
        (obj2
           (req "status" (constant "applied"))
           (req "operation_kind" (constant "delegation")))
        (function Applied Delegation_result -> Some ((),()) | _ -> None)
        (fun ((),()) -> Applied Delegation_result) ;
      case (Tag 4)
        (obj2
           (req "status" (constant "failed"))
           (req "errors" (list Error_monad.error_encoding)))
        (function Failed errs -> Some ((), errs) | _ -> None)
        (fun ((), errs) -> Failed errs) ;
      case (Tag 5)
        (obj1 (req "status" (constant "skipped")))
        (function Skipped -> Some () | _ -> None)
        (fun () -> Skipped) ]

type consensus_operation_result =
  | Endorsements_result of Signature.Public_key_hash.t * int list

type sourced_operation_result =
  | Consensus_operation_result of consensus_operation_result
  | Amendment_operation_result
  | Manager_operations_result of
      { balance_updates : balance_updates ;
        operation_results : (manager_operation_kind * manager_operation_result) list }
  | Dictator_operation_result

type operation_result =
  | Anonymous_operations_result of anonymous_operation_result list
  | Sourced_operation_result of sourced_operation_result

let encoding =
  union
    [ case (Tag 0)
        (obj2
           (req "kind" (constant "anonymous"))
           (req "results" (list anonymous_operation_result_encoding)))
        (function Anonymous_operations_result rs -> Some ((), rs) | _ -> None)
        (fun ((), rs) -> Anonymous_operations_result rs) ;
      case (Tag 1)
        (obj3
           (req "kind" (constant "endorsements"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "slots" (list uint8)))
        (function
          | Sourced_operation_result
              (Consensus_operation_result
                 (Endorsements_result (d, s))) -> Some ((), d, s)
          | _ -> None)
        (fun ((), d, s) ->
           Sourced_operation_result
             (Consensus_operation_result
                (Endorsements_result (d, s)))) ;
      case (Tag 2)
        (obj1
           (req "kind" (constant "amendment")))
        (function Sourced_operation_result Amendment_operation_result -> Some () | _ -> None)
        (fun () -> Sourced_operation_result Amendment_operation_result) ;
      case (Tag 3)
        (obj1
           (req "kind" (constant "dictator")))
        (function Sourced_operation_result Dictator_operation_result -> Some () | _ -> None)
        (fun () -> Sourced_operation_result Dictator_operation_result) ;
      case (Tag 4)
        (obj3
           (req "kind" (constant "manager"))
           (req "balance_updates" balance_updates_encoding)
           (req "operation_results"
              (list (merge_objs
                       (obj1 (req "operation" manager_operation_kind_encoding))
                       manager_operation_result_encoding))))
        (function
          | Sourced_operation_result
              (Manager_operations_result
                 { balance_updates = bus ; operation_results = rs }) ->
              Some ((), bus, rs) | _ -> None)
        (fun ((), bus, rs) ->
           Sourced_operation_result
             (Manager_operations_result
                { balance_updates = bus ; operation_results = rs })) ]
