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

let error_encoding =
  def "error"
    ~description:
      "The full list of RPC errors would be too long to include.\n\
       It is available at RPC `/errors` (GET).\n\
       Errors specific to protocol Alpha have an id that starts with `proto.alpha`." @@
  splitted
    ~json:(conv
             (fun err ->
                Data_encoding.Json.construct Error_monad.error_encoding err)
             (fun json ->
                Data_encoding.Json.destruct Error_monad.error_encoding json)
             json)
    ~binary:Error_monad.error_encoding

type balance =
  | Contract of Contract.t
  | Rewards of Signature.Public_key_hash.t * Cycle.t
  | Fees of Signature.Public_key_hash.t * Cycle.t
  | Deposits of Signature.Public_key_hash.t * Cycle.t

let balance_encoding =
  def "operation_metadata.alpha.balance" @@
  union
    [ case (Tag 0)
        ~title:"Contract"
        (obj2
           (req "kind" (constant "contract"))
           (req "contract" Contract.encoding))
        (function Contract c -> Some ((), c) | _ -> None )
        (fun ((), c) -> (Contract c)) ;
      case (Tag 1)
        ~title:"Rewards"
        (obj4
           (req "kind" (constant "freezer"))
           (req "category" (constant "rewards"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "level" Cycle.encoding))
        (function Rewards (d, l) -> Some ((), (), d, l) | _ -> None)
        (fun ((), (), d, l) -> Rewards (d, l)) ;
      case (Tag 2)
        ~title:"Fees"
        (obj4
           (req "kind" (constant "freezer"))
           (req "category" (constant "fees"))
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "level" Cycle.encoding))
        (function Fees (d, l) -> Some ((), (), d, l) | _ -> None)
        (fun ((), (), d, l) -> Fees (d, l)) ;
      case (Tag 3)
        ~title:"Deposits"
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
  def "operation_metadata.alpha.balance_update" @@
  obj1
    (req "change"
       (conv
          (function
            | Credited v -> Tez.to_mutez v
            | Debited v -> Int64.neg (Tez.to_mutez v))
          (Json.wrap_error @@
           fun v ->
           if Compare.Int64.(v < 0L) then
             match Tez.of_mutez (Int64.neg v) with
             | Some v -> Debited v
             | None -> failwith "Qty.of_mutez"
           else
             match Tez.of_mutez v with
             | Some v -> Credited v
             | None -> failwith "Qty.of_mutez")
          int64))

type balance_updates = (balance * balance_update) list

let cleanup_balance_updates balance_updates =
  List.filter
    (fun (_, (Credited update | Debited update)) ->
       not (Tez.equal update Tez.zero))
    balance_updates

let balance_updates_encoding =
  def "operation_metadata.alpha.balance_updates" @@
  list (merge_objs balance_encoding balance_update_encoding)

type _ successful_manager_operation_result =
  | Reveal_result : Kind.reveal successful_manager_operation_result
  | Transaction_result :
      { storage : Script.expr option ;
        balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t ;
      } -> Kind.transaction successful_manager_operation_result
  | Origination_result :
      { balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t ;
      } -> Kind.origination successful_manager_operation_result
  | Delegation_result : Kind.delegation successful_manager_operation_result

type packed_successful_manager_operation_result =
  | Successful_manager_result :
      'kind successful_manager_operation_result -> packed_successful_manager_operation_result

type 'kind manager_operation_result =
  | Applied of 'kind successful_manager_operation_result
  | Failed : 'kind Kind.manager * error list -> 'kind manager_operation_result
  | Skipped : 'kind Kind.manager -> 'kind manager_operation_result

type packed_internal_operation_result =
  | Internal_operation_result :
      'kind internal_operation * 'kind manager_operation_result -> packed_internal_operation_result

module Manager_result = struct

  type 'kind case =
      MCase : {
        op_case: 'kind Operation.Encoding.Manager_operations.case ;
        encoding: 'a Data_encoding.t ;
        kind: 'kind Kind.manager ;
        iselect:
          packed_internal_operation_result ->
          ('kind internal_operation * 'kind manager_operation_result) option;
        select:
          packed_successful_manager_operation_result ->
          'kind successful_manager_operation_result option ;
        proj: 'kind successful_manager_operation_result -> 'a ;
        inj: 'a -> 'kind successful_manager_operation_result ;
        t: 'kind manager_operation_result Data_encoding.t ;
      } -> 'kind case

  let make ~op_case ~encoding ~kind ~iselect ~select ~proj ~inj =
    let Operation.Encoding.Manager_operations.MCase { name ; _ } = op_case in
    let t =
      def (Format.asprintf "operation.alpha.operation_result.%s" name) @@
      union ~tag_size:`Uint8 [
        case (Tag 0)
          ~title:"Applied"
          (merge_objs
             (obj1
                (req "status" (constant "applied")))
             encoding)
          (fun o ->
             match o with
             | Skipped _ | Failed _ -> None
             | Applied o ->
                 match select (Successful_manager_result o) with
                 | None -> None
                 | Some o -> Some ((), proj o))
          (fun ((), x) ->  (Applied (inj x))) ;
        case (Tag 1)
          ~title:"Failed"
          (obj2
             (req "status" (constant "failed"))
             (req "errors" (list error_encoding)))
          (function  (Failed (_, errs)) -> Some ((), errs) | _ -> None)
          (fun ((), errs) -> Failed (kind, errs)) ;
        case (Tag 2)
          ~title:"Skipped"
          (obj1 (req "status" (constant "skipped")))
          (function Skipped _ -> Some () | _ -> None)
          (fun () -> Skipped kind)
      ] in
    MCase { op_case ; encoding ; kind ; iselect ; select ; proj ; inj ; t }

  let reveal_case =
    make
      ~op_case: Operation.Encoding.Manager_operations.reveal_case
      ~encoding: Data_encoding.empty
      ~iselect:
        (function
          | Internal_operation_result
              ({ operation = Reveal _ ; _} as op, res) ->
              Some (op, res)
          | _ -> None)
      ~select:
        (function
          | Successful_manager_result (Reveal_result as op) -> Some op
          | _ -> None)
      ~kind: Kind.Reveal_manager_kind
      ~proj: (function Reveal_result -> ())
      ~inj: (fun () -> Reveal_result)

  let transaction_case =
    make
      ~op_case: Operation.Encoding.Manager_operations.transaction_case
      ~encoding:
        (obj5
           (opt "storage" Script.expr_encoding)
           (dft "balance_updates" balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" z Z.zero)
           (dft "storage_size_diff" int64 0L))
      ~iselect:
        (function
          | Internal_operation_result
              ({ operation = Transaction _ ; _} as op, res) ->
              Some (op, res)
          | _ -> None)
      ~select:
        (function
          | Successful_manager_result (Transaction_result _ as op) -> Some op
          | _ -> None)
      ~kind: Kind.Transaction_manager_kind
      ~proj:
        (function
          | Transaction_result
              { storage ; balance_updates ;
                originated_contracts ; consumed_gas ;
                storage_size_diff } ->
              (storage, balance_updates,
               originated_contracts, consumed_gas,
               storage_size_diff))
      ~inj:
        (fun (storage, balance_updates,
              originated_contracts, consumed_gas,
              storage_size_diff) ->
          Transaction_result { storage ; balance_updates ;
                               originated_contracts ; consumed_gas ;
                               storage_size_diff })

  let origination_case =
    make
      ~op_case: Operation.Encoding.Manager_operations.origination_case
      ~encoding:
        (obj4
           (dft "balance_updates" balance_updates_encoding [])
           (dft "originated_contracts" (list Contract.encoding) [])
           (dft "consumed_gas" z Z.zero)
           (dft "storage_size_diff" int64 0L))
      ~iselect:
        (function
          | Internal_operation_result
              ({ operation = Origination _ ; _} as op, res) ->
              Some (op, res)
          | _ -> None)
      ~select:
        (function
          | Successful_manager_result (Origination_result _ as op) -> Some op
          | _ -> None)
      ~proj:
        (function
          | Origination_result
              { balance_updates ;
                originated_contracts ; consumed_gas ;
                storage_size_diff } ->
              (balance_updates,
               originated_contracts, consumed_gas,
               storage_size_diff))
      ~kind: Kind.Origination_manager_kind
      ~inj:
        (fun (balance_updates,
              originated_contracts, consumed_gas,
              storage_size_diff) ->
          Origination_result
            { balance_updates ;
              originated_contracts ; consumed_gas ;
              storage_size_diff })

  let delegation_case =
    make
      ~op_case: Operation.Encoding.Manager_operations.delegation_case
      ~encoding: Data_encoding.empty
      ~iselect:
        (function
          | Internal_operation_result
              ({ operation = Delegation _ ; _} as op, res) ->
              Some (op, res)
          | _ -> None)
      ~select:
        (function
          | Successful_manager_result (Delegation_result as op) -> Some op
          | _ -> None)
      ~kind: Kind.Delegation_manager_kind
      ~proj: (function Delegation_result -> ())
      ~inj: (fun () -> Delegation_result)

end

let internal_operation_result_encoding :
  packed_internal_operation_result Data_encoding.t =
  let make (type kind)
      (Manager_result.MCase res_case : kind Manager_result.case) =
    let Operation.Encoding.Manager_operations.MCase op_case = res_case.op_case in
    case (Tag op_case.tag)
      ~title:op_case.name
      (merge_objs
         (obj3
            (req "kind" (constant op_case.name))
            (req "source" Contract.encoding)
            (req "nonce" uint16))
         (merge_objs
            op_case.encoding
            (obj1 (req "result" res_case.t))))
      (fun op ->
         match res_case.iselect op with
         | Some (op, res) ->
             Some (((), op.source, op.nonce),
                   (op_case.proj op.operation, res))
         | None -> None)
      (fun (((), source, nonce), (op, res)) ->
         let op = { source ; operation = op_case.inj op ; nonce } in
         Internal_operation_result (op, res)) in
  def "operation.alpha.internal_operation_result" @@
  union [
    make Manager_result.reveal_case ;
    make Manager_result.transaction_case ;
    make Manager_result.origination_case ;
    make Manager_result.delegation_case ;
  ]

type 'kind contents_result =
  | Endorsements_result :
      Signature.Public_key_hash.t * int list -> Kind.endorsements contents_result
  | Seed_nonce_revelation_result :
      balance_updates -> Kind.seed_nonce_revelation contents_result
  | Double_endorsement_evidence_result :
      balance_updates -> Kind.double_endorsement_evidence contents_result
  | Double_baking_evidence_result :
      balance_updates -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      balance_updates -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
  | Manager_operation_result :
      { balance_updates : balance_updates ;
        operation_result : 'kind manager_operation_result ;
        internal_operation_results : packed_internal_operation_result list ;
      } -> 'kind Kind.manager contents_result

type packed_contents_result =
  | Contents_result : 'kind contents_result -> packed_contents_result

type packed_contents_and_result =
  | Contents_and_result :
      'kind Operation.contents * 'kind contents_result -> packed_contents_and_result

module Encoding = struct

  type 'kind case =
      Case : { op_case: 'kind Operation.Encoding.case ;
               encoding: 'a Data_encoding.t ;
               select: packed_contents_result -> 'kind contents_result option ;
               mselect: packed_contents_and_result -> ('kind contents * 'kind contents_result) option ;
               proj: 'kind contents_result -> 'a ;
               inj: 'a -> 'kind contents_result ;
             } -> 'kind case

  let tagged_case tag name args proj inj =
    let open Data_encoding in
    case tag
      ~title:(String.capitalize_ascii name)
      (merge_objs
         (obj1 (req "kind" (constant name)))
         args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  let endorsement_case =
    Case {
      op_case = Operation.Encoding.endorsement_case ;
      encoding =
        (obj2
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "slots" (list uint8))) ;
      select =
        (function
          | Contents_result (Endorsements_result _ as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Endorsements _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj =
        (function
          | Endorsements_result (d, s) -> (d, s)) ;
      inj =
        (fun (d, s) -> Endorsements_result (d, s))
    }

  let seed_nonce_revelation_case =
    Case {
      op_case = Operation.Encoding.seed_nonce_revelation_case ;
      encoding =
        (obj1
           (req "balance_updates" balance_updates_encoding)) ;
      select =
        (function
          | Contents_result (Seed_nonce_revelation_result _ as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Seed_nonce_revelation _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj = (fun (Seed_nonce_revelation_result bus) -> bus) ;
      inj = (fun bus -> Seed_nonce_revelation_result bus) ;
    }

  let double_endorsement_evidence_case =
    Case {
      op_case = Operation.Encoding.double_endorsement_evidence_case ;
      encoding =
        (obj1
           (req "balance_updates" balance_updates_encoding)) ;
      select =
        (function
          | Contents_result (Double_endorsement_evidence_result _ as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Double_endorsement_evidence _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj =
        (fun (Double_endorsement_evidence_result bus) -> bus) ;
      inj = (fun bus -> Double_endorsement_evidence_result bus)
    }

  let double_baking_evidence_case =
    Case {
      op_case = Operation.Encoding.double_baking_evidence_case ;
      encoding =
        (obj1
           (req "balance_updates" balance_updates_encoding)) ;
      select =
        (function
          | Contents_result (Double_baking_evidence_result _ as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Double_baking_evidence _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj =
        (fun (Double_baking_evidence_result bus) -> bus) ;
      inj = (fun bus -> Double_baking_evidence_result bus) ;
    }

  let activate_account_case =
    Case {
      op_case = Operation.Encoding.activate_account_case ;
      encoding =
        (obj1
           (req "balance_updates" balance_updates_encoding)) ;
      select =
        (function
          | Contents_result (Activate_account_result _ as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Activate_account _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj = (fun (Activate_account_result bus) -> bus) ;
      inj = (fun bus -> Activate_account_result bus) ;
    }

  let proposals_case =
    Case {
      op_case = Operation.Encoding.proposals_case ;
      encoding = Data_encoding.empty ;
      select =
        (function
          | Contents_result (Proposals_result as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Proposals _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj = (fun Proposals_result -> ()) ;
      inj = (fun () -> Proposals_result) ;
    }

  let ballot_case =
    Case {
      op_case = Operation.Encoding.ballot_case ;
      encoding = Data_encoding.empty ;
      select =
        (function
          | Contents_result (Ballot_result as op) -> Some op
          | _ -> None) ;
      mselect =
        (function
          | Contents_and_result (Ballot _ as op, res) -> Some (op, res)
          | _ -> None) ;
      proj = (fun Ballot_result -> ()) ;
      inj = (fun () -> Ballot_result) ;
    }

  let make_manager_case
      (type kind)
      (Operation.Encoding.Case op_case : kind Kind.manager Operation.Encoding.case)
      (Manager_result.MCase res_case : kind Manager_result.case)
      mselect =
    Case {
      op_case = Operation.Encoding.Case op_case ;
      encoding =
        (obj3
           (req "balance_updates" balance_updates_encoding)
           (req "operation_result" res_case.t)
           (dft "internal_operation_results"
              (list internal_operation_result_encoding) [])) ;
      select =
        (function
          | Contents_result
              (Manager_operation_result
                 ({ operation_result = Applied res ; _ } as op)) -> begin
              match res_case.select (Successful_manager_result res) with
              | Some res ->
                  Some (Manager_operation_result
                          { op with operation_result = Applied res })
              | None -> None
            end
          | _ -> None) ;
      mselect ;
      proj =
        (fun (Manager_operation_result
                { balance_updates = bus ; operation_result = r ;
                  internal_operation_results = rs }) ->
          (bus, r, rs)) ;
      inj =
        (fun (bus, r, rs) ->
           Manager_operation_result
             { balance_updates = bus ; operation_result = r ;
               internal_operation_results = rs }) ;
    }

  let reveal_case =
    make_manager_case
      Operation.Encoding.reveal_case
      Manager_result.reveal_case
      (function
        | Contents_and_result
            (Manager_operation
               { operation = Reveal _ ; _ } as op, res) ->
            Some (op, res)
        | _ -> None)

  let transaction_case =
    make_manager_case
      Operation.Encoding.transaction_case
      Manager_result.transaction_case
      (function
        | Contents_and_result
            (Manager_operation
               { operation = Transaction _ ; _ } as op, res) ->
            Some (op, res)
        | _ -> None)

  let origination_case =
    make_manager_case
      Operation.Encoding.origination_case
      Manager_result.origination_case
      (function
        | Contents_and_result
            (Manager_operation
               { operation = Origination _ ; _ } as op, res) ->
            Some (op, res)
        | _ -> None)

  let delegation_case =
    make_manager_case
      Operation.Encoding.delegation_case
      Manager_result.delegation_case
      (function
        | Contents_and_result
            (Manager_operation
               { operation = Delegation _ ; _ } as op, res) ->
            Some (op, res)
        | _ -> None)

end

let contents_result_encoding =
  let open Encoding in
  let make (Case { op_case = Operation.Encoding.Case { tag ; name ; _ } ;
                   encoding ; mselect = _ ; select ; proj ; inj }) =
    let proj x =
      match select x with
      | None -> None
      | Some x -> Some (proj x) in
    let inj x = Contents_result (inj x) in
    tagged_case (Tag tag) name encoding proj inj in
  def "operation.alpha.contents_result" @@
  union [
    make endorsement_case ;
    make seed_nonce_revelation_case ;
    make double_endorsement_evidence_case ;
    make double_baking_evidence_case ;
    make activate_account_case ;
    make proposals_case ;
    make ballot_case ;
    make reveal_case ;
    make transaction_case ;
    make origination_case ;
    make delegation_case ;
  ]

let contents_and_result_encoding =
  let open Encoding in
  let make
      (Case { op_case = Operation.Encoding.Case { tag ; name ; encoding ; proj ; inj ; _ } ;
              mselect ; encoding = meta_encoding ; proj = meta_proj ; inj = meta_inj ; _ }) =
    let proj c =
      match mselect c with
      | Some (op, res) -> Some (proj op, meta_proj res)
      | _ -> None in
    let inj (op, res) = Contents_and_result (inj op, meta_inj res) in
    let encoding =
      merge_objs
        encoding
        (obj1
           (req "metadata" meta_encoding)) in
    tagged_case (Tag tag) name encoding proj inj in
  def "operation.alpha.operation_contents_and_result" @@
  union [
    make endorsement_case ;
    make seed_nonce_revelation_case ;
    make double_endorsement_evidence_case ;
    make double_baking_evidence_case ;
    make activate_account_case ;
    make proposals_case ;
    make ballot_case ;
    make reveal_case ;
    make transaction_case ;
    make origination_case ;
    make delegation_case ;
  ]

type 'kind contents_result_list =
  | Single_result : 'kind contents_result -> 'kind contents_result_list
  | Cons_result :
      'kind Kind.manager contents_result * 'rest Kind.manager contents_result_list ->
    (('kind * 'rest) Kind.manager ) contents_result_list

type packed_contents_result_list =
    Contents_result_list : 'kind contents_result_list -> packed_contents_result_list

let contents_result_list_encoding =
  let rec to_list = function
    | Contents_result_list (Single_result o) -> [Contents_result o]
    | Contents_result_list (Cons_result (o, os)) ->
        Contents_result o :: to_list (Contents_result_list os) in
  let rec of_list = function
    | [] -> Pervasives.failwith "cannot decode empty operation result"
    | [Contents_result o] -> Contents_result_list (Single_result o)
    | (Contents_result o) :: os ->
        let Contents_result_list os = of_list os in
        match o, os with
        | Manager_operation_result _, Single_result (Manager_operation_result _) ->
            Contents_result_list (Cons_result (o, os))
        | Manager_operation_result _, Cons_result _ ->
            Contents_result_list (Cons_result (o, os))
        | _ -> Pervasives.failwith "cannot decode ill-formed operation result" in
  def "operation.alpha.contents_list_result" @@
  conv to_list of_list (list contents_result_encoding)

type 'kind contents_and_result_list =
  | Single_and_result : 'kind Alpha_context.contents * 'kind contents_result -> 'kind contents_and_result_list
  | Cons_and_result : 'kind Kind.manager Alpha_context.contents * 'kind Kind.manager contents_result * 'rest Kind.manager contents_and_result_list -> ('kind * 'rest) Kind.manager contents_and_result_list

type packed_contents_and_result_list =
  | Contents_and_result_list : 'kind contents_and_result_list -> packed_contents_and_result_list

let contents_and_result_list_encoding =
  let rec to_list = function
    | Contents_and_result_list (Single_and_result (op, res)) ->
        [Contents_and_result (op, res)]
    | Contents_and_result_list (Cons_and_result (op, res, rest)) ->
        Contents_and_result (op, res) ::
        to_list (Contents_and_result_list rest) in
  let rec of_list = function
    | [] -> Pervasives.failwith "cannot decode empty combined operation result"
    | [Contents_and_result (op, res)] ->
        Contents_and_result_list (Single_and_result (op, res))
    | (Contents_and_result (op, res)) :: rest ->
        let Contents_and_result_list rest = of_list rest in
        match op, rest with
        | Manager_operation _, Single_and_result (Manager_operation _, _) ->
            Contents_and_result_list (Cons_and_result (op, res, rest))
        | Manager_operation _, Cons_and_result (_, _, _) ->
            Contents_and_result_list (Cons_and_result (op, res, rest))
        | _ -> Pervasives.failwith "cannot decode ill-formed combined operation result" in
  conv to_list of_list (list contents_and_result_encoding)

type 'kind operation_metadata = {
  contents: 'kind contents_result_list ;
}

type packed_operation_metadata =
  | Operation_metadata : 'kind operation_metadata -> packed_operation_metadata

let operation_metadata_encoding =
  def "operation.alpha.result" @@
  conv
    (fun (Operation_metadata { contents }) -> Contents_result_list contents)
    (fun (Contents_result_list contents) -> Operation_metadata { contents })
    contents_result_list_encoding

type ('a, 'b) eq = Eq : ('a, 'a) eq

let kind_equal
  : type kind kind2. kind contents -> kind2 contents_result -> (kind, kind2) eq option =
  fun op res ->
    match op, res with
    | Endorsements _, Endorsements_result _ -> Some Eq
    | Endorsements _, _ -> None
    | Seed_nonce_revelation _, Seed_nonce_revelation_result _ -> Some Eq
    | Seed_nonce_revelation _, _ -> None
    | Double_endorsement_evidence _, Double_endorsement_evidence_result _ -> Some Eq
    | Double_endorsement_evidence _, _ -> None
    | Double_baking_evidence _, Double_baking_evidence_result _ -> Some Eq
    | Double_baking_evidence _, _ -> None
    | Activate_account _, Activate_account_result _ -> Some Eq
    | Activate_account _, _ -> None
    | Proposals _, Proposals_result -> Some Eq
    | Proposals _, _ -> None
    | Ballot _, Ballot_result -> Some Eq
    | Ballot _, _ -> None
    | Manager_operation
        { operation = Reveal _ ; _ },
      Manager_operation_result
        { operation_result = Applied Reveal_result ; _ } -> Some Eq
    | Manager_operation
        { operation = Reveal _ ; _ },
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Reveal_manager_kind, _); _ } -> Some Eq
    | Manager_operation
        { operation = Reveal _ ; _ },
      Manager_operation_result
        { operation_result =
            Skipped (Alpha_context.Kind.Reveal_manager_kind); _ } -> Some Eq
    | Manager_operation { operation = Reveal _ ; _ }, _ -> None
    | Manager_operation
        { operation = Transaction _ ; _ },
      Manager_operation_result
        { operation_result = Applied (Transaction_result _); _ } -> Some Eq
    | Manager_operation
        { operation = Transaction _ ; _ },
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Transaction_manager_kind, _); _ } -> Some Eq
    | Manager_operation
        { operation = Transaction _ ; _ },
      Manager_operation_result
        { operation_result =
            Skipped (Alpha_context.Kind.Transaction_manager_kind); _ } -> Some Eq
    | Manager_operation { operation = Transaction _ ; _ }, _ -> None
    | Manager_operation
        { operation = Origination _ ; _ },
      Manager_operation_result
        { operation_result = Applied (Origination_result _); _ } -> Some Eq
    | Manager_operation
        { operation = Origination _ ; _ },
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Origination_manager_kind, _); _ } -> Some Eq
    | Manager_operation
        { operation = Origination _ ; _ },
      Manager_operation_result
        { operation_result =
            Skipped (Alpha_context.Kind.Origination_manager_kind); _ } -> Some Eq
    | Manager_operation { operation = Origination _ ; _ }, _ -> None
    | Manager_operation
        { operation = Delegation _ ; _ },
      Manager_operation_result
        { operation_result = Applied Delegation_result ; _ } -> Some Eq
    | Manager_operation
        { operation = Delegation _ ; _ },
      Manager_operation_result
        { operation_result =
            Failed (Alpha_context.Kind.Delegation_manager_kind, _); _ } -> Some Eq
    | Manager_operation
        { operation = Delegation _ ; _ },
      Manager_operation_result
        { operation_result =
            Skipped (Alpha_context.Kind.Delegation_manager_kind); _ } -> Some Eq
    | Manager_operation { operation = Delegation _ ; _ }, _ -> None

let rec kind_equal_list
  : type kind kind2. kind contents_list -> kind2 contents_result_list -> (kind, kind2) eq option =
  fun contents res ->
    match contents, res with
    | Single op, Single_result res -> begin
        match kind_equal op res with
        | None -> None
        | Some Eq -> Some Eq
      end
    | Cons (op, ops), Cons_result (res, ress) -> begin
        match kind_equal op res with
        | None -> None
        | Some Eq ->
            match kind_equal_list ops ress with
            | None -> None
            | Some Eq -> Some Eq
      end
    | _ -> None

let rec pack_contents_list :
  type kind. kind contents_list -> kind contents_result_list -> kind contents_and_result_list =
  fun contents res -> begin
      match contents, res with
      | Single op, Single_result res -> Single_and_result (op, res)
      | Cons (op, ops), Cons_result (res, ress) ->
          Cons_and_result (op, res, pack_contents_list ops ress)
      | Single (Manager_operation _),
        Cons_result (Manager_operation_result _, Single_result _) -> .
      | Cons (_, _),
        Single_result (Manager_operation_result
                         { operation_result = Failed _ ; _}) -> .
      | Cons (_, _),
        Single_result (Manager_operation_result
                         { operation_result = Skipped _ ; _}) -> .
      | Cons (_, _),
        Single_result (Manager_operation_result
                         { operation_result = Applied _ ; _}) -> .
      | Single _, Cons_result _ -> .
    end

let rec unpack_contents_list :
  type kind. kind contents_and_result_list ->
  (kind contents_list * kind contents_result_list) =
  function
  | Single_and_result (op, res) -> Single op, Single_result res
  | Cons_and_result (op, res, rest) ->
      let ops, ress = unpack_contents_list rest in
      Cons (op, ops), Cons_result (res, ress)

let operation_data_and_metadata_encoding =
  def "operation.alpha.operation_with_metadata" @@
  conv
    (fun (Operation_data op, Operation_metadata res) ->
       match kind_equal_list op.contents res.contents with
       | None -> Pervasives.failwith "cannot decode inconsistent combined operation result"
       | Some Eq ->
           (Contents_and_result_list
              (pack_contents_list op.contents res.contents),
            op.signature))
    (fun (Contents_and_result_list contents, signature) ->
       let op_contents, res_contents = unpack_contents_list contents in
       (Operation_data { contents = op_contents ; signature },
        Operation_metadata { contents = res_contents }))
    (obj2
       (req "contents" contents_and_result_list_encoding)
       (varopt "signature" Signature.encoding))
