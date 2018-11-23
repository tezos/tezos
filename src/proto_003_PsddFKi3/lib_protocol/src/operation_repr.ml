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

module Kind = struct
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

let raw_encoding = Operation.encoding

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

let manager_kind : type kind. kind manager_operation -> kind Kind.manager =
  function
  | Reveal _ -> Kind.Reveal_manager_kind
  | Transaction _ -> Kind.Transaction_manager_kind
  | Origination _ -> Kind.Origination_manager_kind
  | Delegation _ -> Kind.Delegation_manager_kind

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

type packed_protocol_data =
  | Operation_data : 'kind protocol_data -> packed_protocol_data

type packed_operation = {
  shell: Operation.shell_header ;
  protocol_data: packed_protocol_data ;
}

let pack ({ shell ; protocol_data} : _ operation) : packed_operation = {
  shell ;
  protocol_data = Operation_data protocol_data ;
}

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

let rec to_list = function
  | Contents_list (Single o) -> [Contents o]
  | Contents_list (Cons (o, os)) ->
      Contents o :: to_list (Contents_list os)

let rec of_list = function
  | [] -> assert false
  | [Contents o] -> Contents_list (Single o)
  | (Contents o) :: os ->
      let Contents_list os = of_list os in
      match o, os with
      | Manager_operation _, Single (Manager_operation _) ->
          Contents_list (Cons (o, os))
      | Manager_operation _, Cons _ ->
          Contents_list (Cons (o, os))
      | _ ->
          Pervasives.failwith "Operation list of length > 1 \
                               should only contains manager operations."

module Encoding = struct

  open Data_encoding

  let case tag name args proj inj =
    let open Data_encoding in
    case tag
      ~title:(String.capitalize_ascii name)
      (merge_objs
         (obj1 (req "kind" (constant name)))
         args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  module Manager_operations = struct

    type 'kind case =
        MCase : { tag: int ;
                  name: string ;
                  encoding: 'a Data_encoding.t ;
                  select: packed_manager_operation -> 'kind manager_operation option ;
                  proj: 'kind manager_operation -> 'a ;
                  inj: 'a -> 'kind manager_operation } -> 'kind case

    let reveal_case =
      MCase {
        tag = 0 ;
        name = "reveal" ;
        encoding =
          (obj1
             (req "public_key" Signature.Public_key.encoding)) ;
        select =
          (function
            | Manager (Reveal _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function Reveal pkh -> pkh) ;
        inj =
          (fun pkh -> Reveal pkh)
      }

    let transaction_case =
      MCase {
        tag = 1 ;
        name = "transaction" ;
        encoding =
          (obj3
             (req "amount" Tez_repr.encoding)
             (req "destination" Contract_repr.encoding)
             (opt "parameters" Script_repr.lazy_expr_encoding)) ;
        select =
          (function
            | Manager (Transaction _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function
            | Transaction { amount ; destination ; parameters } ->
                (amount, destination, parameters)) ;
        inj =
          (fun (amount, destination, parameters) ->
             Transaction { amount ; destination ; parameters })
      }

    let origination_case =
      MCase {
        tag = 2 ;
        name = "origination" ;
        encoding =
          (obj6
             (req "managerPubkey" Signature.Public_key_hash.encoding)
             (req "balance" Tez_repr.encoding)
             (dft "spendable" bool true)
             (dft "delegatable" bool true)
             (opt "delegate" Signature.Public_key_hash.encoding)
             (opt "script" Script_repr.encoding)) ;
        select =
          (function
            | Manager (Origination _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function
            | Origination { manager ; credit ; spendable ;
                            delegatable ; delegate ; script ;
                            preorigination = _
                            (* the hash is only used internally
                               when originating from smart
                               contracts, don't serialize it *) } ->
                (manager, credit, spendable,
                 delegatable, delegate, script)) ;
        inj =
          (fun (manager, credit, spendable, delegatable, delegate, script) ->
             Origination
               {manager ; credit ; spendable ; delegatable ;
                delegate ; script ; preorigination = None })
      }

    let delegation_case =
      MCase {
        tag = 3 ;
        name = "delegation" ;
        encoding =
          (obj1
             (opt "delegate" Signature.Public_key_hash.encoding)) ;
        select =
          (function
            | Manager (Delegation _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function Delegation key -> key) ;
        inj =
          (fun key -> Delegation key)
      }

    let encoding =
      let make (MCase { tag ; name ; encoding ; select ; proj ; inj }) =
        case (Tag tag) name encoding
          (fun o -> match select o with None -> None | Some o -> Some (proj o))
          (fun x -> Manager (inj x)) in
      union ~tag_size:`Uint8 [
        make reveal_case ;
        make transaction_case ;
        make origination_case ;
        make delegation_case ;
      ]

  end

  type 'b case =
      Case : { tag: int ;
               name: string ;
               encoding: 'a Data_encoding.t ;
               select: packed_contents -> 'b contents option ;
               proj: 'b contents -> 'a ;
               inj: 'a -> 'b contents } -> 'b case

  let endorsement_encoding =
    obj1
      (req "level" Raw_level_repr.encoding)

  let endorsement_case =
    Case {
      tag = 0 ;
      name = "endorsement" ;
      encoding = endorsement_encoding ;
      select =
        (function
          | Contents (Endorsement _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Endorsement { level }) -> level) ;
      inj =
        (fun level -> Endorsement { level })
    }

  let endorsement_encoding =
    let make (Case { tag ; name ; encoding ; select = _ ; proj ; inj }) =
      case (Tag tag) name encoding
        (fun o -> Some (proj o))
        (fun x -> inj x) in
    let to_list : Kind.endorsement contents_list -> _ = function
      | Single o -> o in
    let of_list : Kind.endorsement contents -> _ = function
      | o -> Single o in
    def "inlined.endorsement" @@
    conv
      (fun ({ shell ; protocol_data = { contents ; signature } } : _ operation)->
         (shell, (contents, signature)))
      (fun (shell, (contents, signature)) ->
         ({ shell ; protocol_data = { contents ; signature }} : _ operation))
      (merge_objs
         Operation.shell_header_encoding
         (obj2
            (req "operations"
               (conv to_list of_list  @@
                def "inlined.endorsement.contents" @@
                union [
                  make endorsement_case ;
                ]))
            (varopt "signature" Signature.encoding)))

  let seed_nonce_revelation_case =
    Case {
      tag = 1;
      name = "seed_nonce_revelation" ;
      encoding =
        (obj2
           (req "level" Raw_level_repr.encoding)
           (req "nonce" Seed_repr.nonce_encoding)) ;
      select =
        (function
          | Contents (Seed_nonce_revelation _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Seed_nonce_revelation { level ; nonce }) -> (level, nonce)) ;
      inj =
        (fun (level, nonce) -> Seed_nonce_revelation { level ; nonce })
    }

  let double_endorsement_evidence_case : Kind.double_endorsement_evidence case =
    Case {
      tag = 2 ;
      name = "double_endorsement_evidence" ;
      encoding =
        (obj2
           (req "op1" (dynamic_size endorsement_encoding))
           (req "op2" (dynamic_size endorsement_encoding))) ;
      select =
        (function
          | Contents (Double_endorsement_evidence _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Double_endorsement_evidence { op1 ; op2 }) -> (op1, op2)) ;
      inj =
        (fun (op1, op2) -> (Double_endorsement_evidence { op1 ; op2 }))
    }

  let double_baking_evidence_case =
    Case {
      tag = 3 ;
      name = "double_baking_evidence" ;
      encoding =
        (obj2
           (req "bh1" (dynamic_size Block_header_repr.encoding))
           (req "bh2" (dynamic_size Block_header_repr.encoding))) ;
      select =
        (function
          | Contents (Double_baking_evidence _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Double_baking_evidence { bh1 ; bh2 }) -> (bh1, bh2)) ;
      inj =
        (fun (bh1, bh2) -> Double_baking_evidence { bh1 ; bh2 }) ;
    }

  let activate_account_case =
    Case {
      tag = 4 ;
      name = "activate_account" ;
      encoding =
        (obj2
           (req "pkh" Ed25519.Public_key_hash.encoding)
           (req "secret" Blinded_public_key_hash.activation_code_encoding)) ;
      select =
        (function
          | Contents (Activate_account _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Activate_account { id ; activation_code }) -> (id, activation_code)) ;
      inj =
        (fun (id, activation_code) -> Activate_account { id ; activation_code })
    }

  let proposals_case =
    Case {
      tag = 5 ;
      name = "proposals" ;
      encoding =
        (obj3
           (req "source" Signature.Public_key_hash.encoding)
           (req "period" Voting_period_repr.encoding)
           (req "proposals" (list Protocol_hash.encoding))) ;
      select =
        (function
          | Contents (Proposals _ as op) -> Some op
          | _ -> None) ;
      proj =
        (fun (Proposals { source ; period ; proposals }) ->
           (source, period, proposals)) ;
      inj =
        (fun (source, period, proposals) ->
           Proposals { source ; period ; proposals }) ;
    }

  let ballot_case =
    Case {
      tag = 6 ;
      name = "ballot" ;
      encoding =
        (obj4
           (req "source" Signature.Public_key_hash.encoding)
           (req "period" Voting_period_repr.encoding)
           (req "proposal" Protocol_hash.encoding)
           (req "ballot" Vote_repr.ballot_encoding)) ;
      select =
        (function
          | Contents (Ballot _ as op) -> Some op
          | _ -> None) ;
      proj =
        (function
            (Ballot { source ; period ; proposal ; ballot }) ->
              (source, period, proposal, ballot)) ;
      inj =
        (fun (source, period, proposal, ballot) ->
           Ballot { source ; period ; proposal ; ballot }) ;
    }

  let manager_encoding =
    (obj5
       (req "source" Contract_repr.encoding)
       (req "fee" Tez_repr.encoding)
       (req "counter" (check_size 10 n))
       (req "gas_limit" (check_size 10 n))
       (req "storage_limit" (check_size 10 n)))

  let extract
      (type kind)
      (Manager_operation { source ; fee ; counter ;
                           gas_limit ; storage_limit ; operation = _ } : kind Kind.manager contents) =
    (source, fee, counter, gas_limit, storage_limit)

  let rebuild (source, fee, counter, gas_limit, storage_limit) operation =
    Manager_operation { source ; fee ; counter ;
                        gas_limit ; storage_limit ; operation }

  let make_manager_case tag
      (type kind)
      (Manager_operations.MCase mcase : kind Manager_operations.case) =
    Case {
      tag ;
      name = mcase.name ;
      encoding =
        merge_objs
          manager_encoding
          mcase.encoding ;
      select =
        (function
          | Contents (Manager_operation ({ operation ; _ } as op)) -> begin
              match mcase.select (Manager operation) with
              | None -> None
              | Some operation ->
                  Some (Manager_operation { op with operation })
            end
          | _ -> None) ;
      proj =
        (function
          | Manager_operation { operation ; _ } as op ->
              (extract op, mcase.proj operation )) ;
      inj =
        (fun (op, contents) ->
           (rebuild op (mcase.inj contents)))
    }

  let reveal_case = make_manager_case 7 Manager_operations.reveal_case
  let transaction_case = make_manager_case 8 Manager_operations.transaction_case
  let origination_case = make_manager_case 9 Manager_operations.origination_case
  let delegation_case = make_manager_case 10 Manager_operations.delegation_case

  let contents_encoding =
    let make (Case { tag ; name ; encoding ; select ; proj ; inj }) =
      case (Tag tag) name encoding
        (fun o -> match select o with None -> None | Some o -> Some (proj o))
        (fun x -> Contents (inj x)) in
    def "operation.alpha.contents" @@
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

  let contents_list_encoding =
    conv to_list of_list (Variable.list contents_encoding)

  let optional_signature_encoding =
    conv
      (function Some s -> s | None -> Signature.zero)
      (fun s -> if Signature.equal s Signature.zero then None else Some s)
      Signature.encoding

  let protocol_data_encoding =
    def "operation.alpha.contents_and_signature" @@
    conv
      (fun (Operation_data { contents ; signature }) ->
         (Contents_list contents, signature))
      (fun (Contents_list contents, signature) ->
         Operation_data { contents ; signature })
      (obj2
         (req "contents" contents_list_encoding)
         (req "signature" optional_signature_encoding))

  let operation_encoding =
    conv
      (fun ({ shell ; protocol_data }) ->
         (shell, protocol_data))
      (fun (shell, protocol_data) ->
         { shell ; protocol_data })
      (merge_objs
         Operation.shell_header_encoding
         protocol_data_encoding)

  let unsigned_operation_encoding =
    def "operation.alpha.unsigned_operation" @@
    merge_objs
      Operation.shell_header_encoding
      (obj1 (req "contents" contents_list_encoding))

  let internal_operation_encoding =
    def "operation.alpha.internal_operation" @@
    conv
      (fun (Internal_operation { source ; operation ; nonce }) ->
         ((source, nonce), Manager operation))
      (fun ((source, nonce), Manager operation) ->
         Internal_operation { source ; operation ; nonce })
      (merge_objs
         (obj2
            (req "source" Contract_repr.encoding)
            (req "nonce" uint16))
         Manager_operations.encoding)

end

let encoding = Encoding.operation_encoding
let contents_encoding = Encoding.contents_encoding
let contents_list_encoding = Encoding.contents_list_encoding
let protocol_data_encoding = Encoding.protocol_data_encoding
let unsigned_operation_encoding = Encoding.unsigned_operation_encoding
let internal_operation_encoding = Encoding.internal_operation_encoding

let raw ({ shell ; protocol_data } : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data protocol_data) in
  { Operation.shell ; proto }

let acceptable_passes (op : packed_operation) =
  let Operation_data protocol_data = op.protocol_data in
  match protocol_data.contents with

  | Single (Endorsement _) -> [0]

  | Single (Proposals _ ) -> [1]
  | Single (Ballot _ ) -> [1]

  | Single (Seed_nonce_revelation _) -> [2]
  | Single (Double_endorsement_evidence _) -> [2]
  | Single (Double_baking_evidence _) -> [2]
  | Single (Activate_account _) -> [2]

  | Single (Manager_operation _) -> [3]
  | Cons _ -> [3]

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

let check_signature_sync (type kind) key chain_id ({ shell ; protocol_data } : kind operation) =
  let check ~watermark contents signature =
    let unsigned_operation =
      Data_encoding.Binary.to_bytes_exn
        unsigned_operation_encoding (shell, contents) in
    if Signature.check ~watermark key signature unsigned_operation then
      Ok ()
    else
      Error [Invalid_signature] in
  match protocol_data.contents, protocol_data.signature with
  | Single _, None ->
      Error [Missing_signature]
  | Cons _, None ->
      Error [Missing_signature]
  | Single (Endorsement _) as contents, Some signature ->
      check ~watermark:(Endorsement chain_id) (Contents_list contents) signature
  | Single _ as contents, Some signature ->
      check ~watermark:Generic_operation (Contents_list contents) signature
  | Cons _ as contents, Some signature ->
      check ~watermark:Generic_operation (Contents_list contents) signature

let check_signature pk chain_id op =
  Lwt.return (check_signature_sync pk chain_id op)

let hash_raw = Operation.hash
let hash (o : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data o.protocol_data) in
  Operation.hash { shell = o.shell ; proto }
let hash_packed (o : packed_operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      o.protocol_data in
  Operation.hash { shell = o.shell ; proto }

type ('a, 'b) eq = Eq : ('a, 'a) eq

let equal_manager_operation_kind
  : type a b. a manager_operation -> b manager_operation -> (a, b) eq option
  = fun op1 op2 ->
    match op1, op2 with
    | Reveal _, Reveal _ -> Some Eq
    | Reveal _, _ -> None
    | Transaction _, Transaction _ -> Some Eq
    | Transaction _, _ -> None
    | Origination _, Origination _ -> Some Eq
    | Origination _, _ -> None
    | Delegation _, Delegation _ -> Some Eq
    | Delegation _, _ -> None

let equal_contents_kind
  : type a b. a contents -> b contents -> (a, b) eq option
  = fun op1 op2 ->
    match op1, op2 with
    | Endorsement _, Endorsement _ -> Some Eq
    | Endorsement _, _ -> None
    | Seed_nonce_revelation _, Seed_nonce_revelation _ -> Some Eq
    | Seed_nonce_revelation _, _ -> None
    | Double_endorsement_evidence _, Double_endorsement_evidence _ -> Some Eq
    | Double_endorsement_evidence _, _ -> None
    | Double_baking_evidence _, Double_baking_evidence _ -> Some Eq
    | Double_baking_evidence _, _ -> None
    | Activate_account _, Activate_account _ -> Some Eq
    | Activate_account _, _ -> None
    | Proposals _, Proposals _ -> Some Eq
    | Proposals _, _ -> None
    | Ballot _, Ballot _ -> Some Eq
    | Ballot _, _ -> None
    | Manager_operation op1, Manager_operation op2 -> begin
        match equal_manager_operation_kind op1.operation op2.operation with
        | None -> None
        | Some Eq -> Some Eq
      end
    | Manager_operation _, _ -> None

let rec equal_contents_kind_list
  : type a b. a contents_list -> b contents_list -> (a, b) eq option
  = fun op1 op2 ->
    match op1, op2 with
    | Single op1, Single op2 ->
        equal_contents_kind op1 op2
    | Single _, Cons _ -> None
    | Cons _, Single _ -> None
    | Cons (op1, ops1), Cons (op2, ops2) -> begin
        match equal_contents_kind op1 op2 with
        | None -> None
        | Some Eq ->
            match equal_contents_kind_list ops1 ops2 with
            | None -> None
            | Some Eq -> Some Eq
      end

let equal
  : type a b. a operation -> b operation -> (a, b) eq option
  = fun op1 op2 ->
    if not (Operation_hash.equal (hash op1) (hash op2)) then
      None
    else
      equal_contents_kind_list
        op1.protocol_data.contents op2.protocol_data.contents
