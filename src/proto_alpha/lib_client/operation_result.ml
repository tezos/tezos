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
open Apply_operation_result

let pp_manager_operation_content
    (type kind) source internal pp_result
    ppf (operation, result : kind manager_operation * _) =
  Format.fprintf ppf "@[<v 0>" ;
  begin match operation with
    | Transaction { destination ; amount ; parameters } ->
        Format.fprintf ppf
          "@[<v 2>%s:@,\
           Amount: %s%a@,\
           From: %a@,\
           To: %a"
          (if internal then "Internal transaction" else "Transaction")
          Client_proto_args.tez_sym
          Tez.pp amount
          Contract.pp source
          Contract.pp destination ;
        begin match parameters with
          | None -> ()
          | Some expr ->
              let expr =
                Option.unopt_exn
                  (Failure "ill-serialized argument")
                  (Data_encoding.force_decode expr) in
              Format.fprintf ppf
                "@,Parameter: @[<v 0>%a@]"
                Michelson_v1_printer.print_expr expr
        end ;
        pp_result ppf result ;
        Format.fprintf ppf "@]" ;
    | Origination { manager ; delegate ; credit ; spendable ; delegatable ; script } ->
        Format.fprintf ppf "@[<v 2>%s:@,\
                            From: %a@,\
                            For: %a@,\
                            Credit: %s%a"
          (if internal then "Internal origination" else "Origination")
          Contract.pp source
          Signature.Public_key_hash.pp manager
          Client_proto_args.tez_sym
          Tez.pp credit ;
        begin match script with
          | None -> Format.fprintf ppf "@,No script (accepts all transactions)"
          | Some { code ; storage } ->
              let code =
                Option.unopt_exn
                  (Failure "ill-serialized code")
                  (Data_encoding.force_decode code)
              and storage =
                Option.unopt_exn
                  (Failure "ill-serialized storage")
                  (Data_encoding.force_decode storage) in
              Format.fprintf ppf
                "@,@[<hv 2>Script:@ %a\
                 @,@[<hv 2>Initial storage:@ %a@]"
                Michelson_v1_printer.print_expr code
                Michelson_v1_printer.print_expr storage
        end ;
        begin match delegate with
          | None -> Format.fprintf ppf "@,No delegate for this contract"
          | Some delegate -> Format.fprintf ppf "@,Delegate: %a" Signature.Public_key_hash.pp delegate
        end ;
        if spendable then Format.fprintf ppf "@,Spendable by the manager" ;
        if delegatable then Format.fprintf ppf "@,Delegate can be changed by the manager" ;
        pp_result ppf result ;
        Format.fprintf ppf "@]" ;
    | Reveal key ->
        Format.fprintf ppf
          "@[<v 2>%s of manager public key:@,\
           Contract: %a@,\
           Key: %a%a@]"
          (if internal then "Internal revelation" else "Revelation")
          Contract.pp source
          Signature.Public_key.pp key
          pp_result result
    | Delegation None ->
        Format.fprintf ppf
          "@[<v 2>%s:@,\
           Contract: %a@,\
           To: nobody%a@]"
          (if internal then "Internal Delegation" else "Delegation")
          Contract.pp source
          pp_result result
    | Delegation (Some delegate) ->
        Format.fprintf ppf
          "@[<v 2>%s:@,\
           Contract: %a@,\
           To: %a%a@]"
          (if internal then "Internal Delegation" else "Delegation")
          Contract.pp source
          Signature.Public_key_hash.pp delegate
          pp_result result
  end ;
  Format.fprintf ppf "@]"

let pp_balance_updates ppf = function
  | [] -> ()
  | balance_updates ->
      let balance_updates =
        List.map (fun (balance, update) ->
            let balance = match balance with
              | Contract c ->
                  Format.asprintf "%a" Contract.pp c
              | Rewards (pkh, l) ->
                  Format.asprintf "rewards(%a,%a)"
                    Signature.Public_key_hash.pp pkh Cycle.pp l
              | Fees (pkh, l) ->
                  Format.asprintf "fees(%a,%a)"
                    Signature.Public_key_hash.pp pkh Cycle.pp l
              | Deposits (pkh, l) ->
                  Format.asprintf "deposits(%a,%a)"
                    Signature.Public_key_hash.pp pkh Cycle.pp l in
            (balance, update)) balance_updates in
      let column_size =
        List.fold_left
          (fun acc (balance, _) -> Compare.Int.max acc (String.length balance))
          0 balance_updates in
      let pp_update ppf = function
        | Credited amount -> Format.fprintf ppf "+%s%a" Client_proto_args.tez_sym Tez.pp amount
        | Debited amount -> Format.fprintf ppf "-%s%a" Client_proto_args.tez_sym Tez.pp amount in
      let pp_one ppf (balance, update) =
        let to_fill = column_size + 3 - String.length balance in
        let filler = String.make to_fill '.' in
        Format.fprintf ppf "%s %s %a" balance filler pp_update update in
      Format.fprintf ppf "@[<v 0>%a@]"
        (Format.pp_print_list pp_one) balance_updates

let pp_manager_operation_contents_and_result ppf
    (Manager_operation { source ; fee ; operation ; counter ; gas_limit ; storage_limit },
     Manager_operation_result { balance_updates ; operation_result ;
                                internal_operation_results }) =
  let pp_result (type kind) ppf (result : kind manager_operation_result) =
    Format.fprintf ppf "@," ;
    match result with
    | Skipped _ ->
        Format.fprintf ppf
          "This operation was skipped"
    | Failed (_, _errs) ->
        Format.fprintf ppf
          "This operation FAILED."
    | Applied Reveal_result ->
        Format.fprintf ppf
          "This revelation was successfully applied"
    | Applied Delegation_result ->
        Format.fprintf ppf
          "This delegation was successfully applied"
    | Applied (Transaction_result { balance_updates ; consumed_gas ;
                                    storage ;
                                    originated_contracts ;
                                    storage_size ; paid_storage_size_diff }) ->
        Format.fprintf ppf
          "This transaction was successfully applied" ;
        begin match originated_contracts with
          | [] -> ()
          | contracts ->
              Format.fprintf ppf "@,@[<v 2>Originated contracts:@,%a@]"
                (Format.pp_print_list Contract.pp) contracts
        end ;
        begin match storage with
          | None -> ()
          | Some expr ->
              Format.fprintf ppf "@,@[<hv 2>Updated storage:@ %a@]"
                Michelson_v1_printer.print_expr expr
        end ;
        begin if storage_size <> Z.zero then
            Format.fprintf ppf
              "@,Storage size: %s bytes"
              (Z.to_string storage_size)
        end ;
        begin if paid_storage_size_diff <> Z.zero then
            Format.fprintf ppf
              "@,Paid storage size diff: %s bytes"
              (Z.to_string paid_storage_size_diff)
        end ;
        Format.fprintf ppf
          "@,Consumed gas: %s"
          (Z.to_string consumed_gas) ;
        begin match balance_updates with
          | [] -> ()
          | balance_updates ->
              Format.fprintf ppf
                "@,Balance updates:@,  %a"
                pp_balance_updates balance_updates
        end
    | Applied (Origination_result { balance_updates ; consumed_gas ;
                                    originated_contracts ;
                                    storage_size ; paid_storage_size_diff }) ->
        Format.fprintf ppf
          "This origination was successfully applied" ;
        begin match originated_contracts with
          | [] -> ()
          | contracts ->
              Format.fprintf ppf "@,@[<v 2>Originated contracts:@,%a@]"
                (Format.pp_print_list Contract.pp) contracts
        end ;
        begin if storage_size <> Z.zero then
            Format.fprintf ppf
              "@,Storage size: %s bytes"
              (Z.to_string storage_size)
        end ;
        begin if paid_storage_size_diff <> Z.zero then
            Format.fprintf ppf
              "@,Paid storage size diff: %s bytes"
              (Z.to_string paid_storage_size_diff)
        end ;
        Format.fprintf ppf
          "@,Consumed gas: %s"
          (Z.to_string consumed_gas) ;
        begin match balance_updates with
          | [] -> ()
          | balance_updates ->
              Format.fprintf ppf
                "@,Balance updates:@,  %a"
                pp_balance_updates balance_updates
        end in
  Format.fprintf ppf
    "@[<v 0>@[<v 2>Manager signed operations:@,\
     From: %a@,\
     Fee to the baker: %s%a@,\
     Expected counter: %s@,\
     Gas limit: %s@,\
     Storage limit: %s bytes"
    Contract.pp source
    Client_proto_args.tez_sym
    Tez.pp fee
    (Z.to_string counter)
    (Z.to_string gas_limit)
    (Z.to_string storage_limit) ;
  begin match balance_updates with
    | [] -> ()
    | balance_updates ->
        Format.fprintf ppf
          "@,Balance updates:@,  %a"
          pp_balance_updates balance_updates
  end ;
  Format.fprintf ppf
    "@,%a"
    (pp_manager_operation_content source false pp_result)
    (operation, operation_result) ;
  begin
    match internal_operation_results with
    | [] -> ()
    | _ :: _ ->
        Format.fprintf ppf
          "@,@[<v 2>Internal operations:@ %a@]"
          (Format.pp_print_list
             (fun ppf (Internal_operation_result (op, res)) ->
                pp_manager_operation_content op.source false pp_result
                  ppf (op.operation, res)))
          internal_operation_results
  end ;
  Format.fprintf ppf "@]"

let rec pp_contents_and_result_list :
  type kind. Format.formatter -> kind contents_and_result_list -> unit =
  fun ppf -> function
    | Single_and_result
        (Seed_nonce_revelation { level ; nonce },
         Seed_nonce_revelation_result bus) ->
        Format.fprintf ppf
          "@[<v 2>Seed nonce revelation:@,\
           Level: %a@,\
           Nonce (hash): %a@,\
           Balance updates:@,\
          \  %a@]"
          Raw_level.pp level
          Nonce_hash.pp (Nonce.hash nonce)
          pp_balance_updates bus
    | Single_and_result
        (Double_baking_evidence { bh1 ; bh2 },
         Double_baking_evidence_result bus) ->
        Format.fprintf ppf
          "@[<v 2>Double baking evidence:@,\
           Exhibit A: %a@,\
           Exhibit B: %a@,\
           Balance updates:@,\
          \  %a@]"
          Block_hash.pp (Block_header.hash bh1)
          Block_hash.pp (Block_header.hash bh2)
          pp_balance_updates bus
    | Single_and_result
        (Double_endorsement_evidence { op1 ; op2 },
         Double_endorsement_evidence_result bus) ->
        Format.fprintf ppf
          "@[<v 2>Double endorsement evidence:@,\
           Exhibit A: %a@,\
           Exhibit B: %a@,\
           Balance updates:@,\
          \  %a@]"
          Operation_hash.pp (Operation.hash op1)
          Operation_hash.pp (Operation.hash op2)
          pp_balance_updates bus
    | Single_and_result
        (Activate_account { id ; _ },
         Activate_account_result bus) ->
        Format.fprintf ppf
          "@[<v 2>Genesis account activation:@,\
           Account: %a@,\
           Balance updates:@,\
          \  %a@]"
          Ed25519.Public_key_hash.pp id
          pp_balance_updates bus
    | Single_and_result
        (Endorsement { level },
         Endorsement_result { balance_updates ; delegate ; slots }) ->
        Format.fprintf ppf
          "@[<v 2>Endorsement:@,\
           Level: %a@,\
           Balance updates:%a@,\
           Delegate: %a@,\
           Slots: %a@]"
          Raw_level.pp level
          pp_balance_updates balance_updates
          Signature.Public_key_hash.pp delegate
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_space
             Format.pp_print_int)
          slots
    | Single_and_result
        (Proposals { source ; period ; proposals },
         Proposals_result) ->
        Format.fprintf ppf
          "@[<v 2>Proposals:@,\
           From: %a@,\
           Period: %a@,\
           Protocols:@,\
          \  @[<v 0>%a@]@]"
          Signature.Public_key_hash.pp source
          Voting_period.pp period
          (Format.pp_print_list Protocol_hash.pp) proposals
    | Single_and_result
        (Ballot { source ;period ; proposal ; ballot },
         Ballot_result) ->
        Format.fprintf ppf
          "@[<v 2>Ballot:@,\
           From: %a@,\
           Period: %a@,\
           Protocol: %a@,\
           Vote: %s@]"
          Signature.Public_key_hash.pp source
          Voting_period.pp period
          Protocol_hash.pp proposal
          (match ballot with Yay -> "YAY" | Pass -> "PASS" | Nay -> "NAY")
    | Single_and_result (Manager_operation _ as op,
                         (Manager_operation_result _ as res))->
        Format.fprintf ppf "%a"
          pp_manager_operation_contents_and_result (op, res)
    | Cons_and_result (Manager_operation _ as op,
                       (Manager_operation_result _ as res),
                       rest) ->
        Format.fprintf ppf "%a@\n%a"
          pp_manager_operation_contents_and_result (op, res)
          pp_contents_and_result_list rest

let pp_operation_result ppf
    (op, res : 'kind contents_list * 'kind contents_result_list) =
  Format.fprintf ppf "@[<v 0>" ;
  let contents_and_result_list =
    Apply_operation_result.pack_contents_list op res in
  pp_contents_and_result_list ppf contents_and_result_list ;
  Format.fprintf ppf "@]@."

let pp_internal_operation ppf (Internal_operation { source ; operation }) =
  pp_manager_operation_content source true (fun _ppf () -> ())
    ppf (operation, ())
