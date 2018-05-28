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

let pp_manager_operation_content ppf source operation internal pp_result result =
  Format.fprintf ppf "@[<v 0>" ;
  begin match operation with
    | Alpha_context.Transaction { destination ; amount ; parameters } ->
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

let pp_operation_result ppf ({ contents ; _ }, operation_result) =
  Format.fprintf ppf "@[<v 0>" ;
  begin match contents, operation_result with
    | Anonymous_operations ops, Anonymous_operations_result rs ->
        let ops_rs = List.combine ops rs in
        let pp_anonymous_operation_result ppf = function
          | Seed_nonce_revelation { level ; nonce },
            Seed_nonce_revelation_result bus ->
              Format.fprintf ppf
                "@[<v 2>Seed nonce revelation:@,\
                 Level: %a@,\
                 Nonce (hash): %a@,\
                 Balance updates:@,\
                \  %a@]"
                Raw_level.pp level
                Nonce_hash.pp (Nonce.hash nonce)
                pp_balance_updates bus
          | Double_baking_evidence { bh1 ; bh2 },
            Double_baking_evidence_result bus ->
              Format.fprintf ppf
                "@[<v 2>Double baking evidence:@,\
                 Exhibit A: %a@,\
                 Exhibit B: %a@,\
                 Balance updates:@,\
                \  %a@]"
                Block_hash.pp (Block_header.hash bh1)
                Block_hash.pp (Block_header.hash bh2)
                pp_balance_updates bus
          | Double_endorsement_evidence { op1 ; op2},
            Double_endorsement_evidence_result bus ->
              Format.fprintf ppf
                "@[<v 2>Double endorsement evidence:@,\
                 Exhibit A: %a@,\
                 Exhibit B: %a@,\
                 Balance updates:@,\
                \  %a@]"
                Operation_hash.pp (Operation.hash op1)
                Operation_hash.pp (Operation.hash op2)
                pp_balance_updates bus
          | Activation { id ; _ },
            Activation_result bus ->
              Format.fprintf ppf
                "@[<v 2>Genesis account activation:@,\
                 Account: %a@,\
                 Balance updates:@,\
                \  %a@]"
                Ed25519.Public_key_hash.pp id
                pp_balance_updates bus
          | _, _ -> invalid_arg "Apply_operation_result.pp"
        in
        Format.pp_print_list pp_anonymous_operation_result ppf ops_rs
    | Sourced_operation
        (Consensus_operation
           (Endorsements { block ; level ; slots })),
      Sourced_operation_result
        (Consensus_operation_result
           (Endorsements_result (delegate, _slots))) ->
        Format.fprintf ppf
          "@[<v 2>Endorsement:@,\
           Block: %a@,\
           Level: %a@,\
           Delegate: %a@,\
           Slots: %a@]"
          Block_hash.pp block
          Raw_level.pp level
          Signature.Public_key_hash.pp delegate
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_space
             Format.pp_print_int)
          slots
    | Sourced_operation
        (Amendment_operation { source ; operation = Proposals { period ; proposals } }),
      Sourced_operation_result Amendment_operation_result ->
        Format.fprintf ppf
          "@[<v 2>Proposals:@,\
           From: %a@,\
           Period: %a@,\
           Protocols:@,\
          \  @[<v 0>%a@]@]"
          Signature.Public_key_hash.pp source
          Voting_period.pp period
          (Format.pp_print_list Protocol_hash.pp) proposals
    | Sourced_operation
        (Amendment_operation { source ; operation = Ballot { period ; proposal ; ballot } }),
      Sourced_operation_result Amendment_operation_result ->
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
    | Sourced_operation (Dictator_operation (Activate protocol)),
      Sourced_operation_result Dictator_operation_result ->
        Format.fprintf ppf
          "@[<v 2>Dictator protocol activation:@,\
           Protocol: %a@]"
          Protocol_hash.pp protocol
    | Sourced_operation (Dictator_operation (Activate_testchain protocol)),
      Sourced_operation_result Dictator_operation_result ->
        Format.fprintf ppf
          "@[<v 2>Dictator test protocol activation:@,\
           Protocol: %a@]"
          Protocol_hash.pp protocol
    | Sourced_operation (Manager_operations { source ; fee ; counter ; operations ; gas_limit ; storage_limit }),
      Sourced_operation_result (Manager_operations_result { balance_updates ; operation_results }) ->
        let pp_result ppf result =
          Format.fprintf ppf "@," ;
          match result with
          | Skipped ->
              Format.fprintf ppf
                "This operation was skipped"
          | Failed _errs ->
              Format.fprintf ppf
                "This operation FAILED."
          | Applied Reveal_result ->
              Format.fprintf ppf
                "This revelation was successfully applied"
          | Applied Delegation_result ->
              Format.fprintf ppf
                "This delegation was successfully applied"
          | Applied (Transaction_result { balance_updates ; consumed_gas ;
                                          operations ; storage ;
                                          originated_contracts ; storage_size_diff }) ->
              Format.fprintf ppf
                "This transaction was successfully applied" ;
              begin match operations with
                | [] -> ()
                | ops -> Format.fprintf ppf "@,Internal operations: %d" (List.length ops)
              end ;
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
              begin if storage_size_diff <> 0L then
                  Format.fprintf ppf
                    "@,Storage size difference: %Ld bytes"
                    storage_size_diff
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
                                          originated_contracts ; storage_size_diff }) ->
              Format.fprintf ppf
                "This origination was successfully applied" ;
              begin match originated_contracts with
                | [] -> ()
                | contracts ->
                    Format.fprintf ppf "@,@[<v 2>Originated contracts:@,%a@]"
                      (Format.pp_print_list Contract.pp) contracts
              end ;
              begin if storage_size_diff <> 0L then
                  Format.fprintf ppf
                    "@,Storage size used: %Ld bytes"
                    storage_size_diff
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
        let rec pp_manager_operations_results ppf = function
          | [], [] -> ()
          | operation :: ops, (External, r) :: rs ->
              Format.fprintf ppf "@," ;
              pp_manager_operation_content ppf source operation false pp_result r ;
              pp_manager_operations_results ppf (ops, rs)
          | ops, (Internal { source ; operation }, r) :: rs ->
              Format.fprintf ppf "@," ;
              pp_manager_operation_content ppf source operation true pp_result r ;
              pp_manager_operations_results ppf (ops, rs)
          | [], _ :: _
          | _ :: _, [] -> invalid_arg "Apply_operation_result.pp" in
        Format.fprintf ppf
          "@[<v 0>@[<v 2>Manager signed operations:@,\
           From: %a@,\
           Fee to the baker: %s%a@,\
           Expected counter: %ld@,\
           Gas limit: %s@,\
           Storage limit: %Ld bytes"
          Contract.pp source
          Client_proto_args.tez_sym
          Tez.pp fee
          counter
          (Z.to_string gas_limit)
          storage_limit ;
        begin match balance_updates with
          | [] -> ()
          | balance_updates ->
              Format.fprintf ppf
                "@,Balance updates:@,  %a"
                pp_balance_updates balance_updates
        end ;
        Format.fprintf ppf
          "@]%a@]"
          pp_manager_operations_results (operations, operation_results)
    | _, _ -> invalid_arg "Apply_operation_result.pp"
  end ;
  Format.fprintf ppf "@]"

let pp_internal_operation ppf { source ; operation } =
  pp_manager_operation_content ppf source operation true (fun _ppf () -> ()) ()
