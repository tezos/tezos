(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

open Alpha_context

type config =
  { minimal_fees : Alpha_context.Tez.t ;
    minimal_fees_per_gas_unit : Alpha_context.Tez.t ;
    minimal_fees_per_byte : Alpha_context.Tez.t ;
    allow_script_failure : bool ;
    implicit_account_minimal_balance : Alpha_context.Tez.t }

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { minimal_fees ;
           minimal_fees_per_gas_unit ;
           minimal_fees_per_byte ;
           allow_script_failure ;
           implicit_account_minimal_balance } ->
      (minimal_fees,
       minimal_fees_per_gas_unit,
       minimal_fees_per_byte,
       allow_script_failure,
       implicit_account_minimal_balance))
    (fun (minimal_fees,
          minimal_fees_per_gas_unit,
          minimal_fees_per_byte,
          allow_script_failure,
          implicit_account_minimal_balance) ->
      { minimal_fees ;
        minimal_fees_per_gas_unit ;
        minimal_fees_per_byte ;
        allow_script_failure ;
        implicit_account_minimal_balance })
    (obj5
       (dft "minimal_fees" Tez.encoding Tez.zero)
       (dft "minimal_fees_per_gas_unit" Tez.encoding Tez.zero)
       (dft "minimal_fees_per_byte" Tez.encoding Tez.zero)
       (dft "allow_script_failure" bool true)
       (dft "implicit_account_minimal_balance" Tez.encoding Tez.zero))

let default_config =
  { minimal_fees = Tez.zero ;
    minimal_fees_per_gas_unit = Tez.zero ;
    minimal_fees_per_byte = Tez.zero ;
    allow_script_failure = true ;
    implicit_account_minimal_balance =
      Option.unopt_exn (Failure "tez invalid conversion") (Tez.of_mutez 257_000L) ;
  }

module Proto = Tezos_embedded_protocol_002_PsYLVpVv.Registerer.Registered

let rec pre_filter_manager
  : type t. config -> t Kind.manager contents_list -> bool
  = fun config op -> match op with
    | Single (Manager_operation { fee ; gas_limit } as op) ->
        let bytes =
          Data_encoding.Binary.length
            Operation.contents_encoding
            (Contents op) in
        begin match Tez.(fee /? Int64.of_int bytes) with
          | Ok fee_per_byte -> Tez.(fee_per_byte >= config.minimal_fees_per_byte)
          | Error _ -> false
        end
        && (Z.equal gas_limit Z.zero || begin match Tez.(fee /? Z.to_int64 gas_limit) with
            | Ok fee_per_gas_unit ->
                Tez.(fee_per_gas_unit >= config.minimal_fees_per_gas_unit)
            | Error _ -> false
            | exception _ -> false
          end)
        && Tez.(fee >= config.minimal_fees)
    | Cons (Manager_operation op, rest) ->
        pre_filter_manager config (Single (Manager_operation op))
        && pre_filter_manager config rest

let pre_filter config (Operation_data { contents } : Operation.packed_protocol_data) =
  match contents with
  | Single (Endorsement _) -> true
  | Single (Seed_nonce_revelation _) -> true
  | Single (Double_endorsement_evidence _) -> true
  | Single (Double_baking_evidence _) -> true
  | Single (Activate_account _) -> true
  | Single (Proposals _) -> true
  | Single (Ballot _) -> true
  | Single (Manager_operation _) as op -> pre_filter_manager config op
  | Cons (Manager_operation _, _) as op -> pre_filter_manager config op

open Apply_results

let apply_results_to_list contents_result =
  let open Apply_results in
  let rec to_list acc = function
    | Contents_result_list (Single_result o) -> [Contents_result o]
    | Contents_result_list (Cons_result (o, os)) ->
        to_list (Contents_result o :: acc) (Contents_result_list os)
  in
  List.rev (to_list [] contents_result)

let ensure_reserve post_ctxt contents min_amount =
  (* Ensure that it is a transaction *)
  let balances = Signature.Public_key_hash.Map.empty in
  let open Apply_results in
  let collect_balances balances balance_updates =
    fold_left_s (fun balances ->
        let open Delegate in function
          | (Contract c, _) ->
              begin match Contract.is_implicit c with
                | Some pkh ->
                    begin match Signature.Public_key_hash.Map.find_opt pkh balances with
                      | Some _ -> return balances
                      | None -> Alpha_context.Delegate.full_balance post_ctxt pkh >>=
                          function
                          | Ok final_balance ->
                              return (Signature.Public_key_hash.Map.add pkh final_balance balances)
                          | Error _ -> return balances
                    end
                | None -> return balances end
          | _ -> return balances)
      balances balance_updates
  in
  let results = apply_results_to_list (Contents_result_list contents) in
  begin fold_left_s (fun balances -> function
      | Contents_result
          (Manager_operation_result
             {balance_updates ; operation_result ; internal_operation_results }) ->

          collect_balances balances balance_updates >>=? fun balances ->

          begin match operation_result with
            | Applied (Transaction_result { balance_updates }) ->
                collect_balances balances balance_updates
            | _ -> return balances
          end >>=? fun balances ->

          fold_left_s (fun balances -> function
              | Internal_operation_result (_, Applied (Transaction_result { balance_updates })) ->
                  collect_balances balances balance_updates
              | _ -> return balances
            ) balances internal_operation_results

      | _ -> return balances
    ) balances results
  end >>=? fun balances ->
  let insufficient_reserve_pkh_list =
    Signature.Public_key_hash.Map.fold (fun pkh resulting_balance acc ->
        if Tez.(resulting_balance < min_amount) then
          pkh :: acc
        else
          acc
      ) balances []
  in
  return insufficient_reserve_pkh_list

let rec post_filter_manager
  : type t. Alpha_context.t -> t Kind.manager contents_result_list -> config -> bool Lwt.t
  = fun ctxt op config -> match op with
    | Single_result (Manager_operation_result { operation_result }) ->
        begin match operation_result with
          | Applied _ ->
              ensure_reserve ctxt op config.implicit_account_minimal_balance >>= begin function
                | Ok [] ->
                    Lwt.return_true
                | Ok (_ :: _) | Error _ ->
                    Lwt.return_false
              end
          | Skipped _ | Failed _ | Backtracked _ ->
              Lwt.return config.allow_script_failure
        end
    | Cons_result (Manager_operation_result res, rest) ->
        post_filter_manager ctxt (Single_result (Manager_operation_result res)) config >>= function
        | false -> Lwt.return_false
        | true -> post_filter_manager ctxt rest config

let post_filter config
    ~validation_state_before:_
    ~validation_state_after: ({ Main.ctxt } : Proto.validation_state)
    (_op, receipt) =
  match receipt with
  | No_operation_metadata -> assert false (* only for multipass validator *)
  | Operation_metadata { contents } ->
      match contents with
      | Single_result (Endorsement_result _) -> Lwt.return_true
      | Single_result (Seed_nonce_revelation_result _) -> Lwt.return_true
      | Single_result (Double_endorsement_evidence_result _) -> Lwt.return_true
      | Single_result (Double_baking_evidence_result _) -> Lwt.return_true
      | Single_result (Activate_account_result _) -> Lwt.return_true
      | Single_result (Proposals_result) -> Lwt.return_true
      | Single_result (Ballot_result) -> Lwt.return_true
      | Single_result (Manager_operation_result _) as op -> post_filter_manager ctxt op config
      | Cons_result (Manager_operation_result _, _) as op -> post_filter_manager ctxt op config
