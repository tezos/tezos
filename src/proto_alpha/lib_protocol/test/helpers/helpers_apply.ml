(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Error_monad
open Proto_alpha.Apply_operation_result

let extract_result = function
  | Sourced_operation_result (Manager_operations_result { operation_results }) ->
      List.fold_left
        (fun (acc, err) (_, r) ->
           match r with
           | Applied (Transaction_result { originated_contracts }
                     | Origination_result { originated_contracts }) ->
               (originated_contracts @ acc, err)
           | Applied Reveal_result
           | Applied Delegation_result
           | Skipped -> (acc, err)
           | Failed errs -> (acc, Some errs))
        ([], None) operation_results
  | _ -> ([], None)

let bind_result (tc, result) =
  match extract_result result with
  | _, Some err ->
      Lwt.return (Error err)
  | contracts, None ->
      return (contracts, tc)

let operation
    ~tc ?(src: Helpers_account.t option)
    pred_block_hash op_sh proto_op =
  return @@ Helpers_operation.apply_of_proto src op_sh proto_op >>=? fun operation ->
  let hash = Proto_alpha.Alpha_context.Operation.hash operation in
  Proto_alpha.Apply.apply_operation
    tc
    pred_block_hash
    0
    hash
    operation
  >>=? bind_result


let transaction ~tc ?(fee = 0)
    pbh opsh src (dst: Helpers_account.t)
    amount =
  Helpers_operation.transaction_full
    src dst.contract
    (Helpers_cast.cents_of_int amount)
    ~fee: (Helpers_cast.cents_of_int fee)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ~src pbh opsh protop


let transaction_pred ?tc ~(pred: Helpers_block.result) (src, dst, amount, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  let fee = Option.unopt ~default:10 fee in
  transaction ~tc ~fee pred.hash (Helpers_block.get_op_header_res pred) src dst amount


let script_origination
    ~tc pbh opsh script src amount =
  Helpers_operation.script_origination_full
    script src (Helpers_cast.cents_of_int amount)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop -> operation ~tc ~src pbh opsh protop


let origination
    ~tc ?(spendable = true) ?(fee = 0) ?(delegatable = true)
    pbh opsh src amount =
  Helpers_operation.origination_full
    src ~spendable ~delegatable
    (Helpers_cast.cents_of_int amount) ~fee:(Helpers_cast.tez_of_int fee)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ~src pbh opsh protop


let script_origination_pred
    ?tc ~(pred: Helpers_block.result) (script, src, amount) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  script_origination ~tc pred.hash (Helpers_block.get_op_header_res pred) (Some script) src amount


let origination_pred
    ?tc ~(pred: Helpers_block.result) (src, amount, spendable, delegatable, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  origination ~tc ~spendable ~fee ~delegatable
    pred.hash
    (Helpers_block.get_op_header_res pred)
    src amount


let delegation ~tc ?(fee = 0) pbh opsh src delegate =
  Helpers_operation.delegation_full
    src delegate ~fee:(Helpers_cast.cents_of_int fee)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ~src pbh opsh protop


let delegation_pred
    ?tc ~(pred: Helpers_block.result) (src, delegate, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  delegation ~tc ~fee pred.hash (Helpers_block.get_op_header_res pred) src delegate


