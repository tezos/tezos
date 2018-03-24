(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Error_monad
open Alpha_context

let init_amount = 20000

let execute_code_pred
    ?tc (pred : Helpers_block.result) (script : Script.t) (argument : Script.expr) =
  let op = List.nth Helpers_account.bootstrap_accounts 0 in
  let tc = Option.unopt ~default:pred.tezos_context tc in
  Helpers_apply.script_origination_pred ~tc ~pred (script, op, init_amount)
  >>=? fun ((dst, _), tc) ->
  let dst = List.hd dst in
  let ctxt = Helpers_cast.ctxt_of_tc tc in
  let gas = Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc in
  Helpers_operation.transaction_full op dst Tez.zero gas ctxt
  >>=? fun dummy_protop ->
  let op_header = Helpers_block.get_op_header_res pred in
  let apply_op = Helpers_operation.apply_of_proto
      (Some op) op_header dummy_protop in
  let hash = Operation.hash apply_op in
  let dummy_nonce = Contract.initial_origination_nonce hash in
  let amount = Tez.zero in
  Lwt.return (Proto_alpha.Alpha_context.Gas.set_limit tc gas) >>=? fun tc ->
  let return = Script_interpreter.execute
      dummy_nonce op.contract dst
      tc script amount argument in
  return


