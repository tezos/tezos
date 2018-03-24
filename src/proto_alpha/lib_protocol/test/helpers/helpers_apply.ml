(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Error_monad

let operation
    ~tc ?(baker: Helpers_account.t option) ?(src: Helpers_account.t option)
    pred_block_hash op_sh proto_op =
  return @@ Helpers_operation.apply_of_proto src op_sh proto_op >>=? fun operation ->
  let hash = Proto_alpha.Alpha_context.Operation.hash operation in
  Proto_alpha.Apply.apply_operation
    tc
    (Option.map ~f:(fun x -> x.Helpers_account.hpub) baker)
    pred_block_hash
    0
    hash
    operation >>=? fun (tc, _, contracts, err) ->
  return ((contracts, err), tc)


let transaction ~tc ?(fee = 0) ?baker
    pbh opsh src (dst: Helpers_account.t)
    amount =
  Helpers_operation.transaction_full
    src dst.contract
    (Helpers_cast.cents_of_int amount)
    ~fee: (Helpers_cast.cents_of_int fee)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ?baker ~src pbh opsh protop


let transaction_pred ?tc ~(pred: Helpers_block.result) ?baker (src, dst, amount, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  let fee = Option.unopt ~default:10 fee in
  transaction ~tc ~fee ?baker pred.hash (Helpers_block.get_op_header_res pred) src dst amount


let script_origination
    ~tc pbh opsh script src amount =
  Helpers_operation.script_origination_full
    script src (Helpers_cast.cents_of_int amount)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop -> operation ~tc ?baker: None ~src pbh opsh protop


let origination
    ~tc ?baker ?(spendable = true) ?(fee = 0) ?(delegatable = true)
    pbh opsh src amount =
  Helpers_operation.origination_full
    src ~spendable ~delegatable
    (Helpers_cast.cents_of_int amount) ~fee:(Helpers_cast.tez_of_int fee)
    (Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ?baker ~src pbh opsh protop


let script_origination_pred
    ?tc ~(pred: Helpers_block.result) (script, src, amount) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  script_origination ~tc pred.hash (Helpers_block.get_op_header_res pred) (Some script) src amount


let origination_pred
    ?tc ?baker ~(pred: Helpers_block.result) (src, amount, spendable, delegatable, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  origination ~tc ?baker ~spendable ~fee ~delegatable
    pred.hash
    (Helpers_block.get_op_header_res pred)
    src amount


let delegation ~tc ?baker ?(fee = 0) pbh opsh src delegate =
  Helpers_operation.delegation_full
    src delegate ~fee:(Helpers_cast.cents_of_int fee)
  @@ Helpers_cast.ctxt_of_tc tc
  >>=? fun protop ->
  operation ~tc ?baker ~src pbh opsh protop


let delegation_pred
    ?tc ?baker ~(pred: Helpers_block.result) (src, delegate, fee) =
  let tc = Option.unopt ~default:pred.tezos_context tc in
  delegation ~tc ?baker ~fee pred.hash (Helpers_block.get_op_header_res pred) src delegate


