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

let sourced ops = Sourced_operations ops

let manager (src : Helpers_account.t) ?(fee = Tez.zero) operations context gas_limit =
  Alpha_context.prepare
    ~level:0l ~timestamp:(Time.now ()) ~fitness:[] context >>=? fun context ->
  Contract.get_counter context src.contract >>=? fun counter ->
  Contract.is_manager_key_revealed context src.contract >>=? fun revealed ->
  let counter = Int32.succ counter in
  return @@
  Manager_operations {
    source = src.contract ;
    fee ;
    counter ;
    operations = (if revealed then operations else Reveal src.pub :: operations) ;
    gas_limit ;
  }


let manager_full src ?(fee = Tez.zero) ops context gas_limit =
  manager src ~fee ops context gas_limit >>=? fun ops -> return @@ sourced ops


let transaction ?parameters amount destination =
  Transaction {
    amount ;
    parameters ;
    destination ;
  }


let origination
    ?(delegatable = true) ?(script = None)
    ?(spendable = true) ?(delegate = None)
    (manager: Helpers_account.t) credit
  =
  Origination {
    manager = manager.hpub ;
    delegate ;
    spendable ;
    delegatable ;
    script ;
    credit ;
    preorigination = None ;
  }


let delegation delegate =
  Delegation (Some delegate)


let delegation_full ?(fee = Tez.zero) src delegate context =
  manager_full src ~fee [delegation delegate] context Z.zero


let script_origination_full script src credit gas_limit context =
  manager_full src ~fee: Tez.zero [origination ~script src credit] context gas_limit


let origination_full ?(spendable = true) ?(delegatable = true) ?(fee = Tez.zero) src credit gas_limit context =
  manager_full src ~fee [origination ~spendable ~delegatable src credit] context gas_limit


let transaction_full ?(fee = Tez.zero) ?parameters src dst amount gas_limit context =
  manager src ~fee [transaction ?parameters amount dst] context gas_limit
  >>=? fun manager_op ->
  return @@ sourced manager_op


let amendment_operation (src: Helpers_account.t) operation =
  Amendment_operation {
    source = src.hpub ;
    operation
  }

let endorsements ?(slot = 0) block level =
  Endorsements {
    block ;
    level ;
    slots = [slot] ;
  }


let endorsement_full ?(slot = 0) block level =
  sourced
  @@ Consensus_operation (endorsements block level ~slot)

let sign src oph protop =
  let watermark =
    match protop with
    | Proto_alpha.Alpha_context.Anonymous_operations _ -> None
    | Proto_alpha.Alpha_context.Sourced_operations
        (Proto_alpha.Alpha_context.Consensus_operation (Endorsements _)) ->
        Some Signature.Endorsement
    | _ ->
        Some Generic_operation in
  let bytes = Operation.forge oph protop in
  match src with
  | None -> bytes, None
  | Some src ->
      let signature =
        Signature.sign ?watermark src.Helpers_account.ppk bytes in
      Signature.concat bytes signature, Some signature

let main_of_proto (src: Helpers_account.t) operation_header protocol_operation =
  let (proto,_) = sign (Some src) operation_header protocol_operation in
  let data_operation: Tezos_base.Operation.t =
    {shell = operation_header ; proto} in
  let hash = Tezos_base.Operation.hash data_operation in
  Proto_alpha.Main.parse_operation hash data_operation >>? fun op ->
  ok (op, hash)

let apply_of_proto
    (source: Helpers_account.t option) operation_header protocol_operation =
  let (_proto, signature) = sign source operation_header protocol_operation in
  {
    shell = operation_header ;
    contents = protocol_operation ;
    signature
  }
