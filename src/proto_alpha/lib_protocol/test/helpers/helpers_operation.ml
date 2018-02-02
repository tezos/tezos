(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context
open Proto_alpha.Environment.Error_monad

let sourced ops = Sourced_operations ops

let manager (src : Helpers_account.t) ?(fee = Tez.zero) operations context =
  Helpers_misc.get_dummy_tezos_context context >>=? fun context ->
  Contract.get_counter context src.contract >>=? fun counter ->
  let counter = Int32.succ counter in
  return @@
  Manager_operations {
    source = src.contract ;
    public_key = Some src.pub ;
    fee ;
    counter ;
    operations
  }


let manager_full src ?(fee = Tez.zero) ops context =
  manager src ~fee ops context >>=? fun ops -> return @@ sourced ops


let transaction ?(parameters = None) amount destination =
  Transaction {
    amount ;
    parameters ;
    destination
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
    credit
  }


let delegation delegate =
  Delegation (Some delegate)


let delegation_full ?(fee = Tez.zero) src delegate context =
  manager_full src ~fee [delegation delegate] context


let script_origination_full script src credit context =
  manager_full src ~fee: Tez.zero [origination ~script src credit] context


let origination_full ?(spendable = true) ?(delegatable = true) ?(fee = Tez.zero) src credit context =
  manager_full src ~fee [origination ~spendable ~delegatable src credit] context


let transaction_full ?(fee = Tez.zero) src dst amount context =
  manager src ~fee [transaction amount dst] context
  >>=? fun manager_op ->
  return @@ sourced manager_op


let delegate (src: Helpers_account.t) operations =
  Delegate_operations {
    source = src.pub ;
    operations
  }


let endorsement ?(slot = 0) block =
  Endorsement {
    block ;
    slot
  }


let endorsement_full ?(slot = 0) src block =
  sourced
  @@ delegate
    src
    [endorsement block ~slot]


let sign src oph protop =
  let signature_content = Operation.forge oph protop in
  let signature = match src with
    | None -> None
    | Some(src: Helpers_account.t) -> Some (Ed25519.sign src.ppk signature_content) in
  let open Data_encoding in
  let signed_proto_operation_encoding =
    Data_encoding.merge_objs
      Operation.proto_operation_encoding
      (obj1 @@ varopt "signature" Ed25519.Signature.encoding) in
  let proto_bytes =
    Data_encoding.Binary.to_bytes
      signed_proto_operation_encoding
      (protop, signature) in
  (proto_bytes, signature)


let main_of_proto (src: Helpers_account.t) operation_header protocol_operation =
  let (proto,_) = sign (Some src) operation_header protocol_operation in
  let data_operation: Tezos_base.Operation.t =
    {shell = operation_header ; proto} in
  let hash = Tezos_base.Operation.hash data_operation in
  Proto_alpha.Main.parse_operation hash data_operation >>? fun op ->
  ok (op, hash)


let apply_of_proto
    (source: Helpers_account.t option) operation_header protocol_operation =
  let (proto, signature) = sign source operation_header protocol_operation in
  let data_operation: Tezos_base.Operation.t =
    {shell = operation_header ; proto} in
  let hash = Tezos_base.Operation.hash data_operation in
  {
    hash ;
    shell = operation_header ;
    contents = protocol_operation ;
    signature
  }

