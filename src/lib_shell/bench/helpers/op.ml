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

open Proto_alpha
open Alpha_context

let sign ?(watermark = Signature.Generic_operation)
    sk ctxt contents =
  let branch = Context.branch ctxt in
  let unsigned =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, Contents_list contents) in
  let signature = Some (Signature.sign ~watermark sk unsigned) in
  ({ shell = { branch } ;
     protocol_data = {
       contents ;
       signature ;
     } ;
   } : _ Operation.t)

let endorsement ?delegate ?level ctxt =
  fun ?(signing_context=ctxt) slots ->
    begin
      match delegate with
      | None -> Context.get_endorser ctxt (List.hd slots)
      | Some delegate -> return delegate
    end >>=? fun delegate_pkh ->
    Account.find delegate_pkh >>=? fun delegate ->
    begin
      match level with
      | None -> Context.get_level ctxt
      | Some level -> return level
    end >>=? fun level ->
    let op =
      Single
        (Endorsement { level }) in
    return (sign ~watermark:(Signature.Endorsement Chain_id.zero)
              delegate.sk signing_context op)

let sign ?watermark sk ctxt (Contents_list contents) =
  Operation.pack (sign ?watermark sk ctxt contents)

let manager_operation
    ?(fee = Tez.zero)
    ?(gas_limit = Constants_repr.default.hard_gas_limit_per_operation)
    ?(storage_limit = Constants_repr.default.hard_storage_limit_per_operation)
    ?public_key ~source ctxt operation =
  Context.Contract.counter ctxt source >>=? fun counter ->
  Context.Contract.manager ctxt source >>=? fun account ->
  let public_key = Option.unopt ~default:account.pk public_key in
  let counter = Z.succ counter in
  Context.Contract.is_manager_key_revealed ctxt source >>=? function
  | true ->
      let op =
        Manager_operation {
          source ;
          fee ;
          counter ;
          operation ;
          gas_limit ;
          storage_limit ;
        } in
      return (Contents_list (Single op))
  | false ->
      let op_reveal =
        Manager_operation {
          source ;
          fee = Tez.zero ;
          counter ;
          operation = Reveal public_key ;
          gas_limit = Z.of_int 20 ;
          storage_limit = Z.zero ;
        } in
      let op =
        Manager_operation {
          source ;
          fee ;
          counter = Z.succ counter ;
          operation ;
          gas_limit ;
          storage_limit ;
        } in
      return (Contents_list (Cons (op_reveal, Single op)))

let revelation ctxt public_key =
  let pkh = Signature.Public_key.hash public_key in
  let source = Contract.implicit_contract pkh in
  Context.Contract.counter ctxt source >>=? fun counter ->
  Context.Contract.manager ctxt source >>=? fun account ->
  let counter = Z.succ counter in
  let sop =
    Contents_list
      (Single
         (Manager_operation {
             source ;
             fee = Tez.zero ;
             counter ;
             operation = Reveal public_key ;
             gas_limit = Z.of_int 20 ;
             storage_limit = Z.zero ;
           })) in
  return @@ sign account.sk ctxt sop

let originated_contract (op: Operation.packed) =
  let nonce = Contract.initial_origination_nonce (Operation.hash_packed op) in
  Contract.originated_contract nonce

exception Impossible

let origination ?delegate ?script
    ?(spendable = true) ?(delegatable = true) ?(preorigination = None)
    ?public_key ?manager ?credit ?fee ?gas_limit ?storage_limit ctxt source =
  Context.Contract.manager ctxt source >>=? fun account ->
  let manager = Option.unopt ~default:account.pkh manager in
  let default_credit = Tez.of_mutez @@ Int64.of_int 1000001 in
  let default_credit = Option.unopt_exn Impossible default_credit in
  let credit = Option.unopt ~default:default_credit credit in
  let operation =
    Origination {
      manager ;
      delegate ;
      script ;
      spendable ;
      delegatable ;
      credit ;
      preorigination ;
    } in
  manager_operation ?public_key ?fee ?gas_limit ?storage_limit
    ~source ctxt operation >>=? fun sop ->
  let op = sign account.sk ctxt sop in
  return (op , originated_contract op)

let miss_signed_endorsement ?level ctxt slot =
  begin
    match level with
    | None -> Context.get_level ctxt
    | Some level -> return level
  end >>=? fun level ->
  Context.get_endorser ctxt slot >>=? fun real_delegate_pkh ->
  let delegate = Account.find_alternate real_delegate_pkh in
  endorsement ~delegate:delegate.pkh ~level ctxt [slot]

let transaction ?fee ?gas_limit ?storage_limit ?parameters ctxt
    (src:Contract.t) (dst:Contract.t)
    (amount:Tez.t) =
  let top = Transaction {
      amount;
      parameters;
      destination=dst;
    } in
  manager_operation ?fee ?gas_limit ?storage_limit
    ~source:src ctxt top >>=? fun sop ->
  Context.Contract.manager ctxt src >>=? fun account ->
  return @@ sign account.sk ctxt sop

let delegation ?fee ctxt source dst =
  let top = Delegation dst in
  manager_operation ?fee ~source ctxt top >>=? fun sop ->
  Context.Contract.manager ctxt source >>=? fun account ->
  return @@ sign account.sk ctxt sop

let activation ctxt (pkh : Signature.Public_key_hash.t) activation_code =
  begin match pkh with
    | Ed25519 edpkh -> return edpkh
    | _ -> failwith "Wrong public key hash : %a - Commitments must be activated with an Ed25519 \
                     encrypted public key hash" Signature.Public_key_hash.pp pkh
  end >>=? fun id ->
  let contents =
    Single (Activate_account { id ; activation_code } ) in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = Operation_data {
        contents ;
        signature = None ;
      } ;
  }

let double_endorsement ctxt op1 op2 =
  let contents =
    Single (Double_endorsement_evidence {op1 ; op2}) in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = Operation_data {
        contents ;
        signature = None ;
      } ;
  }

let double_baking ctxt bh1 bh2 =
  let contents =
    Single (Double_baking_evidence {bh1 ; bh2}) in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = Operation_data {
        contents ;
        signature = None ;
      } ;
  }
