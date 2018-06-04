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

let sign ?(watermark = Signature.Generic_operation) sk ctxt contents =
  let branch = Context.branch ctxt in
  let unsigned =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({ branch }, contents) in
  let signature = Some (Signature.sign ~watermark sk unsigned) in
  { shell = { branch } ;
    protocol_data = {
      contents ;
      signature ;
    } ;
  }

let endorsement ?delegate ?level ctxt =
  fun ?(signing_context=ctxt) slot ->
    begin
      match delegate with
      | None -> Context.get_endorser ctxt slot
      | Some delegate -> return delegate
    end >>=? fun delegate_pkh ->
    Account.find delegate_pkh >>=? fun delegate ->
    begin
      match level with
      | None -> Context.get_level ctxt
      | Some level -> return level
    end >>=? fun level ->
    let op =
      let operations = Endorsements { block = Context.branch ctxt ; level ; slots = [slot] } in
      Sourced_operation (Consensus_operation operations) in
    return (sign ~watermark:Signature.Endorsement delegate.sk signing_context op)

let manager_operations ?(fee = Tez.zero)
    ?(gas_limit = Constants_repr.default.hard_gas_limit_per_operation)
    ?(storage_limit = Constants_repr.default.hard_storage_limit_per_operation)
    ?public_key ~source ctxt operations =
  Context.Contract.counter ctxt source >>=? fun counter ->
  Context.Contract.manager ctxt source >>=? fun account ->
  let public_key = Option.unopt ~default:account.pk public_key in
  let counter = Int32.succ counter in
  Context.Contract.is_manager_key_revealed ctxt source >>=? begin function
    | true -> return operations
    | false -> return @@ (Reveal public_key) :: operations end >>=? fun operations ->
  return @@ Manager_operations {
    source ;
    fee ;
    counter ;
    operations ;
    gas_limit ;
    storage_limit ;
  }

let revelation ctxt public_key =
  let pkh = Signature.Public_key.hash public_key in
  let contract = Contract.implicit_contract pkh in
  manager_operations ~source:contract ~public_key ctxt [] >>=? fun mop ->
  let sop = Sourced_operation mop in
  Context.Contract.manager ctxt contract >>=? fun account ->
  return @@ sign account.sk ctxt sop

let originated_contract (op:Operation.t) =
  let nonce = Contract.initial_origination_nonce (Operation.hash op) in
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
  let operations = [Origination {
      manager ;
      delegate ;
      script ;
      spendable ;
      delegatable ;
      credit ;
      preorigination ;
    }] in
  manager_operations ?public_key ?fee ?gas_limit ?storage_limit
    ~source ctxt operations >>=? fun mop ->
  let sop = Sourced_operation mop in
  let op:Operation.t = sign account.sk ctxt sop in
  return (op , originated_contract op)

let miss_signed_endorsement ?level ctxt slot =
  begin
    match level with
    | None -> Context.get_level ctxt
    | Some level -> return level
  end >>=? fun level ->
  Context.get_endorser ctxt slot >>=? fun real_delegate_pkh ->
  let delegate = Account.find_alternate real_delegate_pkh in
  endorsement ~delegate:delegate.pkh ~level ctxt slot

let transaction ?fee ?gas_limit ?storage_limit ?parameters ctxt
    (src:Contract.t) (dst:Contract.t)
    (amount:Tez.t) =
  let top = Transaction {
      amount;
      parameters;
      destination=dst;
    } in
  manager_operations ?fee ?gas_limit ?storage_limit
    ~source:src ctxt [top] >>=? fun mop ->
  let sop = Sourced_operation mop in
  Context.Contract.manager ctxt src >>=? fun account ->
  return @@ sign account.sk ctxt sop

let delegation ?fee ctxt source dst =
  let top = Delegation dst in
  manager_operations ?fee ~source ctxt [top] >>=? fun mop ->
  let sop = Sourced_operation mop in
  Context.Contract.manager ctxt source >>=? fun account ->
  return @@ sign account.sk ctxt sop

let activation ctxt (pkh : Signature.Public_key_hash.t) activation_code =
  begin match pkh with
    | Ed25519 edpkh -> return edpkh
    | _ -> failwith "Wrong public key hash : %a - Commitments must be activated with an Ed25519 \
                     encrypted public key hash" Signature.Public_key_hash.pp pkh
  end >>=? fun id ->
  let contents =
    Anonymous_operations
      [ Activation { id ; activation_code } ]
  in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = {
      contents ;
      signature = None ;
    } ;
  }

let double_endorsement ctxt op1 op2 =
  let contents =
    Anonymous_operations [
      Double_endorsement_evidence {op1 ; op2}
    ] in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = {
      contents ;
      signature = None ;
    } ;
  }

let double_baking ctxt bh1 bh2 =
  let contents =
    Anonymous_operations [
      Double_baking_evidence {bh1 ; bh2}
    ] in
  let branch = Context.branch ctxt in
  return {
    shell = { branch } ;
    protocol_data = {
      contents ;
      signature = None ;
    } ;
  }
