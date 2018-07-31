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

type error +=
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Counter_in_the_past of Contract_repr.contract * Z.t * Z.t (* `Branch *)
  | Counter_in_the_future of Contract_repr.contract * Z.t * Z.t (* `Temporary *)
  | Unspendable_contract of Contract_repr.contract (* `Permanent *)
  | Non_existing_contract of Contract_repr.contract (* `Temporary *)
  | Empty_implicit_contract of Signature.Public_key_hash.t (* `Temporary *)
  | Empty_transaction of Contract_repr.t (* `Temporary *)
  | Inconsistent_hash of Signature.Public_key.t * Signature.Public_key_hash.t * Signature.Public_key_hash.t (* `Permanent *)
  | Inconsistent_public_key of Signature.Public_key.t * Signature.Public_key.t (* `Permanent *)
  | Failure of string (* `Permanent *)
  | Previously_revealed_key of Contract_repr.t (* `Permanent *)
  | Unrevealed_manager_key of Contract_repr.t (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"contract.unspendable_contract"
    ~title:"Unspendable contract"
    ~description:"An operation tried to spend tokens from an unspendable contract"
    ~pp:(fun ppf c ->
        Format.fprintf ppf "The tokens of contract %a can only be spent by its script"
          Contract_repr.pp c)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Unspendable_contract c   -> Some c | _ -> None)
    (fun c -> Unspendable_contract c) ;
  register_error_kind
    `Temporary
    ~id:"contract.balance_too_low"
    ~title:"Balance too low"
    ~description:"An operation tried to spend more tokens than the contract has"
    ~pp:(fun ppf (c, b, a) ->
        Format.fprintf ppf "Balance of contract %a too low (%a) to spend %a"
          Contract_repr.pp c Tez_repr.pp b Tez_repr.pp a)
    Data_encoding.(obj3
                     (req "contract" Contract_repr.encoding)
                     (req "balance" Tez_repr.encoding)
                     (req "amount" Tez_repr.encoding))
    (function Balance_too_low (c, b, a)   -> Some (c, b, a) | _ -> None)
    (fun (c, b, a) -> Balance_too_low (c, b, a)) ;
  register_error_kind
    `Temporary
    ~id:"contract.counter_in_the_future"
    ~title:"Invalid counter (not yet reached) in a manager operation"
    ~description:"An operation assumed a contract counter in the future"
    ~pp:(fun ppf (contract, exp, found) ->
        Format.fprintf ppf
          "Counter %s not yet reached for contract %a (expected %s)"
          (Z.to_string found)
          Contract_repr.pp contract
          (Z.to_string exp))
    Data_encoding.
      (obj3
         (req "contract" Contract_repr.encoding)
         (req "expected" z)
         (req "found" z))
    (function Counter_in_the_future (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_future (c, x, y)) ;
  register_error_kind
    `Branch
    ~id:"contract.counter_in_the_past"
    ~title:"Invalid counter (already used) in a manager operation"
    ~description:"An operation assumed a contract counter in the past"
    ~pp:(fun ppf (contract, exp, found) ->
        Format.fprintf ppf
          "Counter %s already used for contract %a (expected %s)"
          (Z.to_string found)
          Contract_repr.pp contract
          (Z.to_string exp))
    Data_encoding.
      (obj3
         (req "contract" Contract_repr.encoding)
         (req "expected" z)
         (req "found" z))
    (function Counter_in_the_past (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_past (c, x, y)) ;
  register_error_kind
    `Temporary
    ~id:"contract.non_existing_contract"
    ~title:"Non existing contract"
    ~description:"A contract handle is not present in the context \
                  (either it never was or it has been destroyed)"
    ~pp:(fun ppf contract ->
        Format.fprintf ppf "Contract %a does not exist"
          Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Non_existing_contract c -> Some c | _ -> None)
    (fun c -> Non_existing_contract c) ;
  register_error_kind
    `Permanent
    ~id:"contract.manager.inconsistent_hash"
    ~title:"Inconsistent public key hash"
    ~description:"A revealed manager public key is inconsistent with the announced hash"
    ~pp:(fun ppf (k, eh, ph) ->
        Format.fprintf ppf "The hash of the manager public key %s is not %a as announced but %a"
          (Signature.Public_key.to_b58check k)
          Signature.Public_key_hash.pp ph
          Signature.Public_key_hash.pp eh)
    Data_encoding.(obj3
                     (req "public_key" Signature.Public_key.encoding)
                     (req "expected_hash" Signature.Public_key_hash.encoding)
                     (req "provided_hash" Signature.Public_key_hash.encoding))
    (function Inconsistent_hash (k, eh, ph) -> Some (k, eh, ph) | _ -> None)
    (fun (k, eh, ph) -> Inconsistent_hash (k, eh, ph)) ;
  register_error_kind
    `Permanent
    ~id:"contract.manager.inconsistent_public_key"
    ~title:"Inconsistent public key"
    ~description:"A provided manager public key is different with the public key stored in the contract"
    ~pp:(fun ppf (eh, ph) ->
        Format.fprintf ppf "Expected manager public key %s but %s was provided"
          (Signature.Public_key.to_b58check ph)
          (Signature.Public_key.to_b58check eh))
    Data_encoding.(obj2
                     (req "public_key" Signature.Public_key.encoding)
                     (req "expected_public_key" Signature.Public_key.encoding))
    (function Inconsistent_public_key (eh, ph) -> Some (eh, ph) | _ -> None)
    (fun (eh, ph) -> Inconsistent_public_key (eh, ph)) ;
  register_error_kind
    `Permanent
    ~id:"contract.failure"
    ~title:"Contract storage failure"
    ~description:"Unexpected contract storage error"
    ~pp:(fun ppf s -> Format.fprintf ppf "Contract_storage.Failure %S" s)
    Data_encoding.(obj1 (req "message" string))
    (function Failure s -> Some s | _ -> None)
    (fun s -> Failure s) ;
  register_error_kind
    `Branch
    ~id:"contract.unrevealed_key"
    ~title:"Manager operation precedes key revelation"
    ~description:
      "One tried to apply a manager operation \
       without revealing the manager public key"
    ~pp:(fun ppf s ->
        Format.fprintf ppf "Unrevealed manager key for contract %a."
          Contract_repr.pp s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Unrevealed_manager_key s -> Some s | _ -> None)
    (fun s -> Unrevealed_manager_key s) ;
  register_error_kind
    `Branch
    ~id:"contract.previously_revealed_key"
    ~title:"Manager operation already revealed"
    ~description:
      "One tried to revealed twice a manager public key"
    ~pp:(fun ppf s ->
        Format.fprintf ppf "Previously revealed manager key for contract %a."
          Contract_repr.pp s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Previously_revealed_key s -> Some s | _ -> None)
    (fun s -> Previously_revealed_key s) ;
  register_error_kind
    `Branch
    ~id:"implicit.empty_implicit_contract"
    ~title:"Empty implicit contract"
    ~description:"No manager operations are allowed on an empty implicit contract."
    ~pp:(fun ppf implicit ->
        Format.fprintf ppf
          "Empty implicit contract (%a)"
          Signature.Public_key_hash.pp implicit)
    Data_encoding.(obj1 (req "implicit" Signature.Public_key_hash.encoding))
    (function Empty_implicit_contract c -> Some c | _ -> None)
    (fun c -> Empty_implicit_contract c) ;
  register_error_kind
    `Branch
    ~id:"contract.empty_transaction"
    ~title:"Empty transaction"
    ~description:"Forbidden to credit 0ꜩ to a contract without code."
    ~pp:(fun ppf contract ->
        Format.fprintf ppf
          "Transaction of 0ꜩ towards a contract without code are forbidden (%a)."
          Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Empty_transaction c -> Some c | _ -> None)
    (fun c -> Empty_transaction c)

let failwith msg = fail (Failure msg)

type big_map_diff_item = {
  diff_key : Script_repr.expr;
  diff_key_hash : Script_expr_hash.t;
  diff_value : Script_repr.expr option;
}
type big_map_diff = big_map_diff_item list

let big_map_diff_item_encoding =
  let open Data_encoding in
  conv
    (fun { diff_key_hash ; diff_key ; diff_value } -> (diff_key_hash, diff_key, diff_value))
    (fun (diff_key_hash, diff_key, diff_value) -> { diff_key_hash ; diff_key ; diff_value })
    (obj3
       (req "key_hash" Script_expr_hash.encoding)
       (req "key" Script_repr.expr_encoding)
       (opt "value" Script_repr.expr_encoding))

let big_map_diff_encoding =
  let open Data_encoding in
  def "contract.big_map_diff" @@
  list big_map_diff_item_encoding

let update_script_big_map c contract = function
  | None -> return (c, Z.zero)
  | Some diff ->
      fold_left_s (fun (c, total) diff_item ->
          match diff_item.diff_value with
          | None ->
              Storage.Contract.Big_map.remove (c, contract) diff_item.diff_key_hash
              >>=? fun (c, freed) ->
              return (c, Z.sub total (Z.of_int freed))
          | Some v ->
              Storage.Contract.Big_map.init_set (c, contract) diff_item.diff_key_hash v
              >>=? fun (c, size_diff) ->
              return (c, Z.add total (Z.of_int size_diff)))
        (c, Z.zero) diff

let create_base c
    ?(prepaid_bootstrap_storage=false) (* Free space for bootstrap contracts *)
    contract
    ~balance ~manager ~delegate ?script ~spendable ~delegatable =
  (match Contract_repr.is_implicit contract with
   | None -> return Z.zero
   | Some _ -> Storage.Contract.Global_counter.get c) >>=? fun counter ->
  Storage.Contract.Balance.init c contract balance >>=? fun c ->
  Storage.Contract.Manager.init c contract (Manager_repr.Hash manager) >>=? fun c ->
  begin
    match delegate with
    | None -> return c
    | Some delegate ->
        Delegate_storage.init c contract delegate
  end >>=? fun c ->
  Storage.Contract.Spendable.set c contract spendable >>= fun c ->
  Storage.Contract.Delegatable.set c contract delegatable >>= fun c ->
  Storage.Contract.Counter.init c contract counter >>=? fun c ->
  (match script with
   | Some ({ Script_repr.code ; storage }, big_map_diff) ->
       Storage.Contract.Code.init c contract code >>=? fun (c, code_size) ->
       Storage.Contract.Storage.init c contract storage >>=? fun (c, storage_size) ->
       update_script_big_map c contract big_map_diff >>=? fun (c, big_map_size) ->
       let total_size = Z.add (Z.add (Z.of_int code_size) (Z.of_int storage_size)) big_map_size in
       assert Compare.Z.(total_size >= Z.zero) ;
       let prepaid_bootstrap_storage =
         if prepaid_bootstrap_storage then
           total_size
         else
           Z.zero
       in
       Storage.Contract.Paid_storage_space.init c contract prepaid_bootstrap_storage >>=? fun c ->
       Storage.Contract.Used_storage_space.init c contract total_size
   | None -> begin
       match Contract_repr.is_implicit contract with
       | None ->
           Storage.Contract.Paid_storage_space.init c contract Z.zero >>=? fun c ->
           Storage.Contract.Used_storage_space.init c contract Z.zero
       | Some _ ->
           return c
     end >>=? fun c ->
       return c) >>=? fun c ->
  return c

let originate c ?prepaid_bootstrap_storage contract
    ~balance ~manager ?script ~delegate ~spendable ~delegatable =
  create_base c ?prepaid_bootstrap_storage contract ~balance ~manager
    ~delegate ?script ~spendable ~delegatable

let create_implicit c manager ~balance =
  create_base c (Contract_repr.implicit_contract manager)
    ~balance ~manager ?script:None ~delegate:None
    ~spendable:true ~delegatable:false

let delete c contract =
  match Contract_repr.is_implicit contract with
  | None ->
      (* For non implicit contract Big_map should be cleared *)
      failwith "Non implicit contracts cannot be removed"
  | Some _ ->
      Delegate_storage.remove c contract >>=? fun c ->
      Storage.Contract.Balance.delete c contract >>=? fun c ->
      Storage.Contract.Manager.delete c contract >>=? fun c ->
      Storage.Contract.Spendable.del c contract >>= fun c ->
      Storage.Contract.Delegatable.del c contract >>= fun c ->
      Storage.Contract.Counter.delete c contract >>=? fun c ->
      Storage.Contract.Code.remove c contract >>=? fun (c, _) ->
      Storage.Contract.Storage.remove c contract >>=? fun (c, _) ->
      Storage.Contract.Paid_storage_space.remove c contract >>= fun c ->
      Storage.Contract.Used_storage_space.remove c contract >>= fun c ->
      return c

let allocated c contract =
  Storage.Contract.Counter.get_option c contract >>=? function
  | None -> return_false
  | Some _ -> return_true

let exists c contract =
  match Contract_repr.is_implicit contract with
  | Some _ -> return_true
  | None -> allocated c contract

let must_exist c contract =
  exists c contract >>=? function
  | true -> return_unit
  | false -> fail (Non_existing_contract contract)

let must_be_allocated c contract =
  allocated c contract >>=? function
  | true -> return_unit
  | false ->
      match Contract_repr.is_implicit contract with
      | Some pkh -> fail (Empty_implicit_contract pkh)
      | None -> fail (Non_existing_contract contract)

let list c = Storage.Contract.list c

let fresh_contract_from_current_nonce c =
  Lwt.return (Raw_context.increment_origination_nonce c) >>=? fun (c, nonce) ->
  return (c, Contract_repr.originated_contract nonce)

let originated_from_current_nonce ~since: ctxt_since ~until: ctxt_until =
  Lwt.return (Raw_context.origination_nonce ctxt_since) >>=? fun since ->
  Lwt.return (Raw_context.origination_nonce ctxt_until) >>=? fun until ->
  filter_map_s
    (fun contract -> exists ctxt_until contract >>=? function
       | true -> return_some contract
       | false -> return_none)
    (Contract_repr.originated_contracts ~since ~until)

let check_counter_increment c contract counter =
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  let expected = Z.succ contract_counter in
  if Compare.Z.(expected = counter)
  then return_unit
  else if Compare.Z.(expected > counter) then
    fail (Counter_in_the_past (contract, expected, counter))
  else
    fail (Counter_in_the_future (contract, expected, counter))

let increment_counter c contract =
  Storage.Contract.Global_counter.get c >>=? fun global_counter ->
  Storage.Contract.Global_counter.set c (Z.succ global_counter) >>=? fun c ->
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  Storage.Contract.Counter.set c contract (Z.succ contract_counter)

let get_script c contract =
  Storage.Contract.Code.get_option c contract >>=? fun (c, code) ->
  Storage.Contract.Storage.get_option c contract >>=? fun (c, storage) ->
  match code, storage with
  | None, None -> return (c, None)
  | Some code, Some storage -> return (c, Some { Script_repr.code ; storage })
  | None, Some _ | Some _, None -> failwith "get_script"

let get_storage ctxt contract =
  Storage.Contract.Storage.get_option ctxt contract >>=? function
  | (ctxt, None) -> return (ctxt, None)
  | (ctxt, Some storage) ->
      Lwt.return (Script_repr.force_decode storage) >>=? fun (storage, cost) ->
      Lwt.return (Raw_context.consume_gas ctxt cost) >>=? fun ctxt ->
      return (ctxt, Some storage)

let get_counter c contract =
  Storage.Contract.Counter.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_implicit contract with
      | Some _ -> Storage.Contract.Global_counter.get c
      | None -> failwith "get_counter"
    end
  | Some v -> return v

let get_manager c contract =
  Storage.Contract.Manager.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_implicit contract with
      | Some manager -> return manager
      | None -> failwith "get_manager"
    end
  | Some (Manager_repr.Hash v) -> return v
  | Some (Manager_repr.Public_key v) -> return (Signature.Public_key.hash v)

let get_manager_key c contract =
  Storage.Contract.Manager.get_option c contract >>=? function
  | None -> failwith "get_manager_key"
  | Some (Manager_repr.Hash _) -> fail (Unrevealed_manager_key contract)
  | Some (Manager_repr.Public_key v) -> return v

let is_manager_key_revealed c contract =
  Storage.Contract.Manager.get_option c contract >>=? function
  | None -> return_false
  | Some (Manager_repr.Hash _) -> return_false
  | Some (Manager_repr.Public_key _) -> return_true

let reveal_manager_key c contract public_key =
  Storage.Contract.Manager.get c contract >>=? function
  | Public_key _ -> fail (Previously_revealed_key contract)
  | Hash v ->
      let actual_hash = Signature.Public_key.hash public_key in
      if (Signature.Public_key_hash.equal actual_hash v) then
        let v = (Manager_repr.Public_key public_key) in
        Storage.Contract.Manager.set c contract v >>=? fun c ->
        return c
      else fail (Inconsistent_hash (public_key,v,actual_hash))

let get_balance c contract =
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_implicit contract with
      | Some _ -> return Tez_repr.zero
      | None -> failwith "get_balance"
    end
  | Some v -> return v

let is_delegatable = Delegate_storage.is_delegatable
let is_spendable c contract =
  match Contract_repr.is_implicit contract with
  | Some _ -> return_true
  | None ->
      Storage.Contract.Spendable.mem c contract >>= return

let update_script_storage c contract storage big_map_diff =
  let storage = Script_repr.lazy_expr storage in
  update_script_big_map c contract big_map_diff >>=? fun (c, big_map_size_diff) ->
  Storage.Contract.Storage.set c contract storage >>=? fun (c, size_diff) ->
  Storage.Contract.Used_storage_space.get c contract >>=? fun previous_size ->
  let new_size = Z.add previous_size (Z.add big_map_size_diff (Z.of_int size_diff)) in
  Storage.Contract.Used_storage_space.set c contract new_size

let spend_from_script c contract amount =
  Storage.Contract.Balance.get c contract >>=? fun balance ->
  match Tez_repr.(balance -? amount) with
  | Error _ ->
      fail (Balance_too_low (contract, balance, amount))
  | Ok new_balance ->
      Storage.Contract.Balance.set c contract new_balance >>=? fun c ->
      Roll_storage.Contract.remove_amount c contract amount >>=? fun c ->
      if Tez_repr.(new_balance > Tez_repr.zero) then
        return c
      else match Contract_repr.is_implicit contract with
        | None -> return c (* Never delete originated contracts *)
        | Some pkh ->
            Delegate_storage.get c contract >>=? function
            | Some pkh' ->
                (* Don't delete "delegate" contract *)
                assert (Signature.Public_key_hash.equal pkh pkh') ;
                return c
            | None ->
                (* Delete empty implicit contract *)
                delete c contract

let credit c contract amount =
  begin
    if Tez_repr.(amount <> Tez_repr.zero) then
      return c
    else
      Storage.Contract.Code.mem c contract >>=? fun (c, target_has_code) ->
      fail_unless target_has_code (Empty_transaction contract) >>=? fun () ->
      return c
  end >>=? fun c ->
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_implicit contract with
      | None -> fail (Non_existing_contract contract)
      | Some manager ->
          create_implicit c manager ~balance:amount
    end
  | Some balance ->
      Lwt.return Tez_repr.(amount +? balance) >>=? fun balance ->
      Storage.Contract.Balance.set c contract balance >>=? fun c ->
      Roll_storage.Contract.add_amount c contract amount >>=? fun c ->
      begin
        match contract with
        | Implicit delegate ->
            Delegate_storage.registered c delegate >>= fun registered ->
            if registered then
              Roll_storage.Delegate.set_active c delegate >>=? fun c ->
              return c
            else
              return c
        | Originated _ ->
            return c
      end >>=? fun c ->
      return c

let spend c contract amount =
  is_spendable c contract >>=? fun spendable ->
  if not spendable
  then fail (Unspendable_contract contract)
  else spend_from_script c contract amount

let init c =
  Storage.Contract.Global_counter.init c Z.zero

let used_storage_space c contract =
  Storage.Contract.Used_storage_space.get_option c contract >>=? function
  | None -> return Z.zero
  | Some fees -> return fees

let paid_storage_space c contract =
  Storage.Contract.Paid_storage_space.get_option c contract >>=? function
  | None -> return Z.zero
  | Some paid_space -> return paid_space

let set_paid_storage_space_and_return_fees_to_pay c contract new_storage_space =
  Storage.Contract.Paid_storage_space.get c contract >>=? fun already_paid_space ->
  if Compare.Z.(already_paid_space >= new_storage_space) then
    return (Z.zero, c)
  else
    let to_pay = Z.sub new_storage_space already_paid_space in
    Storage.Contract.Paid_storage_space.set c contract new_storage_space >>=? fun c ->
    return (to_pay, c)

module Big_map = struct
  let mem ctxt contract key =
    Storage.Contract.Big_map.mem (ctxt, contract) key
  let get_opt ctxt contract key =
    Storage.Contract.Big_map.get_option (ctxt, contract) key
end
