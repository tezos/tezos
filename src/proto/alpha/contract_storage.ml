(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t
  | Initial_amount_too_low of Tez_repr.t * Tez_repr.t
  | Counter_in_the_past of Contract_repr.contract * int32 * int32
  | Counter_in_the_future of Contract_repr.contract * int32 * int32
  | Unspendable_contract of Contract_repr.contract
  | Non_existing_contract (* TODO: DOC *)
  | No_delegate (* TODO: DOC *)
  | Undelagatable_contract (* TODO: DOC *)
  | Failure of string

let () =
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
    `Permanent
    ~id:"contract.initial_amount_too_low"
    ~title:"Initial amount too low"
    ~description:"Not enough tokens provided for an origination"
    ~pp:(fun ppf (r, p) ->
        Format.fprintf ppf "Initial amount too low (required %a but provided %a)"
          Tez_repr.pp r Tez_repr.pp p)
    Data_encoding.(obj2
                     (req "required" Tez_repr.encoding)
                     (req "provided" Tez_repr.encoding))
    (function Initial_amount_too_low (r, p)   -> Some (r, p) | _ -> None)
    (fun (r, p) -> Initial_amount_too_low (r, p)) ;
  register_error_kind
    `Branch
    ~id:"contract.unspendable_contract"
    ~title:"Unspendable contract"
    ~description:"An operation tried to spend tokens from an unspendable contract"
    ~pp:(fun ppf c ->
        Format.fprintf ppf "The tokens of contract %s can only be spent by its script"
          (Contract_repr.to_b58check c))
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Unspendable_contract c   -> Some c | _ -> None)
    (fun c -> Unspendable_contract c) ;
  register_error_kind
    `Temporary
    ~id:"contract.balance_too_low"
    ~title:"Too low balance"
    ~description:"An operation tried to spend more tokens than the contract has"
    ~pp:(fun ppf (c, b, a) ->
        Format.fprintf ppf "Balance of contract %s too low (%a) to spend %a"
          (Contract_repr.to_b58check c) Tez_repr.pp b Tez_repr.pp a)
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
          "Counter %ld not yet reached for contract %s (expected %ld)"
          found (Contract_repr.to_b58check contract) exp)
    Data_encoding.
      (obj3
         (req "contract" Contract_repr.encoding)
         (req "expected" int32)
         (req "found" int32))
    (function Counter_in_the_future (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_future (c, x, y)) ;
  register_error_kind
    `Branch
    ~id:"contract.counter_in_the_past"
    ~title:"Invalid counter (already used) in a manager operation"
    ~description:"An operation assumed a contract counter in the past"
    ~pp:(fun ppf (contract, exp, found) ->
        Format.fprintf ppf
          "Counter %ld already used for contract %s (expected %ld)"
          found (Contract_repr.to_b58check contract) exp)
    Data_encoding.
      (obj3
         (req "contract" Contract_repr.encoding)
         (req "expected" int32)
         (req "found" int32))
    (function Counter_in_the_past (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_past (c, x, y))

let failwith msg = fail (Failure msg)

let create_base c contract ~balance ~manager ~delegate ~script ~spendable ~delegatable =
  (match Contract_repr.is_default contract with
   | None -> return 0l
   | Some _ -> Storage.Contract.Global_counter.get c) >>=? fun counter ->
  Storage.Contract.Balance.init c contract balance >>=? fun c ->
  Storage.Contract.Manager.init c contract manager >>=? fun c ->
  (* TODO, to answer:
     If the contract is not delegatable, can it be created with a delegate ? *)
  begin
    match delegate with
    | None -> return c
    | Some delegate ->
        Storage.Contract.Delegate.init c contract delegate
  end >>=? fun c ->
  Storage.Contract.Spendable.init c contract spendable >>=? fun c ->
  Storage.Contract.Delegatable.init c contract delegatable >>=? fun c ->
  Storage.Contract.Assets.init c contract Asset_repr.Map.empty >>=? fun c ->
  Storage.Contract.Counter.init c contract counter >>=? fun c ->
  (match script with
   | Script_repr.Script { code ; storage } ->
       Storage.Contract.Code.init c contract code >>=? fun c ->
       Storage.Contract.Storage.init c contract storage
   | No_script ->
       return c) >>=? fun c ->
  Roll_storage.Contract.init c contract >>=? fun c ->
  Roll_storage.Contract.add_amount c contract balance >>=? fun c ->
  Storage.Contract.Set.add c contract >>=? fun c ->
  Lwt.return (Ok (c, contract))

let create c nonce ~balance ~manager ~delegate ~script ~spendable ~delegatable =
  let contract = Contract_repr.originated_contract nonce in
  create_base c contract ~balance ~manager ~delegate ~script ~spendable ~delegatable >>=? fun (ctxt, contract) ->
  return (ctxt, contract, Contract_repr.incr_origination_nonce nonce)

let create_default c manager ~balance =
  let contract = Contract_repr.default_contract manager in
  create_base c contract ~manager ~delegate:(Some manager)
    ~spendable:true ~delegatable:false ~script:Script_repr.No_script
    ~balance

let delete c contract =
  Storage.Contract.Balance.get c contract >>=? fun balance ->
  Roll_storage.Contract.remove_amount c contract balance >>=? fun c ->
  Roll_storage.Contract.assert_empty c contract >>=? fun () ->
  Storage.Contract.Balance.delete c contract >>=? fun c ->
  Storage.Contract.Manager.delete c contract >>=? fun c ->
  Storage.Contract.Delegate.remove c contract >>= fun c ->
  Storage.Contract.Spendable.delete c contract >>=? fun c ->
  Storage.Contract.Delegatable.delete c contract >>=? fun c ->
  Storage.Contract.Counter.delete c contract >>=? fun c ->
  Storage.Contract.Code.remove c contract >>= fun c ->
  Storage.Contract.Storage.remove c contract >>= fun c ->
  Storage.Contract.Set.del c contract

let exists c contract =
  match Contract_repr.is_default contract with
  | Some _ -> return true
  | None ->
      Storage.Contract.Counter.get_option c contract >>=? function
      | None -> return false
      | Some _ -> return true

let list c =
  Storage.Contract.Set.elements c

let check_counter_increment c contract counter =
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  let expected = Int32.succ contract_counter in
  if Compare.Int32.(expected = counter)
  then return ()
  else if Compare.Int32.(expected > counter) then
    fail (Counter_in_the_past (contract, expected, counter))
  else
    fail (Counter_in_the_future (contract, expected, counter))

let increment_counter c contract =
  Storage.Contract.Global_counter.get c >>=? fun global_counter ->
  Storage.Contract.Global_counter.set c (Int32.succ global_counter) >>=? fun c ->
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  Storage.Contract.Counter.set c contract (Int32.succ contract_counter)

let get_script c contract =
  Storage.Contract.Code.get_option c contract >>=? fun code ->
  Storage.Contract.Storage.get_option c contract >>=? fun storage ->
  match code, storage with
  | None, None -> return Script_repr.No_script
  | Some code, Some storage -> return (Script_repr.Script { code ; storage })
  | None, Some _ | Some _, None -> failwith "get_script"

let get_counter c contract =
  Storage.Contract.Counter.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> Storage.Contract.Global_counter.get c
      | None -> failwith "get_counter"
    end
  | Some v -> return v

let get_manager c contract =
  Storage.Contract.Manager.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some manager -> return manager
      | None -> failwith "get_manager"
    end
  | Some v -> return v

let get_delegate_opt = Roll_storage.get_contract_delegate

let get_delegate c contract =
  get_delegate_opt c contract >>=? function
  | None -> fail No_delegate
  | Some delegate -> return delegate

let get_balance c contract =
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> return Tez_repr.zero
      | None -> failwith "get_balance"
    end
  | Some v -> return v

let get_assets c contract =
  Storage.Contract.Assets.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> return Asset_repr.Map.empty
      | None -> failwith "get_assets"
    end
  | Some a -> return a

let is_delegatable c contract =
  Storage.Contract.Delegatable.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> return false
      | None -> failwith "is_delegatable"
    end
  | Some v -> return v

let is_spendable c contract =
  Storage.Contract.Spendable.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> return true
      | None -> failwith "is_spendable"
    end
  | Some v -> return v

let set_delegate c contract delegate =
  (* A contract delegate can be set only if the contract is delegatable *)
  Storage.Contract.Delegatable.get c contract >>=? fun delegatable ->
  if not delegatable
  then fail Undelagatable_contract
  else
    match delegate with
    | None ->
        Storage.Contract.Delegate.remove c contract >>= fun c ->
        return c
    | Some delegate ->
        Storage.Contract.Delegate.init_set c contract delegate

let script_storage_fee script =
  match script with
  | Script_repr.No_script -> return Constants_repr.minimal_contract_balance
  | Script { code ; storage } ->
     let storage_fee = Script_repr.storage_cost storage in
     let code_fee = Script_repr.code_cost code in
     Lwt.return Tez_repr.(code_fee +? storage_fee) >>=? fun script_fee ->
     Lwt.return Tez_repr.(Constants_repr.minimal_contract_balance +? script_fee)

let update_script_storage c contract storage =
  let open Script_repr in
  Storage.Contract.Balance.get_option c contract >>=? function
  | None ->
      (* The contract was destroyed *)
      return c
  | Some balance ->
      get_script c contract >>=? function
      | No_script -> failwith "update_script_storage"
      | Script { code ; storage = { storage_type } } ->
         script_storage_fee
           (Script_repr.Script { code ; storage = { storage; storage_type }}) >>=? fun fee ->
         fail_unless Tez_repr.(balance > fee)
           (Balance_too_low (contract, balance, fee)) >>=? fun () ->
         Storage.Contract.Storage.set c contract { storage; storage_type }

let unconditional_spend c contract amount =
  Storage.Contract.Balance.get c contract >>=? fun balance ->
  match Tez_repr.(balance - amount) with
  | None ->
      fail (Balance_too_low (contract, balance, amount))
  | Some new_balance ->
      get_script c contract >>=? fun script ->
      script_storage_fee script >>=? fun fee ->
      if Tez_repr.(fee <= new_balance) then
        Storage.Contract.Balance.set c contract new_balance >>=? fun c ->
        Roll_storage.Contract.remove_amount c contract amount
      else
        delete c contract

let credit c contract amount =
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      (* If the contract does not exists and it is a default contract,
         create it *)
      match Contract_repr.is_default contract with
      | None -> fail Non_existing_contract
      | Some manager ->
          if Tez_repr.(amount < Constants_repr.minimal_contract_balance)
          then
            (* If this is not enough to maintain the contract alive,
               we just drop the money *)
            return c
          else
            create_default c manager ~balance:amount >>=? fun (c, _) ->
            (* TODO: fail_unless Contract_repr.(contract = new_contract) still needed ?? *)
            return c
    end
  | Some balance ->
      Lwt.return Tez_repr.(amount +? balance) >>=? fun balance ->
      Storage.Contract.Balance.set c contract balance >>=? fun c ->
      Roll_storage.Contract.add_amount c contract amount

let issue c contract asset key quantity =
  Storage.Contract.Assets.get_option c contract >>=? function
  | None ->
     Lwt.return (Asset_repr.Map.add Asset_repr.Map.empty asset key quantity) >>=?
     Storage.Contract.Assets.set c contract
  | Some assets ->
     Lwt.return (Asset_repr.Map.add assets asset key quantity) >>=?
     Storage.Contract.Assets.set c contract

let spend c contract amount =
  Storage.Contract.Spendable.get c contract >>=? fun spendable ->
  if not spendable
  then fail (Unspendable_contract contract)
  else unconditional_spend c contract amount

let originate c nonce ~balance ~manager ~script ~delegate ~spendable ~delegatable  =
  script_storage_fee script >>=? fun fee ->
  fail_unless Tez_repr.(balance > fee)
    (Initial_amount_too_low (fee, balance)) >>=? fun () ->
  create c nonce ~balance ~manager ~delegate ~script ~spendable ~delegatable

let init c =
  Storage.Contract.Global_counter.init c 0l

let pp fmt c =
  Format.pp_print_string fmt (Contract_repr.to_b58check c)
