(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Initial_amount_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Permanent *)
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Cannot_pay_storage_fee of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Counter_in_the_past of Contract_repr.contract * int32 * int32 (* `Branch *)
  | Counter_in_the_future of Contract_repr.contract * int32 * int32 (* `Temporary *)
  | Faucet_counter_in_the_past of int32 * int32 (* `Branch *)
  | Faucet_counter_in_the_future of int32 * int32 (* `Temporary *)
  | Unspendable_contract of Contract_repr.contract (* `Permanent *)
  | Non_existing_contract of Contract_repr.contract (* `Temporary *)
  | Non_delegatable_contract of Contract_repr.contract (* `Permanent *)
  | Failure of string (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"contract.initial_amount_too_low"
    ~title:"Initial amount too low"
    ~description:"Not enough tokens provided for an origination"
    ~pp:(fun ppf (c, r, p) ->
        Format.fprintf ppf "Initial amount of contract %a too low (required %a but provided %a)"
          Contract_repr.pp c
          Tez_repr.pp r Tez_repr.pp p)
    Data_encoding.(obj3
                     (req "contract" Contract_repr.encoding)
                     (req "required" Tez_repr.encoding)
                     (req "provided" Tez_repr.encoding))
    (function Initial_amount_too_low (c, r, p)   -> Some (c, r, p) | _ -> None)
    (fun (c, r, p) -> Initial_amount_too_low (c, r, p)) ;
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
    ~id:"contract.cannot_pay_storage_fee"
    ~title:"Cannot pay storage fee"
    ~description:"The storage fee is higher than the contract balance"
    ~pp:(fun ppf (c, b, a) ->
        Format.fprintf ppf "Balance of contract %a too low (%a) to pay storage fee %a"
          Contract_repr.pp c Tez_repr.pp b Tez_repr.pp a)
    Data_encoding.(obj3
                     (req "contract" Contract_repr.encoding)
                     (req "balance" Tez_repr.encoding)
                     (req "storage_fee" Tez_repr.encoding))
    (function Cannot_pay_storage_fee (c, b, a)   -> Some (c, b, a) | _ -> None)
    (fun (c, b, a) -> Cannot_pay_storage_fee (c, b, a)) ;
  register_error_kind
    `Temporary
    ~id:"contract.counter_in_the_future"
    ~title:"Invalid counter (not yet reached) in a manager operation"
    ~description:"An operation assumed a contract counter in the future"
    ~pp:(fun ppf (contract, exp, found) ->
        Format.fprintf ppf
          "Counter %ld not yet reached for contract %a (expected %ld)"
          found Contract_repr.pp contract exp)
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
          "Counter %ld already used for contract %a (expected %ld)"
          found Contract_repr.pp contract exp)
    Data_encoding.
      (obj3
         (req "contract" Contract_repr.encoding)
         (req "expected" int32)
         (req "found" int32))
    (function Counter_in_the_past (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_past (c, x, y)) ;
  register_error_kind
    `Temporary
    ~id:"contract.faucet_counter_in_the_future"
    ~title:"Invalid faucet counter (not yet reached) in a manager operation"
    ~description:"An operation assumed a faucet counter in the future"
    ~pp:(fun ppf (exp, found) ->
        Format.fprintf ppf
          "Faucet counter %ld not yet reached (expected %ld)"
          found exp)
    Data_encoding.
      (obj2
         (req "expected" int32)
         (req "found" int32))
    (function Faucet_counter_in_the_future (x, y) -> Some (x, y) | _ -> None)
    (fun (x, y) -> Faucet_counter_in_the_future (x, y)) ;
  register_error_kind
    `Branch
    ~id:"contract.faucet_counter_in_the_past"
    ~title:"Invalid faucet counter (already used) in a manager operation"
    ~description:"An operation assumed a faucet counter in the past"
    ~pp:(fun ppf (exp, found) ->
        Format.fprintf ppf
          "Faucet counter %ld already used (expected %ld)"
          found exp)
    Data_encoding.
      (obj2
         (req "expected" int32)
         (req "found" int32))
    (function Faucet_counter_in_the_past (x, y) -> Some (x, y) | _ -> None)
    (fun (x, y) -> Faucet_counter_in_the_past (x, y)) ;
  register_error_kind
    `Temporary
    ~id:"contract.non_existing_contract"
    ~title:"Non existing contract"
    ~description:"A non default contract handle is not present in the context \
                  (either it never was or it has been destroyed)"
    ~pp:(fun ppf contract ->
        Format.fprintf ppf "Contract %a does not exist"
          Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Non_existing_contract c -> Some c | _ -> None)
    (fun c -> Non_existing_contract c) ;
  register_error_kind
    `Permanent
    ~id:"contract.undelagatable_contract"
    ~title:"Non delegatable contract"
    ~description:"Tried to delegate a default contract \
                  or a non delegatable originated contract"
    ~pp:(fun ppf contract ->
        Format.fprintf ppf "Contract %a is not delegatable"
          Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Non_delegatable_contract c -> Some c | _ -> None)
    (fun c -> Non_delegatable_contract c) ;
  register_error_kind
    `Permanent
    ~id:"contract.failure"
    ~title:"Contract storage failure"
    ~description:"Unexpected contract storage error"
    ~pp:(fun ppf s -> Format.fprintf ppf "Contract_storage.Failure %S" s)
    Data_encoding.(obj1 (req "message" string))
    (function Failure s -> Some s | _ -> None)
    (fun s -> Failure s)

let failwith msg = fail (Failure msg)

let create_base c contract ~balance ~manager ~delegate ?script ~spendable ~delegatable =
  (match Contract_repr.is_default contract with
   | None -> return 0l
   | Some _ -> Storage.Contract.Global_counter.get c) >>=? fun counter ->
  Storage.Contract.Balance.init c contract balance >>=? fun c ->
  Storage.Contract.Manager.init c contract manager >>=? fun c ->
  begin
    match delegate with
    | None -> return c
    | Some delegate ->
        Storage.Contract.Delegate.init c contract delegate
  end >>=? fun c ->
  Storage.Contract.Spendable.init c contract spendable >>=? fun c ->
  Storage.Contract.Delegatable.init c contract delegatable >>=? fun c ->
  Storage.Contract.Counter.init c contract counter >>=? fun c ->
  (match script with
   | Some ({ Script_repr.code ; storage }, (code_fees, storage_fees)) ->
       Storage.Contract.Code.init c contract code >>=? fun c ->
       Storage.Contract.Storage.init c contract storage >>=? fun c ->
       Storage.Contract.Code_fees.init c contract code_fees >>=? fun c ->
       Storage.Contract.Storage_fees.init c contract storage_fees
   | None ->
       return c) >>=? fun c ->
  Roll_storage.Contract.init c contract >>=? fun c ->
  Roll_storage.Contract.add_amount c contract balance >>=? fun c ->
  Storage.Contract.Set.add c contract >>=? fun c ->
  Lwt.return (Ok (c, contract))

let create c nonce ~balance ~manager ~delegate ?script ~spendable ~delegatable =
  let contract = Contract_repr.originated_contract nonce in
  create_base c contract ~balance ~manager ~delegate ?script ~spendable ~delegatable >>=? fun (ctxt, contract) ->
  return (ctxt, contract, Contract_repr.incr_origination_nonce nonce)

let create_default c manager ~balance =
  create_base c (Contract_repr.default_contract manager)
    ~balance ~manager ~delegate:(Some manager)
    ?script:None
    ~spendable:true ~delegatable:false

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
  Storage.Contract.Code_fees.remove c contract >>= fun c ->
  Storage.Contract.Storage_fees.remove c contract >>= fun c ->
  Storage.Contract.Set.del c contract

let exists c contract =
  match Contract_repr.is_default contract with
  | Some _ -> return true
  | None ->
      Storage.Contract.Counter.get_option c contract >>=? function
      | None -> return false
      | Some _ -> return true

let must_exist c contract =
  exists c contract >>=? function
  | true -> return ()
  | false -> fail (Non_existing_contract contract)

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

let get_faucet_counter c =
  Storage.Contract.Faucet_counter.get c

let check_faucet_counter_increment c counter =
  Storage.Contract.Faucet_counter.get c >>=? fun faucet_counter ->
  let expected = Int32.succ faucet_counter in
  if Compare.Int32.(expected = counter)
  then return ()
  else if Compare.Int32.(expected > counter) then
    fail (Faucet_counter_in_the_past (expected, counter))
  else
    fail (Faucet_counter_in_the_future (expected, counter))

let increment_faucet_counter c =
  Storage.Contract.Faucet_counter.get c >>=? fun faucet_counter ->
  Storage.Contract.Faucet_counter.set c (Int32.succ faucet_counter)

let get_script c contract =
  Storage.Contract.Code.get_option c contract >>=? fun code ->
  Storage.Contract.Storage.get_option c contract >>=? fun storage ->
  match code, storage with
  | None, None -> return None
  | Some code, Some storage -> return (Some { Script_repr.code ; storage })
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

let get_balance c contract =
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | Some _ -> return Tez_repr.zero
      | None -> failwith "get_balance"
    end
  | Some v -> return v

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
  is_delegatable c contract >>=? function
  | false -> fail (Non_delegatable_contract contract)
  | true ->
      match delegate with
      | None ->
          Storage.Contract.Delegate.remove c contract >>= fun c ->
          return c
      | Some delegate ->
          Storage.Contract.Delegate.init_set c contract delegate

let contract_fee c contract =
  Storage.Contract.Code_fees.get_option c contract >>=? fun code_fees ->
  Storage.Contract.Storage_fees.get_option c contract >>=? fun storage_fees ->
  match code_fees, storage_fees with
  | (None, Some _) | (Some _, None) -> failwith "contract_fee"
  | None, None ->
      return Constants_repr.minimal_contract_balance
  | Some code_fees, Some storage_fees ->
      Lwt.return Tez_repr.(code_fees +? storage_fees) >>=? fun script_fees ->
      Lwt.return Tez_repr.(Constants_repr.minimal_contract_balance +? script_fees)

let update_script_storage_and_fees c contract storage_fees storage =
  let open Script_repr in
  Storage.Contract.Balance.get_option c contract >>=? function
  | None ->
      (* The contract was destroyed *)
      return c
  | Some balance ->
      Storage.Contract.Storage.get c contract >>=? fun { storage_type } ->
      Storage.Contract.Storage_fees.set c contract storage_fees >>=? fun c ->
      contract_fee c contract >>=? fun fee ->
      fail_unless Tez_repr.(balance > fee)
        (Cannot_pay_storage_fee (contract, balance, fee)) >>=? fun () ->
      Storage.Contract.Storage.set c contract { storage; storage_type }

let spend_from_script c contract amount =
  Storage.Contract.Balance.get c contract >>=? fun balance ->
  Lwt.return Tez_repr.(balance -? amount) |>
  trace (Balance_too_low (contract, balance, amount)) >>=? fun new_balance ->
  contract_fee c contract >>=? fun fee ->
  if Tez_repr.(new_balance > fee) then
    Storage.Contract.Balance.set c contract new_balance >>=? fun c ->
    Roll_storage.Contract.remove_amount c contract amount
  else
    (* TODO: do we really want to allow overspending ? *)
    delete c contract

let credit c contract amount =
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_default contract with
      | None -> fail (Non_existing_contract contract)
      | Some manager ->
          if Tez_repr.(amount < Constants_repr.minimal_contract_balance)
          then
            (* Not enough to keep a default contract alive: burn the tokens *)
            return c
          else
            (* Otherwise, create the default contract. *)
            create_default c manager ~balance:amount >>=? fun (c, _) ->
            return c
    end
  | Some balance ->
      Lwt.return Tez_repr.(amount +? balance) >>=? fun balance ->
      Storage.Contract.Balance.set c contract balance >>=? fun c ->
      Roll_storage.Contract.add_amount c contract amount

let spend c contract amount =
  Storage.Contract.Spendable.get c contract >>=? fun spendable ->
  if not spendable
  then fail (Unspendable_contract contract)
  else spend_from_script c contract amount

let originate c nonce ~balance ~manager ?script ~delegate ~spendable ~delegatable  =
  create c nonce ~balance ~manager ~delegate ?script ~spendable ~delegatable >>=? fun (c, contract, nonce) ->
  (* check contract fee *)
  contract_fee c contract >>=? fun fee ->
  fail_unless Tez_repr.(balance > fee)
    (Initial_amount_too_low (contract, balance, fee)) >>=? fun () ->
  return (c, contract, nonce)

let init c =
  Storage.Contract.Faucet_counter.init c 0l >>=? fun c ->
  Storage.Contract.Global_counter.init c 0l
