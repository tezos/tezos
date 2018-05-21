(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Counter_in_the_past of Contract_repr.contract * int32 * int32 (* `Branch *)
  | Counter_in_the_future of Contract_repr.contract * int32 * int32 (* `Temporary *)
  | Unspendable_contract of Contract_repr.contract (* `Permanent *)
  | Non_existing_contract of Contract_repr.contract (* `Temporary *)
  | Empty_implicit_contract of Signature.Public_key_hash.t (* `Temporary *)
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
    (fun c -> Empty_implicit_contract c)

let failwith msg = fail (Failure msg)

let create_base c contract
    ~balance ~manager ~delegate ?script ~spendable ~delegatable =
  (match Contract_repr.is_implicit contract with
   | None -> return 0l
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
   | Some ({ Script_repr.code ; storage }, (code_fees, storage_fees)) ->
       Storage.Contract.Code.init c contract code >>=? fun c ->
       Storage.Contract.Storage.init c contract storage >>=? fun c ->
       Storage.Contract.Code_fees.init c contract code_fees >>=? fun c ->
       Storage.Contract.Storage_fees.init c contract storage_fees
   | None ->
       return c) >>=? fun c ->
  return (c, contract)

let originate c nonce ~balance ~manager ?script ~delegate ~spendable ~delegatable =
  let contract = Contract_repr.originated_contract nonce in
  create_base c contract ~balance ~manager ~delegate ?script ~spendable ~delegatable >>=? fun (ctxt, contract) ->
  return (ctxt, contract, Contract_repr.incr_origination_nonce nonce)

let create_implicit c manager ~balance =
  create_base c (Contract_repr.implicit_contract manager)
    ~balance ~manager ?script:None ~delegate:None
    ~spendable:true ~delegatable:false

let delete c contract =
  Delegate_storage.remove c contract >>=? fun c ->
  Storage.Contract.Balance.delete c contract >>=? fun c ->
  Storage.Contract.Manager.delete c contract >>=? fun c ->
  Storage.Contract.Spendable.del c contract >>= fun c ->
  Storage.Contract.Delegatable.del c contract >>= fun c ->
  Storage.Contract.Counter.delete c contract >>=? fun c ->
  Storage.Contract.Code.remove c contract >>= fun c ->
  Storage.Contract.Storage.remove c contract >>= fun c ->
  Storage.Contract.Code_fees.remove c contract >>= fun c ->
  Storage.Contract.Storage_fees.remove c contract >>= fun c ->
  Storage.Contract.Big_map.clear (c, contract) >>= fun c ->
  return c

let allocated c contract =
  Storage.Contract.Counter.get_option c contract >>=? function
  | None -> return false
  | Some _ -> return true

let exists c contract =
  match Contract_repr.is_implicit contract with
  | Some _ -> return true
  | None -> allocated c contract

let must_exist c contract =
  exists c contract >>=? function
  | true -> return ()
  | false -> fail (Non_existing_contract contract)

let must_be_allocated c contract =
  allocated c contract >>=? function
  | true -> return ()
  | false ->
      match Contract_repr.is_implicit contract with
      | Some pkh -> fail (Empty_implicit_contract pkh)
      | None -> fail (Non_existing_contract contract)

let list c = Storage.Contract.list c

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
  | None, None -> return None
  | Some code, Some storage -> return (Some { Script_repr.code ; storage })
  | None, Some _ | Some _, None -> failwith "get_script"

let get_storage = Storage.Contract.Storage.get_option

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
  | Some _ -> return true
  | None ->
      Storage.Contract.Spendable.mem c contract >>= return

let code_and_storage_fee c contract =
  Storage.Contract.Code_fees.get_option c contract >>=? fun code_fees ->
  Storage.Contract.Storage_fees.get_option c contract >>=? fun storage_fees ->
  match code_fees, storage_fees with
  | (None, Some _) | (Some _, None) ->
      failwith "contract_fee" (* Internal error *)
  | None, None ->
      return Tez_repr.zero
  | Some code_fees, Some storage_fees ->
      Lwt.return Tez_repr.(code_fees +? storage_fees)

let update_storage_fee c contract storage_fees =
  Storage.Contract.Storage_fees.set c contract storage_fees

type big_map_diff = (string * Script_repr.expr option) list

let update_script_storage c contract storage big_map  =
  begin match big_map with
    | None -> return c
    | Some diff ->
        fold_left_s (fun c (key, value) ->
            match value with
            | None ->
                Storage.Contract.Big_map.remove (c, contract) key >>=
                return
            | Some v ->
                Storage.Contract.Big_map.init_set (c, contract) key v >>=
                return)
          c diff
  end >>=? fun c ->
  Storage.Contract.Storage.set c contract storage

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
  Storage.Contract.Balance.get_option c contract >>=? function
  | None -> begin
      match Contract_repr.is_implicit contract with
      | None -> fail (Non_existing_contract contract)
      | Some manager ->
          create_implicit c manager ~balance:amount >>=? fun (c, _) ->
          return c
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
  Storage.Contract.Global_counter.init c 0l

module Big_map = struct
  let set ctxt contract key value =
    Storage.Contract.Big_map.init_set (ctxt, contract) key value >>= return
  let remove ctxt contract = Storage.Contract.Big_map.delete (ctxt, contract)
  let mem ctxt contract = Storage.Contract.Big_map.mem (ctxt, contract)
  let get_opt ctxt contract = Storage.Contract.Big_map.get_option (ctxt, contract)
end
