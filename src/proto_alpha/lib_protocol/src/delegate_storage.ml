(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Non_delegatable_contract of Contract_repr.contract (* `Permanent *)
  | No_deletion of Ed25519.Public_key_hash.t (* `Permanent *)
  | Active_delegate (* `Temporary *)
  | Current_delegate (* `Temporary *)

let () =
  register_error_kind
    `Permanent
    ~id:"contract.undelagatable_contract"
    ~title:"Non delegatable contract"
    ~description:"Tried to delegate a implicit contract \
                  or a non delegatable originated contract"
    ~pp:(fun ppf contract ->
        Format.fprintf ppf "Contract %a is not delegatable"
          Contract_repr.pp contract)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Non_delegatable_contract c -> Some c | _ -> None)
    (fun c -> Non_delegatable_contract c) ;
  register_error_kind
    `Permanent
    ~id:"delegate.no_deletion"
    ~title:"Forbidden delegate deletion"
    ~description:"Tried to unregister a delegate"
    ~pp:(fun ppf delegate ->
        Format.fprintf ppf "Delegate deletion is forbidden (%a)"
          Ed25519.Public_key_hash.pp delegate)
    Data_encoding.(obj1 (req "delegate" Ed25519.Public_key_hash.encoding))
    (function No_deletion c -> Some c | _ -> None)
    (fun c -> No_deletion c)

let is_delegatable c contract =
  match Contract_repr.is_implicit contract with
  | Some _ ->
      return false
  | None ->
      Storage.Contract.Delegatable.mem c contract >>= return

let link c contract delegate balance =
  Roll_storage.Delegate.add_amount c delegate balance >>=? fun c ->
  match Contract_repr.is_originated contract with
  | None -> return c
  | Some h ->
      Storage.Contract.Delegated.add
        (c, Contract_repr.implicit_contract delegate) h >>= fun c ->
      return c

let unlink c contract balance =
  Storage.Contract.Delegate.get_option c contract >>=? function
  | None -> return c
  | Some delegate ->
      match Contract_repr.is_originated contract with
      | None -> return c
      | Some h ->
          Roll_storage.Delegate.remove_amount c delegate balance >>=? fun c ->
          Storage.Contract.Delegated.del
            (c, Contract_repr.implicit_contract delegate) h >>= fun c ->
          return c

let known c delegate =
  Storage.Contract.Manager.get_option
    c (Contract_repr.implicit_contract delegate) >>=? function
  | None | Some (Manager_repr.Hash _) -> return false
  | Some (Manager_repr.Public_key _) -> return true

(* A delegate is registered if its "implicit account"
   delegates to itself. *)
let registered c delegate =
  Storage.Contract.Delegate.mem
    c (Contract_repr.implicit_contract delegate)

let init ctxt contract delegate =
  Storage.Contract.Delegate.init ctxt contract delegate >>=? fun ctxt ->
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  link ctxt contract delegate balance

let get = Roll_storage.get_contract_delegate

let set c contract delegate =
  match delegate with
  | None -> begin
      match Contract_repr.is_implicit contract with
      | Some pkh ->
          fail (No_deletion pkh)
      | None ->
          Storage.Contract.Balance.get c contract >>=? fun balance ->
          unlink c contract balance >>=? fun c ->
          Storage.Contract.Delegate.remove c contract >>= fun c ->
          return c
    end
  | Some delegate ->
      known c delegate >>=? fun known_delegate ->
      registered c delegate >>= fun registered_delegate ->
      is_delegatable c contract >>=? fun delegatable ->
      let self_delegation =
        match Contract_repr.is_implicit contract with
        | Some pkh -> Ed25519.Public_key_hash.equal pkh delegate
        | None -> false in
      if not known_delegate || not (registered_delegate || self_delegation) then
        fail (Roll_storage.Unregistered_delegate delegate)
      else if not (delegatable || self_delegation) then
        fail (Non_delegatable_contract contract)
      else
        begin
          Storage.Contract.Delegate.get_option c contract >>=? function
          | Some current_delegate
            when Ed25519.Public_key_hash.equal delegate current_delegate ->
              if self_delegation then
                Storage.Contract.Inactive_delegate.mem c contract >>= function
                | true -> return ()
                | false -> fail Active_delegate
              else
                fail Current_delegate
          | None | Some _ -> return ()
        end >>=? fun () ->
        Storage.Contract.Balance.get c contract >>=? fun balance ->
        unlink c contract balance >>=? fun c ->
        Storage.Contract.Delegate.init_set c contract delegate >>= fun c ->
        link c contract delegate balance >>=? fun c ->
        begin
          if self_delegation then
            Storage.Delegates.add c delegate
          else
            Lwt.return c
        end >>= fun c ->
        return c

let remove ctxt contract =
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  unlink ctxt contract balance

let fold = Storage.Delegates.fold
let list = Storage.Delegates.elements


let get_frozen_bond ctxt contract cycle =
  Storage.Contract.Frozen_bonds.get_option (ctxt, contract) cycle >>=? function
  | None -> return Tez_repr.zero
  | Some frozen -> return frozen

let credit_frozen_bond ctxt contract cycle amount =
  get_frozen_bond ctxt contract cycle >>=? fun old_amount ->
  Lwt.return Tez_repr.(old_amount +? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_bonds.init_set
    (ctxt, contract) cycle new_amount >>= fun ctxt ->
  return ctxt

let freeze_bond ctxt delegate amount =
  let { Level_repr.cycle ; _ } = Level_storage.current ctxt in
  let contract = Contract_repr.implicit_contract delegate in
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  Lwt.return Tez_repr.(balance -? amount) >>=? fun new_balance ->
  Storage.Contract.Balance.set ctxt contract new_balance >>=? fun ctxt ->
  credit_frozen_bond ctxt contract cycle amount

let burn_bond ctxt delegate cycle amount =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_bond ctxt contract cycle >>=? fun old_amount ->
  Roll_storage.Delegate.remove_amount ctxt delegate amount >>=? fun ctxt ->
  Lwt.return Tez_repr.(old_amount -? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_bonds.set (ctxt, contract) cycle new_amount



let get_frozen_fees ctxt contract cycle =
  Storage.Contract.Frozen_fees.get_option (ctxt, contract) cycle >>=? function
  | None -> return Tez_repr.zero
  | Some frozen -> return frozen

let credit_frozen_fees ctxt contract cycle amount =
  get_frozen_fees ctxt contract cycle >>=? fun old_amount ->
  Lwt.return Tez_repr.(old_amount +? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_fees.init_set
    (ctxt, contract) cycle new_amount >>= fun ctxt ->
  return ctxt

let freeze_fees ctxt delegate amount =
  let { Level_repr.cycle ; _ } = Level_storage.current ctxt in
  let contract = Contract_repr.implicit_contract delegate in
  Roll_storage.Delegate.add_amount ctxt delegate amount >>=? fun ctxt ->
  credit_frozen_fees ctxt contract cycle amount

let burn_fees ctxt delegate cycle amount =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_fees ctxt contract cycle >>=? fun old_amount ->
  Roll_storage.Delegate.remove_amount ctxt delegate amount >>=? fun ctxt ->
  Lwt.return Tez_repr.(old_amount -? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_fees.set (ctxt, contract) cycle new_amount


let get_frozen_rewards ctxt contract cycle =
  Storage.Contract.Frozen_rewards.get_option (ctxt, contract) cycle >>=? function
  | None -> return Tez_repr.zero
  | Some frozen -> return frozen

let credit_frozen_rewards ctxt contract cycle amount =
  get_frozen_rewards ctxt contract cycle >>=? fun old_amount ->
  Lwt.return Tez_repr.(old_amount +? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_rewards.init_set
    (ctxt, contract) cycle new_amount >>= fun ctxt ->
  return ctxt

let freeze_rewards ctxt delegate amount =
  let { Level_repr.cycle ; _ } = Level_storage.current ctxt in
  let contract = Contract_repr.implicit_contract delegate in
  credit_frozen_rewards ctxt contract cycle amount

let burn_rewards ctxt delegate cycle amount =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_rewards ctxt contract cycle >>=? fun old_amount ->
  Lwt.return Tez_repr.(old_amount -? amount) >>=? fun new_amount ->
  Storage.Contract.Frozen_rewards.set (ctxt, contract) cycle new_amount



let unfreeze ctxt delegate cycle =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_bond ctxt contract cycle >>=? fun bond ->
  get_frozen_fees ctxt contract cycle >>=? fun fees ->
  get_frozen_rewards ctxt contract cycle >>=? fun rewards ->
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  Lwt.return Tez_repr.(balance +? bond) >>=? fun balance ->
  Lwt.return Tez_repr.(balance +? fees) >>=? fun balance ->
  Lwt.return Tez_repr.(balance +? rewards) >>=? fun balance ->
  Storage.Contract.Balance.set ctxt contract balance >>=? fun ctxt ->
  Roll_storage.Delegate.add_amount ctxt delegate rewards >>=? fun ctxt ->
  Storage.Contract.Frozen_bonds.remove (ctxt, contract) cycle >>= fun ctxt ->
  Storage.Contract.Frozen_fees.remove (ctxt, contract) cycle >>= fun ctxt ->
  Storage.Contract.Frozen_rewards.remove (ctxt, contract) cycle >>= fun ctxt ->
  return ctxt

let cycle_end ctxt last_cycle unrevealed =
  let preserved = Constants_storage.preserved_cycles ctxt in
  begin
    match Cycle_repr.pred last_cycle with
    | None -> return ctxt
    | Some revealed_cycle ->
        List.fold_left
          (fun ctxt (u : Nonce_storage.unrevealed) ->
             ctxt >>=? fun ctxt ->
             burn_bond
               ctxt u.delegate revealed_cycle u.bond >>=? fun ctxt ->
             burn_fees
               ctxt u.delegate revealed_cycle u.fees >>=? fun ctxt ->
             burn_rewards
               ctxt u.delegate revealed_cycle u.rewards >>=? fun ctxt ->
             return ctxt)
          (return ctxt) unrevealed
  end >>=? fun ctxt ->
  match Cycle_repr.sub last_cycle preserved with
  | None -> return ctxt
  | Some unfrozen_cycle ->
      fold ctxt
        ~init:(Ok ctxt)
        ~f:(fun delegate ctxt ->
            Lwt.return ctxt >>=? fun ctxt ->
            unfreeze ctxt delegate unfrozen_cycle)


let punish ctxt delegate cycle =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_bond ctxt contract cycle >>=? fun bond ->
  get_frozen_fees ctxt contract cycle >>=? fun fees ->
  Roll_storage.Delegate.remove_amount ctxt delegate bond >>=? fun ctxt ->
  Roll_storage.Delegate.remove_amount ctxt delegate fees >>=? fun ctxt ->
  Storage.Contract.Frozen_bonds.remove (ctxt, contract) cycle >>= fun ctxt ->
  Storage.Contract.Frozen_fees.remove (ctxt, contract) cycle >>= fun ctxt ->
  Storage.Contract.Frozen_rewards.remove (ctxt, contract) cycle >>= fun ctxt ->
  Lwt.return Tez_repr.(bond +? fees) >>=? fun burned ->
  return (ctxt, burned)


let has_frozen_balance ctxt delegate cycle =
  let contract = Contract_repr.implicit_contract delegate in
  get_frozen_bond ctxt contract cycle >>=? fun bond ->
  if Tez_repr.(bond <> zero) then return true
  else
    get_frozen_fees ctxt contract cycle >>=? fun fees ->
    if Tez_repr.(fees <> zero) then return true
    else
      get_frozen_rewards ctxt contract cycle >>=? fun rewards ->
      return Tez_repr.(rewards <> zero)

let frozen_balance ctxt delegate =
  let contract = Contract_repr.implicit_contract delegate in
  let balance = Ok Tez_repr.zero in
  Storage.Contract.Frozen_bonds.fold
    (ctxt, contract) ~init:balance
    ~f:(fun _cycle amount acc ->
        Lwt.return acc >>=? fun acc ->
        Lwt.return (Tez_repr.(acc +? amount))) >>= fun  balance ->
  Storage.Contract.Frozen_fees.fold
    (ctxt, contract) ~init:balance
    ~f:(fun _cycle amount acc ->
        Lwt.return acc >>=? fun acc ->
        Lwt.return (Tez_repr.(acc +? amount))) >>= fun  balance ->
  Storage.Contract.Frozen_rewards.fold
    (ctxt, contract) ~init:balance
    ~f:(fun _cycle amount acc ->
        Lwt.return acc >>=? fun acc ->
        Lwt.return (Tez_repr.(acc +? amount))) >>= fun  balance ->
  Lwt.return balance

let full_balance ctxt delegate =
  let contract = Contract_repr.implicit_contract delegate in
  frozen_balance ctxt delegate >>=? fun frozen_balance ->
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  Lwt.return Tez_repr.(frozen_balance +? balance)
