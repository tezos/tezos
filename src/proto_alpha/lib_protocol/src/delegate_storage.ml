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

(* A delegate is registred if its "implicit account"
   delegates to itself. *)
let registred c delegate =
  Storage.Contract.Delegate.mem
    c (Contract_repr.implicit_contract delegate)

let init ctxt contract delegate =
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
      registred c delegate >>= fun registred_delegate ->
      is_delegatable c contract >>=? fun delegatable ->
      let self_delegation =
        match Contract_repr.is_implicit contract with
        | Some pkh -> Ed25519.Public_key_hash.equal pkh delegate
        | None -> false in
      if not known_delegate || not (registred_delegate || self_delegation) then
        fail (Roll_storage.Unregistred_delegate delegate)
      else if not (delegatable || self_delegation) then
        fail (Non_delegatable_contract contract)
      else
        Storage.Contract.Balance.get c contract >>=? fun balance ->
        unlink c contract balance >>=? fun c ->
        Storage.Contract.Delegate.init_set c contract delegate >>= fun c ->
        link c contract delegate balance >>=? fun c ->
        return c

let remove ctxt contract =
  Storage.Contract.Balance.get ctxt contract >>=? fun balance ->
  unlink ctxt contract balance
