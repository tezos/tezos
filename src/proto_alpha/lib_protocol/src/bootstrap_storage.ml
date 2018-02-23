(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type account = {
  public_key_hash : Ed25519.Public_key_hash.t ;
  public_key : Ed25519.Public_key.t ;
}

let init_account ctxt account =
  let contract = Contract_repr.implicit_contract account.public_key_hash in
  Contract_storage.credit ctxt contract
    Constants_repr.bootstrap_wealth >>=? fun ctxt ->
  Contract_storage.update_manager_key ctxt contract
    (Some account.public_key) >>=? fun (ctxt, _) ->
  Delegate_storage.set ctxt contract
    (Some account.public_key_hash) >>=? fun ctxt ->
  return ctxt


let make public_key =
  { public_key ; public_key_hash = Ed25519.Public_key.hash public_key }

let accounts ctxt =
  let { Constants_repr.bootstrap_keys } = Raw_context.constants ctxt in
  List.map make bootstrap_keys

let init ctxt =
  fold_left_s init_account ctxt (accounts ctxt) >>=? fun ctxt ->
  return ctxt

let account_encoding =
  let open Data_encoding in
  conv
    (fun {public_key_hash ; public_key } ->
       (public_key_hash, public_key))
    (fun (public_key_hash, public_key) ->
       { public_key_hash ; public_key })
    (obj2
       (req "publicKeyHash" Ed25519.Public_key_hash.encoding)
       (req "publicKey" Ed25519.Public_key.encoding))
