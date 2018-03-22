(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let init ctxt (account: Parameters_repr.bootstrap_account) =
  let public_key_hash = Signature.Public_key.hash account.public_key in
  let contract = Contract_repr.implicit_contract public_key_hash in
  Contract_storage.credit ctxt contract account.amount >>=? fun ctxt ->
  Contract_storage.reveal_manager_key ctxt contract account.public_key >>=? fun ctxt ->
  Delegate_storage.set ctxt contract (Some public_key_hash) >>=? fun ctxt ->
  return ctxt

let init ctxt accounts =
  fold_left_s init ctxt accounts
