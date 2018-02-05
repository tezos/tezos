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
  Storage.Public_key.init ctxt account.public_key_hash account.public_key >>=? fun ctxt ->
  Contract_storage.credit
    ctxt
    (Contract_repr.default_contract account.public_key_hash)
    Constants_repr.bootstrap_wealth >>=? fun ctxt ->
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

let refill ctxt =
  (* Unefficient HACK for the alphanet only... *)
  Contract_storage.list ctxt >>= fun contracts ->
  List.fold_left
    (fun total contract ->
       Contract_storage.get_balance ctxt contract >>=? fun balance ->
       total >>=? fun total -> Lwt.return Tez_repr.(total +? balance))
    (return Tez_repr.zero) contracts >>=? fun total ->
  (* The bootstrap accounts should have at least 1/2 of the total amount
     of tokens. *)
  let accounts = accounts ctxt in
  let min_balance =
    Tez_repr.(total /? 2L >>? fun r -> r /? (Int64.of_int (List.length accounts))) in
  fold_left_s
    (fun ctxt account ->
       let contract =
         Contract_repr.default_contract account.public_key_hash in
       Contract_storage.get_balance ctxt contract >>=? fun balance ->
       match Tez_repr.(min_balance >>? fun r -> r -? balance) with
       | Error _ -> return ctxt
       | Ok tez -> Contract_storage.credit ctxt contract tez)
    ctxt
    accounts >>=? fun c ->
  Roll_storage.may_recompute_rolls c >>=? fun c ->
  return c
