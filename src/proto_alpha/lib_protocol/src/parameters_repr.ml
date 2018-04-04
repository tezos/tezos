(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bootstrap_account = {
  public_key : Ed25519.Public_key.t ;
  amount : Tez_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  commitments : (Unclaimed_public_key_hash.t * Commitment_repr.t) list
}


let bootstrap_account_encoding =
  let open Data_encoding in
  conv
    (fun { public_key ; amount } -> (public_key, amount))
    (fun (public_key, amount) -> { public_key ; amount })
    (tup2 Ed25519.Public_key.encoding Tez_repr.encoding)

let encoding =
  let open Data_encoding in
  conv
    (fun { bootstrap_accounts ; commitments } ->
       (bootstrap_accounts, commitments))
    (fun ( bootstrap_accounts, commitments ) ->
       { bootstrap_accounts ; commitments })
    (obj2
       (req "bootstrap_accounts" (list bootstrap_account_encoding))
       (req "commitments"
          (list (merge_tups
                   (tup1 Unclaimed_public_key_hash.encoding)
                   Commitment_repr.encoding))))
