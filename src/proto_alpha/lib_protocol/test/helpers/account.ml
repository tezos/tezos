(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

type t = {
  pkh : Signature.Public_key_hash.t ;
  pk :  Signature.Public_key.t ;
  sk :  Signature.Secret_key.t ;
}
type account = t

let known_accounts = Signature.Public_key_hash.Table.create 17

let new_account () =
  let (pkh, pk, sk) = Signature.generate_key () in
  let account = { pkh ; pk ; sk } in
  Signature.Public_key_hash.Table.add known_accounts pkh account ;
  account

let add_account ({ pkh ; _ } as account) =
  Signature.Public_key_hash.Table.add known_accounts pkh account

let dictator_account = new_account ()

let find pkh =
  try return (Signature.Public_key_hash.Table.find known_accounts pkh)
  with Not_found ->
    failwith "Missing account: %a" Signature.Public_key_hash.pp pkh

let find_alternate pkh =
  let exception Found of t in
  try
    Signature.Public_key_hash.Table.iter
      (fun pkh' account ->
         if not (Signature.Public_key_hash.equal pkh pkh') then
           raise (Found account))
      known_accounts ;
    raise Not_found
  with Found account -> account

let dummy_account = new_account ()

let generate_accounts n : (t * Tez_repr.t) list =
  Signature.Public_key_hash.Table.clear known_accounts ;
  let amount = Tez_repr.of_mutez_exn 4_000_000_000_000L in
  List.map (fun _ ->
      let (pkh, pk, sk) = Signature.generate_key () in
      let account = { pkh ; pk ; sk } in
      Signature.Public_key_hash.Table.add known_accounts pkh account ;
      account, amount)
    (0--(n-1))
