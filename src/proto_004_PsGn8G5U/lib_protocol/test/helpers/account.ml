(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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

let activator_account = new_account ()

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

let generate_accounts ?(initial_balances = []) n : (t * Tez_repr.t) list =
  Signature.Public_key_hash.Table.clear known_accounts ;
  let default_amount = Tez_repr.of_mutez_exn 4_000_000_000_000L in
  let amount i = match List.nth_opt initial_balances i with
    | None -> default_amount
    | Some a -> Tez_repr.of_mutez_exn a
  in
  List.map (fun i ->
      let (pkh, pk, sk) = Signature.generate_key () in
      let account = { pkh ; pk ; sk } in
      Signature.Public_key_hash.Table.add known_accounts pkh account ;
      account, amount i)
    (0--(n-1))
