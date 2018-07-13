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

let commitment_secret =
  Proto_alpha.Blinded_public_key_hash.activation_code_of_hex
    "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"

let known_accounts = Signature.Public_key_hash.Table.create 17

let new_account ?seed () =
  let (pkh, pk, sk) = Signature.generate_key ?seed ~algo:Ed25519 () in
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

let new_commitment ?seed () =
  let (pkh, pk, sk) = Signature.generate_key ?seed ~algo:Ed25519 () in
  let unactivated_account = { pkh; pk; sk } in
  let open Proto_alpha in
  let open Commitment_repr in
  let pkh = match pkh with Ed25519 pkh -> pkh | _ -> assert false in
  let bpkh = Blinded_public_key_hash.of_ed25519_pkh commitment_secret pkh in
  Lwt.return @@ Alpha_environment.wrap_error @@
  Tez_repr.(one *? 4_000L) >>=? fun amount ->
  return @@ (unactivated_account, { blinded_public_key_hash = bpkh ; amount })

let generate_accounts n : (t * Tez_repr.t) list =
  Signature.Public_key_hash.Table.clear known_accounts ;
  let amount = Tez_repr.of_mutez_exn 4_000_000_000_000L in
  List.map (fun _ ->
      let (pkh, pk, sk) = Signature.generate_key () in
      let account = { pkh ; pk ; sk } in
      Signature.Public_key_hash.Table.add known_accounts pkh account ;
      account, amount)
    (0--(n-1))
