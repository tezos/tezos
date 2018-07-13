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

val commitment_secret : Blinded_public_key_hash.activation_code

val dictator_account: account
val dummy_account: account

val new_account: ?seed : MBytes.t -> unit -> account

val new_commitment : ?seed:MBytes.t -> unit ->
  (account * Commitment_repr.t) tzresult Lwt.t

val add_account : t -> unit

val known_accounts : t Signature.Public_key_hash.Table.t

val find: Signature.Public_key_hash.t -> t tzresult Lwt.t
val find_alternate: Signature.Public_key_hash.t -> t

(** [generate_accounts n] : generates [n] random accounts with
    4.000.000.000 tz and add them to the global account state *)
val generate_accounts : int -> (t * Tez_repr.t) list
