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

(**

   Basic roll manipulation.

   If storage related to roll (a.k.a. `Storage.Roll`) are not used
   outside of this module, this interface enforces the invariant that a
   roll is always either in the limbo list or in a contract list.

*)

type error +=
  | Consume_roll_change
  | No_roll_for_delegate
  | No_roll_snapshot_for_cycle of Cycle_repr.t
  | Unregistered_delegate of Signature.Public_key_hash.t (* `Permanent *)

val init : Raw_context.t -> Raw_context.t tzresult Lwt.t
val init_first_cycles : Raw_context.t -> Raw_context.t tzresult Lwt.t

val cycle_end : Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
val snapshot_rolls : Raw_context.t -> Raw_context.t tzresult Lwt.t


val fold :
  Raw_context.t ->
  f:(Roll_repr.roll -> Signature.Public_key.t -> 'a -> 'a tzresult Lwt.t) ->
  'a -> 'a tzresult Lwt.t

val baking_rights_owner :
  Raw_context.t -> Level_repr.t -> priority:int ->
  Signature.Public_key.t tzresult Lwt.t

val endorsement_rights_owner :
  Raw_context.t -> Level_repr.t -> slot:int ->
  Signature.Public_key.t tzresult Lwt.t

module Delegate : sig

  val is_inactive :
    Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

  val add_amount :
    Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val remove_amount :
    Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val set_inactive : Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

  val set_active : Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

end

module Contract : sig

  val add_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

  val remove_amount :
    Raw_context.t -> Contract_repr.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

end

val delegate_pubkey:
  Raw_context.t -> Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t

val get_rolls:
  Raw_context.t -> Signature.Public_key_hash.t -> Roll_repr.t list tzresult Lwt.t
val get_change:
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

(**/**)

val get_contract_delegate:
  Raw_context.t -> Contract_repr.t -> Signature.Public_key_hash.t option tzresult Lwt.t
