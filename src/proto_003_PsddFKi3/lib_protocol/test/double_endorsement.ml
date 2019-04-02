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

(** Double endorsement evidence operation may happen when an endorser
    endorsed two different blocks on the same level. *)

open Proto_alpha
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let get_first_different_baker baker bakers =
  return @@ List.find (fun baker' ->
      Signature.Public_key_hash.(<>) baker baker')
    bakers

let get_first_different_bakers ctxt =
  Context.get_bakers ctxt >>=? fun bakers ->
  let baker_1 = List.hd bakers in
  get_first_different_baker baker_1 (List.tl bakers) >>=? fun baker_2 ->
  return (baker_1, baker_2)

let get_first_different_endorsers ctxt =
  Context.get_endorsers ctxt >>=? fun endorsers ->
  let endorser_1 = (List.hd endorsers) in
  let endorser_2 = (List.hd (List.tl endorsers)) in
  return (endorser_1, endorser_2)

let block_fork b =
  get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >>=? fun blk_b ->
  return (blk_a, blk_b)

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

(** Simple scenario where two endorsements are made from the same
    delegate and exposed by a double_endorsement operation. Also verify
    that punishment is operated. *)
let valid_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->

  block_fork b >>=? fun (blk_a, blk_b) ->

  Context.get_endorser (B blk_a) >>=? fun (delegate, _slots) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->
  Block.bake ~operations:[Operation.pack endorsement_a] blk_a >>=? fun blk_a ->
  (* Block.bake ~operations:[endorsement_b] blk_b >>=? fun _ -> *)

  Op.double_endorsement (B blk_a) endorsement_a endorsement_b >>=? fun operation ->

  (* Bake with someone different than the bad endorser *)
  Context.get_bakers (B blk_a) >>=? fun bakers ->
  get_first_different_baker delegate bakers >>=? fun baker ->

  Block.bake ~policy:(By_account baker) ~operation blk_a >>=? fun blk ->

  (* Check that the frozen deposit, the fees and rewards are removed *)
  iter_s (fun kind ->
      let contract = Alpha_context.Contract.implicit_contract delegate in
      Assert.balance_is ~loc:__LOC__ (B blk) contract ~kind Tez.zero)
    [ Deposit ; Fees ; Rewards ]

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that an invalid double endorsement operation that exposes a valid
    endorsement fails. *)
let invalid_double_endorsement () =
  Context.init 10 >>=? fun (b, _) ->
  Block.bake b >>=? fun b ->

  Op.endorsement (B b) () >>=? fun endorsement ->
  Block.bake ~operation:(Operation.pack endorsement) b >>=? fun b ->

  Op.double_endorsement (B b) endorsement endorsement >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_double_endorsement_evidence -> true
    | _ -> false end

(** Check that a double endorsement added at the same time as a double
    endorsement operation fails. *)
let too_early_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->
  block_fork b >>=? fun (blk_a, blk_b) ->

  Context.get_endorser (B blk_a) >>=? fun (delegate, _) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->

  Op.double_endorsement (B b) endorsement_a endorsement_b >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Too_early_double_endorsement_evidence _ -> true
    | _ -> false end

(** Check that after [preserved_cycles + 1], it is not possible
    to create a double_endorsement anymore. *)
let too_late_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{ parametric = { preserved_cycles ; _ } ; _ } ->

  block_fork b >>=? fun (blk_a, blk_b) ->

  Context.get_endorser (B blk_a) >>=? fun (delegate, _slots) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->

  fold_left_s (fun blk _ -> Block.bake_until_cycle_end blk)
    blk_a (1 -- (preserved_cycles + 1)) >>=? fun blk ->

  Op.double_endorsement (B blk) endorsement_a endorsement_b >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Outdated_double_endorsement_evidence _ -> true
    | _ -> false end

(** Check that an invalid double endorsement evidence that expose two
    endorsements made by two different endorsers fails. *)
let different_delegates () =
  Context.init 2 >>=? fun (b, _) ->

  Block.bake b >>=? fun b ->
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (endorser_a, _a_slots) ->
  get_first_different_endorsers (B blk_b) >>=? fun (endorser_b1c, endorser_b2c) ->
  let endorser_b =
    if Signature.Public_key_hash.(=) endorser_a endorser_b1c.delegate
    then endorser_b2c.delegate
    else endorser_b1c.delegate
  in

  Op.endorsement ~delegate:endorser_a (B blk_a) () >>=? fun e_a ->
  Op.endorsement ~delegate:endorser_b (B blk_b) () >>=? fun e_b ->
  Block.bake ~operation:(Operation.pack e_b) blk_b >>=? fun _ ->
  Op.double_endorsement (B blk_b) e_a e_b >>=? fun operation ->
  Block.bake ~operation blk_b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Inconsistent_double_endorsement_evidence _ -> true
    | _ -> false end

(** Check that a double endorsement evidence that exposes a ill-formed
    endorsement fails. *)
let wrong_delegate () =
  Context.init ~endorsers_per_block:1 2 >>=? fun (b, contracts) ->
  Error_monad.map_s (Context.Contract.manager (B b)) contracts >>=? fun accounts ->
  let pkh1 = (List.nth accounts 0).Account.pkh in
  let pkh2 = (List.nth accounts 1).Account.pkh in

  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (endorser_a, _) ->
  Op.endorsement ~delegate:endorser_a (B blk_a) () >>=? fun endorsement_a ->
  Context.get_endorser (B blk_b) >>=? fun (endorser_b, _) ->
  let delegate =
    if Signature.Public_key_hash.equal pkh1 endorser_b
    then pkh2
    else pkh1
  in
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->

  Op.double_endorsement (B blk_b) endorsement_a endorsement_b >>=? fun operation ->
  Block.bake ~operation blk_b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e begin function
    | Baking.Unexpected_endorsement -> true
    | _ -> false end

let tests = [
  Test.tztest "valid double endorsement evidence" `Quick valid_double_endorsement_evidence ;
  Test.tztest "invalid double endorsement evidence" `Quick invalid_double_endorsement ;
  Test.tztest "too early double endorsement evidence" `Quick too_early_double_endorsement_evidence ;
  Test.tztest "too late double endorsement evidence" `Quick too_late_double_endorsement_evidence ;
  Test.tztest "different delegates" `Quick different_delegates ;
  Test.tztest "wrong delegate" `Quick wrong_delegate ;
]
