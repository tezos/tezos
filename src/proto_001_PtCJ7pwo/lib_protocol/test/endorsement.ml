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

(** Endorsing a block adds an extra layer of confidence to the Tezos's
    PoS algorithm. The block endorsing operation must be included in
    the following block. Each endorser possess a slot corresponding to
    their priority. After [preserved_cycles], a reward is given to the
    endorser. This reward depends on the priority of the endorsed
    block. *)

open Proto_001_PtCJ7pwo
open Alpha_context
open Test_utils
open Test_tez

(****************************************************************)
(*                    Utility functions                         *)
(****************************************************************)

let get_expected_reward ?(priority=0) ~nb_baking ~nb_endorsement ctxt =
  Context.get_constants ctxt >>=? fun Constants.
    { parametric = { endorsement_reward ; block_reward ; _ } ; _  } ->
  let open Alpha_environment in let open Tez in
  endorsement_reward /? Int64.(succ (of_int priority)) >>?= fun endorsement_reward ->

  endorsement_reward *? (Int64.of_int nb_endorsement) >>?= fun endorsement_reward ->
  block_reward *? (Int64.of_int nb_baking) >>?= fun baking_reward ->
  endorsement_reward +? baking_reward >>?= fun reward -> return reward

let get_expected_deposit ctxt ~nb_baking ~nb_endorsement =
  Context.get_constants ctxt >>=? fun Constants.
    { parametric = { endorsement_security_deposit ;
                     block_security_deposit ; _ } ; _  } ->
  let open Alpha_environment in let open Tez in
  endorsement_security_deposit *? (Int64.of_int nb_endorsement) >>?= fun endorsement_deposit ->
  block_security_deposit *? (Int64.of_int nb_baking) >>?= fun baking_deposit ->
  endorsement_deposit +? baking_deposit >>?= fun deposit -> return deposit

let assert_endorser_balance_consistency ~loc ?(priority=0) ?(nb_baking=0) ~nb_endorsement
    ctxt pkh initial_balance =
  let contract = Contract.implicit_contract pkh in
  get_expected_reward ~priority ~nb_baking ~nb_endorsement ctxt >>=? fun reward ->
  get_expected_deposit ctxt ~nb_baking ~nb_endorsement >>=? fun deposit ->

  Assert.balance_was_debited ~loc ctxt contract initial_balance deposit >>=? fun () ->
  Context.Contract.balance ~kind:Rewards ctxt contract >>=? fun reward_balance ->
  Assert.equal_tez ~loc reward_balance reward >>=? fun () ->
  Context.Contract.balance ~kind:Deposit ctxt contract >>=? fun deposit_balance ->
  Assert.equal_tez ~loc deposit_balance deposit

(****************************************************************)
(*                      Tests                                   *)
(****************************************************************)

(** Apply a single endorsement from the slot 0 endorser *)
let simple_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, slots) ->
  Op.endorsement ~delegate (B b) () >>=? fun op ->
  Context.Contract.balance (B b)
    (Contract.implicit_contract delegate) >>=? fun initial_balance ->
  Block.bake
    ~policy:(Excluding [delegate])
    ~operations:[Operation.pack op]
    b >>=? fun b2 ->
  assert_endorser_balance_consistency ~loc:__LOC__
    (B b2) ~nb_endorsement:(List.length slots)
    delegate initial_balance

(** Apply a maximum number of endorsement. A endorser can be selected
    twice. *)
let max_endorsement () =
  let endorsers_per_block = 16 in
  Context.init ~endorsers_per_block 32 >>=? fun (b, _) ->

  Context.get_endorsers (B b) >>=? fun endorsers ->
  Assert.equal_int ~loc:__LOC__
    (List.length (List.concat (List.map (fun { Alpha_services.Delegate.Endorsing_rights.slots } -> slots) endorsers))) endorsers_per_block >>=? fun () ->

  fold_left_s (fun (delegates, ops, balances) (endorser : Alpha_services.Delegate.Endorsing_rights.t) ->
      let delegate = endorser.delegate in
      Context.Contract.balance (B b) (Contract.implicit_contract delegate) >>=? fun balance ->
      Op.endorsement ~delegate (B b) () >>=? fun op ->
      return (delegate :: delegates, Operation.pack op :: ops, (List.length endorser.slots, balance) :: balances)
    )
    ([], [], [])
    endorsers >>=? fun (delegates, ops, previous_balances) ->

  Block.bake ~policy:(Excluding delegates) ~operations:(List.rev ops) b >>=? fun b ->

  (* One account can endorse more than one time per level, we must
     check that the bonds are summed up *)
  iter_s (fun (endorser_account, (nb_endorsement, previous_balance)) ->
      assert_endorser_balance_consistency ~loc:__LOC__
        (B b) ~nb_endorsement endorser_account previous_balance
    ) (List.combine delegates previous_balances)

(** Check that an endorser balance is consistent with a different priority *)
let consistent_priority () =
  Context.init 32 >>=? fun (b, _) ->
  Block.get_next_baker ~policy:(By_priority 15) b >>=? fun (baker_account, _, _) ->
  Block.bake ~policy:(By_priority 15) b >>=? fun b ->

  (* Grab an endorser that didn't bake the previous block *)
  Context.get_endorsers (B b) >>=? fun endorsers ->
  let endorser =
    List.find
      (fun e -> e.Delegate_services.Endorsing_rights.delegate <> baker_account)
      endorsers in
  Context.Contract.balance (B b) (Contract.implicit_contract endorser.delegate) >>=? fun balance ->

  Op.endorsement ~delegate:endorser.delegate (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~policy:( Excluding [ endorser.delegate ] ) ~operation b >>=? fun b ->

  assert_endorser_balance_consistency ~loc:__LOC__ ~priority:15
    (B b) ~nb_endorsement:(List.length endorser.slots) endorser.delegate balance

(** Check every 32 endorser's balances are consistent with a different priority *)
let consistent_priorities () =
  let priorities = 15 -- 31 in
  Context.init 64 >>=? fun (b, _) ->

  iter_s (fun priority ->
      (* Bake with a specific priority *)
      Block.get_next_baker ~policy:(By_priority priority) b >>=? fun (baker_account, _, _) ->
      Block.bake ~policy:(By_priority priority) b >>=? fun b ->

      (* Grab an endorser that didn't bake the previous block *)
      Context.get_endorsers (B b) >>=? fun endorsers ->
      let endorser =
        List.find
          (fun e -> e.Delegate_services.Endorsing_rights.delegate <> baker_account)
          endorsers in

      Context.Contract.balance (B b) (Contract.implicit_contract endorser.delegate) >>=? fun balance ->
      Op.endorsement ~delegate:endorser.delegate (B b) () >>=? fun operation ->
      let operation = Operation.pack operation in
      Block.bake ~policy:( Excluding [ endorser.delegate ] ) ~operation b >>=? fun b ->

      assert_endorser_balance_consistency ~loc:__LOC__ ~priority
        (B b) ~nb_endorsement:(List.length endorser.slots) endorser.delegate balance
    ) priorities

(** Check that after a cycle the endorser gets his reward *)
let reward_retrieval () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun Constants.
    { parametric = { preserved_cycles ; endorsement_reward ; _ } ; _ } ->

  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  Context.Contract.balance (B b) (Contract.implicit_contract endorser) >>=? fun balance ->
  Op.endorsement ~delegate:endorser (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~policy:(Excluding [ endorser ]) ~operation b >>=? fun b ->
  (* Bake (preserved_cycles + 1) cycles *)
  fold_left_s (fun b _ ->
      Block.bake_until_cycle_end ~policy:(Excluding [ endorser ]) b
    ) b (0 -- preserved_cycles) >>=? fun b ->

  Lwt.return Tez.(endorsement_reward *? Int64.of_int (List.length slots)) >>=? fun reward ->
  Assert.balance_was_credited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser) balance reward

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Wrong endorsement predecessor : apply an endorsement with an
    incorrect block predecessor *)
let wrong_endorsement_predecessor () =
  Context.init 5 >>=? fun (b, _) ->

  Context.get_endorser (B b) >>=? fun (genesis_endorser, _slots) ->
  Block.bake b >>=? fun b' ->
  Op.endorsement ~delegate:genesis_endorser ~signing_context:(B b) (B b') () >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~operation b' >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Wrong_endorsement_predecessor _ -> true
    | _ -> false
  end

(** Invalid_endorsement_level : apply an endorsement with an incorrect
    level (i.e. the predecessor level) *)
let invalid_endorsement_level () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_level (B b) >>=? fun genesis_level ->
  Block.bake b >>=? fun b ->
  Op.endorsement ~level:genesis_level (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~operation b >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_endorsement_level -> true
    | _ -> false
  end

(** Duplicate endorsement : apply an endorsement that has already been done *)
let duplicate_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Incremental.begin_construction b >>=? fun inc ->
  Op.endorsement (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Incremental.add_operation inc operation >>=? fun inc ->
  Op.endorsement (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Incremental.add_operation inc operation >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Duplicate_endorsement _ -> true
    | _ -> false
  end

(** Apply a single endorsement from the slot 0 endorser *)
let no_enough_for_deposit () =
  Context.init 5 ~endorsers_per_block:1 >>=? fun (b, contracts) ->
  Error_monad.map_s (fun c ->
      Context.Contract.manager (B b) c >>=? fun m -> return (m, c)) contracts >>=?
  fun managers ->
  Context.get_endorser (B b) >>=? fun (endorser,_) ->
  let _, contract_other_than_endorser =
    List.find (fun (c, _) -> not (Signature.Public_key_hash.equal c.Account.pkh endorser))
      managers
  in
  let _, contract_of_endorser =
    List.find (fun (c, _) -> (Signature.Public_key_hash.equal c.Account.pkh endorser))
      managers
  in
  Op.endorsement ~delegate:endorser (B b) () >>=? fun op_endo ->
  Context.Contract.balance (B b)
    (Contract.implicit_contract endorser) >>=? fun initial_balance ->
  Op.transaction (B b) contract_of_endorser contract_other_than_endorser initial_balance >>=? fun op_trans ->
  Block.bake
    ~policy:(Excluding [endorser])
    ~operations:[Operation.pack op_endo; op_trans]
    b >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Delegate_storage.Balance_too_low_for_deposit _ -> true
    | _ -> false
  end

let tests = [
  Test.tztest "Simple endorsement" `Quick simple_endorsement ;
  Test.tztest "Maximum endorsement" `Quick max_endorsement ;

  Test.tztest "Consistent priority" `Quick consistent_priority ;
  Test.tztest "Consistent priorities" `Quick consistent_priorities ;
  Test.tztest "Reward retrieval" `Quick reward_retrieval ;

  (* Fail scenarios *)
  Test.tztest "Wrong endorsement predecessor" `Quick wrong_endorsement_predecessor ;
  Test.tztest "Invalid endorsement level" `Quick invalid_endorsement_level ;
  Test.tztest "Duplicate endorsement" `Quick duplicate_endorsement ;
  Test.tztest "Not enough for deposit" `Quick no_enough_for_deposit ;
]
