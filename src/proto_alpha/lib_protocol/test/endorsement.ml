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

open Proto_alpha
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

(* calculate the smallest sublist of delegates -- with their first slot -- such that
   their total number of slots >= needed_endorsements;
   these delegates can provide the necessary endorsements for a block to be baked *)
let rec get_min_delegates_to_endorse acc_s acc_delegates endorsers needed_endorsements =
  match endorsers with
  | [] -> acc_delegates
  | endorser :: tail ->
      begin
        let delegate = endorser.Delegate_services.Endorsing_rights.delegate in
        let slots = endorser.Delegate_services.Endorsing_rights.slots in
        let new_s = acc_s + (List.length slots) in
        let new_acc_delegates = (delegate, List.hd slots) :: acc_delegates in
        if new_s < needed_endorsements then
          get_min_delegates_to_endorse new_s new_acc_delegates tail needed_endorsements
        else new_acc_delegates
      end

(****************************************************************)
(*                      Tests                                   *)
(****************************************************************)

(** Apply a single endorsement from the slot 0 endorser *)
let simple_endorsement () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_endorser (B b) >>=? fun (delegate, slots) ->
  Op.endorsement ~delegate:(delegate, List.hd slots) (B b) () >>=? fun op ->
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
    (List.length (List.concat (List.map (fun { Alpha_services.Delegate.Endorsing_rights.slots ; _ } -> slots) endorsers))) endorsers_per_block >>=? fun () ->

  fold_left_s (fun (delegates, ops, balances) (endorser : Alpha_services.Delegate.Endorsing_rights.t) ->
      let delegate = endorser.delegate in
      Context.Contract.balance (B b) (Contract.implicit_contract delegate) >>=? fun balance ->
      Op.endorsement ~delegate:(delegate, List.hd endorser.slots) (B b) () >>=? fun op ->
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

  Op.endorsement ~delegate:(endorser.delegate, (List.hd endorser.slots)) (B b) () >>=? fun operation ->
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
      Op.endorsement ~delegate:(endorser.delegate, List.hd endorser.slots) (B b) () >>=? fun operation ->
      let operation = Operation.pack operation in
      Block.bake ~policy:( Excluding [ endorser.delegate ] ) ~operation b >>=? fun b ->

      assert_endorser_balance_consistency ~loc:__LOC__ ~priority
        (B b) ~nb_endorsement:(List.length endorser.slots) endorser.delegate balance
    ) priorities

(** Check that after [preserved_cycles] cycles the endorser gets his reward *)
let reward_retrieval () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun Constants.
    { parametric = { preserved_cycles ; endorsement_reward ; _ } ; _ } ->

  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  Context.Contract.balance (B b) (Contract.implicit_contract endorser) >>=? fun balance ->
  Op.endorsement ~delegate:(endorser, List.hd slots) (B b) () >>=? fun operation ->
  let operation = Operation.pack operation in
  Block.bake ~policy:(Excluding [ endorser ]) ~operation b >>=? fun b ->
  (* Bake (preserved_cycles + 1) cycles *)
  fold_left_s (fun b _ ->
      Block.bake_until_cycle_end ~policy:(Excluding [ endorser ]) b
    ) b (0 -- preserved_cycles) >>=? fun b ->

  Lwt.return Tez.(endorsement_reward *? Int64.of_int (List.length slots)) >>=? fun reward ->
  Assert.balance_was_credited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser) balance reward


(** Check that after [preserved_cycles] cycles endorsers get their
    reward. Two endorsers are used and they endorse in different
    cycles. *)
let reward_retrieval_two_endorsers () =
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun Constants.
    { parametric = { preserved_cycles ; endorsement_reward ; endorsement_security_deposit ; _ } ; _ } ->

  Context.get_endorsers (B b) >>=? fun endorsers ->
  let endorser1 = List.hd endorsers in
  let endorser2 = List.hd (List.tl endorsers) in

  let policy = Block.Excluding [ endorser1.delegate ; endorser2.delegate ] in

  Context.Contract.balance (B b) (Contract.implicit_contract endorser1.delegate) >>=? fun balance1 ->
  Context.Contract.balance (B b) (Contract.implicit_contract endorser2.delegate) >>=? fun balance2 ->

  Lwt.return Tez.(endorsement_security_deposit *? Int64.of_int (List.length endorser1.slots)) >>=? fun security_deposit1 ->
  Lwt.return Tez.(endorsement_reward *? Int64.of_int (List.length endorser1.slots)) >>=? fun reward1 ->

  (* endorser1 endorses the genesis block in cycle 0 *)
  Op.endorsement ~delegate:(endorser1.delegate, List.hd endorser1.slots) (B b) () >>=? fun operation1 ->

  (* bake next block, include endorsement of endorser1 *)
  Block.bake ~policy ~operation:(Operation.pack operation1) b >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 security_deposit1 >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2  >>=? fun () ->

  (* complete cycle 0 *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 security_deposit1 >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2  >>=? fun () ->

  (* get the slots of endorser2 for the current block *)
  Context.get_endorsers (B b) >>=? fun endorsers ->
  let same_endorser2 endorser = Signature.Public_key_hash.compare endorser.Delegate_services.Endorsing_rights.delegate endorser2.delegate = 0 in
  let endorser2 = List.find same_endorser2 endorsers in (* No exception raised: in sandboxed mode endorsers do not change between blocks *)
  Lwt.return Tez.(endorsement_security_deposit *? Int64.of_int (List.length endorser2.slots)) >>=? fun security_deposit2 ->

  (* endorser2 endorses the last block in cycle 0 *)
  Op.endorsement ~delegate:(endorser2.delegate, List.hd endorser2.slots) (B b) () >>=? fun operation2 ->
  let priority = b.header.protocol_data.contents.priority in
  Tez.(endorsement_reward /? Int64.(succ (of_int priority))) >>?= fun reward_per_slot ->
  Lwt.return Tez.(reward_per_slot *? Int64.of_int (List.length endorser2.slots)) >>=? fun reward2 ->

  (* bake first block in cycle 1, include endorsement of endorser2 *)
  Block.bake ~policy ~operation:(Operation.pack operation2) b >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 security_deposit1 >>=? fun () ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2 security_deposit2 >>=? fun () ->

  (* bake [preserved_cycles] cycles *)
  fold_left_s (fun b _ ->
      Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 security_deposit1 >>=? fun () ->
      Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2 security_deposit2 >>=? fun () ->
      Block.bake_until_cycle_end ~policy b
    ) b (1 -- preserved_cycles) >>=? fun b ->

  Assert.balance_was_credited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 reward1 >>=? fun () ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2 security_deposit2 >>=? fun () ->

  (* bake cycle [preserved_cycle + 1] *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Assert.balance_was_credited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser1.delegate) balance1 reward1 >>=? fun () ->
  Assert.balance_was_credited ~loc:__LOC__ (B b) (Contract.implicit_contract endorser2.delegate) balance2 reward2



(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Wrong endorsement predecessor : apply an endorsement with an
    incorrect block predecessor *)
let wrong_endorsement_predecessor () =
  Context.init 5 >>=? fun (b, _) ->

  Context.get_endorser (B b) >>=? fun (genesis_endorser, slots) ->
  Block.bake b >>=? fun b' ->
  Op.endorsement ~delegate:(genesis_endorser, List.hd slots) ~signing_context:(B b) (B b') () >>=? fun operation ->
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
let not_enough_for_deposit () =
  Context.init 5 ~endorsers_per_block:1 >>=? fun (b_init, contracts) ->
  Error_monad.map_s (fun c ->
      Context.Contract.manager (B b_init) c >>=? fun m -> return (m, c)) contracts >>=?
  fun managers ->
  Block.bake b_init >>=? fun b ->
  (* retrieve the level 2's endorser *)
  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  let _, contract_other_than_endorser =
    List.find (fun (c, _) -> not (Signature.Public_key_hash.equal c.Account.pkh endorser))
      managers
  in
  let _, contract_of_endorser =
    List.find (fun (c, _) -> (Signature.Public_key_hash.equal c.Account.pkh endorser))
      managers
  in
  Context.Contract.balance (B b)
    (Contract.implicit_contract endorser) >>=? fun initial_balance ->
  (* Empty the future endorser account *)
  Op.transaction (B b_init) contract_of_endorser contract_other_than_endorser initial_balance >>=? fun op_trans ->
  Block.bake ~operation:op_trans b_init >>=? fun b ->
  (* Endorse with a zero balance *)
  Op.endorsement ~delegate:(endorser, List.hd slots) (B b) () >>=? fun op_endo ->
  Block.bake
    ~policy:(Excluding [endorser])
    ~operation:(Operation.pack op_endo)
    b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Delegate_storage.Balance_too_low_for_deposit _ -> true
    | _ -> false
  end

let decrease_threshold priority () =
  let minimum_endorsements_per_priority = [32; 24; 16; 8; 4; 2; 1] in
  Context.init ~minimum_endorsements_per_priority 32 >>=? fun (b, _) ->
  Context.get_endorsers (B b) >>=? fun endorsers ->
  (* check that any bake at priorities < priority fail because no endorsements *)
  fold_left_s
    (fun b i -> Block.bake ~policy:(By_priority i) b >>= fun b2 ->
      Assert.proto_error ~loc:__LOC__ b2 begin function
        | Apply.Not_enough_endorsements_for_priority _ -> true
        | _ -> false
      end >>=? fun () -> return b)
    b (1 -- priority) >>=? fun b ->
  let needed_endorsements = List.nth minimum_endorsements_per_priority (priority - 1) in
  let delegates_with_slots = get_min_delegates_to_endorse 0 [] endorsers needed_endorsements in
  map_s (fun x ->  Op.endorsement ~delegate:x (B b) ()) delegates_with_slots >>=? fun ops ->
  (* bake at priority succeed thanks to enough endorsements *)
  Block.bake
    ~policy:(By_priority priority)
    ~operations:(List.map Operation.pack ops)
    b >>= fun _ -> return_unit

let test_fitness_gap () =
  let minimum_endorsements_per_priority = [17; 9; 5; 2; 1] in
  let n = List.length minimum_endorsements_per_priority in
  Context.init ~minimum_endorsements_per_priority 32 >>=? fun (b, _) ->
  begin
    match Fitness_repr.to_int64 b.header.shell.fitness with
    | Ok fitness ->
        return (Int64.to_int fitness)
    | Error _ -> assert false
  end >>=? fun fitness ->
  fold_left_s (fun b i ->
      let needed_endorsements = List.nth minimum_endorsements_per_priority i in
      Context.get_endorsers (B b) >>=? fun endorsers ->
      let delegates_with_slots = get_min_delegates_to_endorse 0 [] endorsers needed_endorsements in
      map_s (fun x ->  Op.endorsement ~delegate:x (B b) ()) delegates_with_slots >>=? fun ops ->
      (* bake at priority i succeed thanks to enough endorsements *)
      Block.bake
        ~policy:(By_priority i)
        ~operations:(List.map Operation.pack ops)
        b >>=? fun b ->
      begin
        match Fitness_repr.to_int64 b.header.shell.fitness with
        | Ok new_fitness ->
            return ((Int64.to_int new_fitness) - fitness)
        | Error _ -> assert false
      end >>=? fun res ->
      (* in EmmyB, fitness increases by 1, so the difference between
         fitness at level i and at level 0 is i+1 *)
      Assert.equal_int ~loc:__LOC__ res (i+1) >>=? fun () ->
      return b
    ) b (0 -- (n - 1)) >>=? fun _ -> return_unit

let tests = [
  Test.tztest "Simple endorsement" `Quick simple_endorsement ;
  Test.tztest "Maximum endorsement" `Quick max_endorsement ;
  Test.tztest "Consistent priority" `Quick consistent_priority ;
  Test.tztest "Consistent priorities" `Quick consistent_priorities ;
  Test.tztest "Reward retrieval" `Quick reward_retrieval ;
  Test.tztest "Reward retrieval two endorsers" `Quick reward_retrieval_two_endorsers ;
  Test.tztest "Decrease threshold" `Quick (decrease_threshold 5) ;
  Test.tztest "fitness_gap" `Quick test_fitness_gap ;

  (* Fail scenarios *)
  Test.tztest "Wrong endorsement predecessor" `Quick wrong_endorsement_predecessor ;
  Test.tztest "Invalid endorsement level" `Quick invalid_endorsement_level ;
  Test.tztest "Duplicate endorsement" `Quick duplicate_endorsement ;
  Test.tztest "Not enough for deposit" `Quick not_enough_for_deposit ;
]
