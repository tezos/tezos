(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
open Error_monad

let name = "Isolate Endorsement"
module Logger = Logging.Make(struct let name = name end)

exception No_error

open Isolate_helpers
open Shorthands

let (>>?=) = Assert.(>>?=)

let test_wrong_slot endorse_a starting_block =
  let wrong_slot = function
    | Proto_alpha.Baking.Invalid_endorsement_slot _ -> true
    | _ -> false
  in
  endorse_a (-1) starting_block >>?= fun result ->
  Assert.economic_error ~msg: __LOC__ wrong_slot result ;
  endorse_a 16 starting_block >>?= fun result ->
  Assert.economic_error ~msg: __LOC__ wrong_slot result ;
  return ()


let test_wrong_delegate endorse_a starting_block =
  let wrong_delegate = function
    | Proto_alpha.Baking.Wrong_delegate _ -> true
    | _ -> false
  in
  endorse_a 0 starting_block >>=? endorse_a 1 >>=? endorse_a 2 >>= Assert.wrap >>= fun result ->
  Assert.economic_error ~msg: __LOC__ wrong_delegate result ;
  return ()


let test_endorsement_payment () =
  Init.main () >>=? fun root ->
  let bootstrap_accounts = Account.bootstrap_accounts in
  let open Proto_alpha.Alpha_context in
  get_tc_full root >>=? fun tc ->
  let level = Level.succ tc @@ Level.current tc in
  Proto_alpha.Services_registration.endorsement_rights tc level None >>=? fun (_, endorsers) ->

  let aux (endorser_slot, block_priority) =
    let contract_p =
      Misc.find_account bootstrap_accounts
      @@ List.nth endorsers endorser_slot in
    Contract.get_balance tc (Contract.default_contract contract_p.hpub) >>=? fun init_balance ->

    (* After one block, endorsement bond cost should be paid *)
    Block.endorsement
      root.tezos_header.shell root.hash
      root.level block_priority contract_p
      root.validation.context endorser_slot
    >>=? fun result ->
    get_balance_res contract_p result >>=? fun bond_balance ->
    let proto_header = Block.get_proto_header block_priority in
    Proto_alpha.Baking.check_baking_rights
      result.tezos_context proto_header root.tezos_header.shell.timestamp
    >>=? fun baker_hpub ->
    let endorsement_bond_cost =
      Constants.endorsement_bond_cost in
    let baking = baker_hpub = contract_p.hpub && block_priority < 4 in
    let baking_bond_cost =
      if baking
      then Constants.baking_bond_cost
      else Tez.zero in
    let cost = Cast.tez_add endorsement_bond_cost baking_bond_cost in
    let expected_balance = Cast.tez_sub init_balance cost in
    Assert.equal_tez ~msg: __LOC__ expected_balance bond_balance ;
    (* After one cycle, (4 blocks in test/proto_alpha/sandbox),
       endorsement reward sould be received *)
    chain_empty_block result >>=? chain_empty_block >>=?
    chain_empty_block >>=? chain_empty_block >>=? fun result ->
    get_balance_res contract_p result >>=? fun reward_balance ->
    Proto_alpha.Baking.endorsement_reward ~block_priority >>=? fun reward ->
    let expected_balance = Cast.tez_add expected_balance reward in
    let expected_balance = Cast.tez_add expected_balance endorsement_bond_cost in
    Assert.equal_tez ~msg: __LOC__ expected_balance reward_balance ;
    return ()
  in
  let slots = [0 ;1 ;2 ;3 ;4 ;5 ;6 ;7 ;8 ;9 ;10 ;11 ;12 ;13 ;14] in
  let prios = [0 ;1 ;2 ;3 ;4 ;5 ;6] in
  iter_s aux @@ List.product slots prios


let test_multiple_endorsement () =
  Init.main () >>=? fun pred ->
  let tc = pred.tezos_context in
  let level = Level.current tc in
  Proto_alpha.Services_registration.endorsement_rights tc level None >>=? fun (_, endorsers) ->
  let endorser =
    Misc.find_account Account.bootstrap_accounts
    @@ List.nth endorsers 0 in
  let op = Isolate_helpers.Operation.endorsement_full endorser pred.hash, endorser in
  Block.of_res ~res: pred ~ops: [op ;op] () >>= Assert.wrap >>= fun x ->
  Assert.double_endorsement ~msg: __LOC__ x ;
  return ()


let test_wrong_endorsement () =
  Init.main () >>=? fun starting_block ->
  let account = Account.new_account () in
  let endorse slot (res: Block.result) =
    Block.endorsement
      res.tezos_header.shell res.hash res.level
      15 account res.validation.context slot
  in
  test_wrong_delegate endorse starting_block >>=? fun () ->
  test_wrong_slot endorse starting_block


let test_fitness () =
  Init.main () >>=? fun res ->
  Block.of_res ~priority: 0 ~res () >>=? fun block_0 ->
  let fitness_0 = block_0.validation.fitness in
  Block.of_res ~priority: 1 ~res () >>=? fun block_1 ->
  let fitness_1 = block_1.validation.fitness in
  let diff = Fitness.compare fitness_0 fitness_1 in
  Assert.equal_int ~msg: "Fitness test" diff 0 ;
  return ()

let tests =
  List.map
    (fun (n, f) -> (n, (fun () -> f () >>= Assert.wrap)))
    [ "endorsement.payment", test_endorsement_payment ;
      "endorsement.wrong", test_wrong_endorsement ;
      "endorsement.multiple", test_multiple_endorsement ;
      "endorsement.fitness", test_fitness ;
    ]
