(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_embedded_proto_alpha
open Tezos_context
open Client_alpha

module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let test_double_endorsement contract block =

  (* Double endorsement for the same level *)
  Helpers.Mining.mine ~fitness_gap:1 contract block >>=? fun b1 ->

  (* branch root *)
  Helpers.Mining.mine ~fitness_gap:1 contract (`Hash b1) >>=? fun b2 ->
  (* changing branch *)
  Helpers.Mining.mine ~fitness_gap:1 contract (`Hash b1) >>=? fun b2' ->

  (* branch root *)
  Helpers.Endorse.endorse ~force:true contract (`Hash b2) >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] contract (`Hash b2) >>=? fun _b3 ->

  Helpers.Endorse.endorse ~force:true contract (`Hash b2') >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] contract (`Hash b2') >>=? fun b3' ->

  Helpers.Endorse.endorse ~force:true contract (`Hash b3') >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] contract (`Hash b3') >>=? fun b4' ->

  (* TODO: Inject double endorsement op ! *)
  Helpers.Mining.mine ~fitness_gap:1 contract (`Hash b4')

(* FIXME: Mining.Invalid_signature is unclassified *)
let test_invalid_signature block =
  let public_key =
    Environment.Ed25519.Public_key.of_b58check_exn
      "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n" in
  let secret_key =
    Environment.Ed25519.Secret_key.of_b58check_exn
      "edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsn\
       DYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi" in
  let account =
    Helpers.Account.create ~keys:(secret_key, public_key) "WRONG SIGNATURE" in
  Helpers.Mining.mine ~fitness_gap:1 account block >>= fun res ->
  Assert.generic_economic_error ~msg:__LOC__ res ;
  return ()

let contain_tzerror ?(msg="") ~f t =
  t >>= function
  | Ok _ -> failwith "%s: Expected error found success" msg
  | Error error when not (List.exists f error) ->
      failwith "@[<v 2>Unexpected error@ %a@]" pp_print_error error
  | _ -> return ()

let test_wrong_delegate contract block =
  contain_tzerror ~msg:__LOC__ ~f:begin Assert.ecoproto_error (function
      | Mining.Wrong_delegate _ -> true
      | _ -> false)
  end
    (Helpers.Endorse.endorse ~slot:1 ~force:true contract block >>=? fun _ ->
     Helpers.Endorse.endorse ~slot:2 ~force:true contract block >>=? fun _ ->
     Helpers.Endorse.endorse ~slot:3 ~force:true contract block >>=? fun _ ->
     Helpers.Endorse.endorse ~slot:4 ~force:true contract block >>=? fun _ ->
     Helpers.Endorse.endorse ~slot:5 ~force:true contract block)

let test_invalid_endorsement_slot contract block =
  Helpers.Endorse.endorse ~slot:~-1 ~force:true contract block >>= fun res ->
  Assert.invalid_endorsement_slot ~msg:__LOC__ res ;
  Helpers.Endorse.endorse ~slot:16 ~force:true contract block >>= fun res ->
  Assert.invalid_endorsement_slot ~msg:__LOC__ res ;
  return ()

let test_endorsement_rewards
    block ({ Helpers.Account.b5 = b1 ; _ } as baccounts) =
  let get_endorser_except_b1 accounts =
    let account, cpt = ref accounts.(0), ref 0 in
    while !account = b1 do
      incr cpt ;
      account := accounts.(!cpt)
    done ;
    return (!account, !cpt) in

  let bond = Tez.to_cents Constants.endorsement_bond_cost in

  (* Endorsement Rights *)
  (* #1 endorse & inject in a block *)
  Helpers.Endorse.endorsers_list block baccounts >>=? fun accounts ->
  get_endorser_except_b1 accounts >>=? fun (account0, slot0) ->
  Helpers.Account.balance account0 >>=? fun balance0 ->
  Helpers.Endorse.endorse ~slot:slot0 ~force:true account0 block >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] b1 block >>=? fun head0 ->
  Helpers.display_level (`Hash head0) >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ account0
    (Int64.sub (Tez.to_cents balance0) bond) >>=? fun () ->


  (* #2 endorse & inject in a block  *)
  let block0 = `Hash head0 in
  Helpers.Endorse.endorsers_list block0 baccounts >>=? fun accounts ->
  get_endorser_except_b1 accounts >>=? fun (account1, slot1) ->
  Helpers.Account.balance account1 >>=? fun balance1 ->
  Helpers.Endorse.endorse ~slot:slot1 ~force:true account1 block0 >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] b1 block0 >>=? fun head1 ->
  Helpers.display_level (`Hash head1) >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ account1
    (Int64.sub (Tez.to_cents balance1) bond) >>=? fun () ->


  (* #3 endorse but the operation is not included in a block, so no reward  *)
  let block1 = `Hash head1 in
  Helpers.Endorse.endorsers_list block1 baccounts >>=? fun accounts ->
  get_endorser_except_b1 accounts >>=? fun (account2, slot2) ->
  Helpers.Account.balance account2 >>=? fun balance2 ->
  Helpers.Endorse.endorse ~slot:slot2 ~force:true account2 block1 >>=? fun _ops ->
  Assert.balance_equal ~msg:__LOC__ account2
    (Int64.sub (Tez.to_cents balance2) bond) >>=? fun () ->

  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head1) >>=? fun head2 ->
  Helpers.display_level (`Hash head2) >>=? fun () ->
  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head2) >>=? fun head3 ->
  Helpers.display_level (`Hash head3) >>=? fun () ->
  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head3) >>=? fun head4 ->
  Helpers.display_level (`Hash head4) >>=? fun () ->

  (* Check rewards after one cycle for account0 *)
  Helpers.Mining.endorsement_reward b1 block0 >>=? fun rw0 ->
  Assert.balance_equal ~msg:__LOC__ account0
    (Int64.add (Tez.to_cents balance0) rw0) >>=? fun () ->

  (* Check rewards after one cycle for account1 *)
  Helpers.Mining.endorsement_reward b1 block1 >>=? fun rw1 ->
  Assert.balance_equal ~msg:__LOC__ account1
    (Int64.add (Tez.to_cents balance1) rw1) >>=? fun () ->

  (* Check no rewards after one cycle for account2 *)
  Assert.balance_equal
    ~msg:__LOC__ account2 (Tez.to_cents balance2) >>=? fun () ->

  (* #2 endorse and check reward only on the good chain  *)
  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head4) >>=? fun head ->
  Helpers.display_level (`Hash head) >>=? fun () ->
  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head4) >>=? fun fork ->
  Helpers.display_level (`Hash fork) >>=? fun () ->

  (* working on head *)
  Helpers.Endorse.endorsers_list (`Hash head) baccounts >>=? fun accounts ->
  get_endorser_except_b1 accounts >>=? fun (account3, slot3) ->
  Helpers.Account.balance account3 >>=? fun balance3 ->
  Helpers.Endorse.endorse
    ~slot:slot3 ~force:true account3 (`Hash head) >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] b1 (`Hash head) >>=? fun new_head ->
  Helpers.display_level (`Hash new_head) >>=? fun () ->

  (* working on fork *)
  Helpers.Endorse.endorsers_list (`Hash fork) baccounts >>=? fun accounts ->
  get_endorser_except_b1 accounts >>=? fun (account4, slot4) ->
  Helpers.Account.balance account4 >>=? fun _balance4 ->
  Helpers.Endorse.endorse ~slot:slot4 ~force:true account4 (`Hash fork) >>=? fun ops ->
  Helpers.Mining.mine ~fitness_gap:2 ~operations:[ ops ] b1 (`Hash fork) >>=? fun _new_fork ->
  Helpers.display_level (`Hash _new_fork) >>=? fun () ->
  Helpers.Account.balance account4 >>=? fun balance4 ->

  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash new_head) >>=? fun head ->
  Helpers.display_level (`Hash head) >>=? fun () ->
  Helpers.Mining.mine ~fitness_gap:1 b1 (`Hash head) >>=? fun head ->
  Helpers.display_level (`Hash head) >>=? fun () ->

  (* Check rewards after one cycle *)
  Helpers.Mining.endorsement_reward b1 (`Hash new_head) >>=? fun reward ->
  Assert.balance_equal ~msg:__LOC__ account3
    (Int64.add (Tez.to_cents balance3) reward) >>=? fun () ->

  (* Check no reward for the fork *)
  begin
    if account3 = account4 then return ()
    (* if account4 is different from account3, we need to check that there
       is no reward for him since the endorsement was in the fork branch *)
    else Assert.balance_equal ~msg:__LOC__ account4 (Tez.to_cents balance4)
  end >>=? fun () -> return head

let test_endorsement_rights contract block =
  Helpers.Endorse.endorsement_rights contract block >>|? fun possibilities ->
  possibilities <> []

let run head (({ b1 ; b2 ; b3 ; b4 ; b5 } : Helpers.Account.bootstrap_accounts) as baccounts) =

  let default_account = Helpers.Account.create "default_account" in

  test_endorsement_rights default_account head >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse false ;
  test_endorsement_rights b1 head >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse true ;
  test_endorsement_rights b1 head >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse true ;

  Assert.balance_equal ~msg:__LOC__ b1 2_000_000_00L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ b2 2_000_000_00L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ b3 2_000_000_00L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ b4 2_000_000_00L >>=? fun () ->
  Assert.balance_equal ~msg:__LOC__ b5 2_000_000_00L >>=? fun () ->

  (* Check Rewards *)
  test_endorsement_rewards head baccounts >>=? fun head ->

  (* Endorse with a contract with wrong delegate:
      - contract with no endorsement rights
      - contract which signs at every available slots *)
  test_wrong_delegate default_account (`Hash head) >>=? fun () ->
  test_wrong_delegate b5 (`Hash head) >>=? fun () ->

  (* Endorse with a wrong slot : -1 and max (16) *)
  test_invalid_endorsement_slot b3 (`Hash head) >>=? fun () ->

  (* FIXME: Mining.Invalid_signature is still unclassified *)
  test_invalid_signature (`Hash head) >>=? fun _ ->

  (* FIXME: cannot inject double endorsement operation yet, but the
     code is still here
     Double endorsement *)
  test_double_endorsement b4 (`Hash head) >>=? fun new_head ->

  return new_head

let main () =
  Helpers.init () >>=? fun (_node_pid, hash) ->
  run (`Hash hash) Helpers.Account.bootstrap_accounts >>=? fun _blkh ->
  return ()


let tests = [
  "main", (fun _ -> main ()) ;
]

let () =
  Test.run "endorsement." tests
