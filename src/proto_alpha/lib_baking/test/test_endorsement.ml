(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context
module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let { Helpers.Account.b1 ; b2 ; b3 ; b4 ; b5 } =
  Helpers.Account.bootstrap_accounts

let default_account =
  Helpers.Account.create "default_account"

let test_double_endorsement_evidence contract block =

  (* Double endorsement for the same level *)
  Helpers.Baking.bake block contract [] >>=? fun b1 ->

  (* branch root *)
  Helpers.Baking.bake (`Hash (b1, 0)) contract [] >>=? fun b2 ->
  (* changing branch *)
  Helpers.Baking.bake (`Hash (b1, 0)) contract [] >>=? fun b2' ->

  (* branch root *)
  Helpers.Endorse.endorse contract (`Hash (b2, 0)) >>=? fun op ->
  Helpers.Baking.bake (`Hash (b2, 0)) contract [ op ] >>=? fun _b3 ->

  Helpers.Endorse.endorse contract (`Hash (b2', 0)) >>=? fun op ->
  Helpers.Baking.bake (`Hash (b2', 0)) contract [ op ] >>=? fun b3' ->

  Helpers.Endorse.endorse contract (`Hash (b3', 0)) >>=? fun op ->
  Helpers.Baking.bake (`Hash (b3', 0)) contract [ op ] >>=? fun b4' ->

  (* TODO: Inject double endorsement op ! *)
  Helpers.Baking.bake (`Hash (b4', 0)) contract []

(* FIXME: Baking.Invalid_signature is unclassified *)
let test_invalid_signature block =
  let public_key =
    Ed25519.Public_key.of_b58check_exn
      "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n" in
  let secret_key =
    Ed25519.Secret_key.of_b58check_exn
      "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh" in
  let account =
    Helpers.Account.create ~keys:(secret_key, public_key) "WRONG SIGNATURE" in
  Helpers.Baking.bake block account [] >>= fun res ->
  Assert.generic_economic_error ~msg:__LOC__ res ;
  return ()

let contain_tzerror ?(msg="") ~f t =
  t >>= function
  | Ok _ -> failwith "%s: Expected error found success" msg
  | Error error when not (List.exists f error) ->
      failwith "@[<v 2>Unexpected error@ %a@]" pp_print_error error
  | _ -> return ()

let test_wrong_delegate ~baker contract block =
  begin
    Helpers.Endorse.endorse ~slot:1 contract block >>=? fun op ->
    Helpers.Baking.bake block baker [ op ] >>=? fun _ ->
    Helpers.Endorse.endorse ~slot:2 contract block >>=? fun op ->
    Helpers.Baking.bake block baker [ op ] >>=? fun _ ->
    Helpers.Endorse.endorse ~slot:3 contract block >>=? fun op ->
    Helpers.Baking.bake block baker [ op ] >>=? fun _ ->
    Helpers.Endorse.endorse ~slot:4 contract block >>=? fun op ->
    Helpers.Baking.bake block baker [ op ] >>=? fun _ ->
    Helpers.Endorse.endorse ~slot:5 contract block >>=? fun op ->
    Helpers.Baking.bake block baker [ op ] >>=? fun _ ->
    return ()
  end >>= fun res ->
  Assert.failed_to_preapply ~msg:__LOC__ begin function
    | Alpha_context.Operation.Invalid_signature -> true
    | _ -> false
  end res ;
  Lwt.return_unit

let test_invalid_endorsement_slot contract block =
  Helpers.Endorse.endorse ~slot:~-1 contract block >>=? fun op ->
  Helpers.Baking.bake block contract [ op ] >>= fun res ->
  Assert.failed_to_preapply ~msg:__LOC__ ~op begin function
    | Baking.Invalid_endorsement_slot _ -> true
    | _ -> false
  end res ;
  Helpers.Endorse.endorse ~slot:16 contract block >>=? fun op ->
  Helpers.Baking.bake block contract [ op ] >>= fun res ->
  Assert.failed_to_preapply ~msg:__LOC__ ~op begin function
    | Operation.Invalid_signature -> true
    | _ -> false
  end res ;
  return ()

let test_endorsement_rewards block0 =

  let get_endorser_except bs accounts =
    let account, cpt = ref accounts.(0), ref 0 in
    while List.mem !account bs do
      incr cpt ;
      account := accounts.(!cpt)
    done ;
    return (!account, !cpt) in

  let deposit = Tez.to_mutez Constants.endorsement_security_deposit in

  (* Endorsement Rights *)
  (* #1 endorse & inject in a block *)
  Helpers.Endorse.endorsers_list block0 >>=? fun accounts ->
  get_endorser_except [ b1 ] accounts >>=? fun (account0, slot0) ->
  Helpers.Account.balance ~block:block0 account0 >>=? fun balance0 ->
  Helpers.Endorse.endorse ~slot:slot0 account0 block0 >>=? fun op ->
  Helpers.Baking.bake block0 b1 [ op ] >>=? fun hash1 ->
  Helpers.display_level (`Hash (hash1, 0)) >>=? fun () ->
  Assert.balance_equal ~block:(`Hash (hash1, 0)) ~msg:__LOC__ account0
    (Int64.sub (Tez.to_mutez balance0) deposit) >>=? fun () ->

  (* #2 endorse & inject in a block  *)
  let block1 = `Hash (hash1, 0) in
  Helpers.Endorse.endorsers_list block1 >>=? fun accounts ->
  get_endorser_except [ b1 ; account0 ] accounts >>=? fun (account1, slot1) ->
  Helpers.Account.balance ~block:block1 account1 >>=? fun balance1 ->
  Helpers.Endorse.endorse ~slot:slot1 account1 block1 >>=? fun op ->
  Helpers.Baking.bake block1 b1 [ op ] >>=? fun hash2 ->
  Helpers.display_level (`Hash (hash2, 0)) >>=? fun () ->
  Assert.balance_equal ~block:(`Hash (hash2, 0)) ~msg:__LOC__ account1
    (Int64.sub (Tez.to_mutez balance1) deposit) >>=? fun () ->

  (*
  (* Check rewards after one cycle for account0 *)
  Helpers.Baking.bake (`Hash (hash2, 0)) b1 [] >>=? fun hash3 ->
  Helpers.display_level (`Hash (hash3, 0)) >>=? fun () ->
  Helpers.Baking.bake (`Hash (hash3, 0)) b1 [] >>=? fun hash4 ->
  Helpers.display_level (`Hash (hash4, 0)) >>=? fun () ->
  Helpers.Baking.bake (`Hash (hash4, 0)) b1 [] >>=? fun hash5 ->
  Helpers.display_level (`Hash (hash5, 0)) >>=? fun () ->
  Helpers.Baking.endorsement_reward block1 >>=? fun rw0 ->
  Assert.balance_equal ~block:(`Hash (hash5, 0)) ~msg:__LOC__ account0
    (Int64.add (Tez.to_mutez balance0) rw0) >>=? fun () ->

  (* Check rewards after one cycle for account1 *)
  Helpers.Baking.endorsement_reward (`Hash (hash2, 0)) >>=? fun rw1 ->
  Assert.balance_equal ~block:(`Hash (hash5, 0)) ~msg:__LOC__ account1
    (Int64.add (Tez.to_mutez balance1) rw1) >>=? fun () ->

  (* #2 endorse and check reward only on the good chain  *)
  Helpers.Baking.bake (`Hash (hash5, 0)) b1 []>>=? fun hash6a ->
  Helpers.display_level (`Hash (hash6a, 0)) >>=? fun () ->
  Helpers.Baking.bake (`Hash (hash5, 0)) b1 [] >>=? fun hash6b ->
  Helpers.display_level (`Hash (hash6b, 0)) >>=? fun () ->

  (* working on head *)
  Helpers.Endorse.endorsers_list (`Hash (hash6a, 0)) >>=? fun accounts ->
  get_endorser_except [ b1 ] accounts >>=? fun (account3, slot3) ->
  Helpers.Account.balance ~block:(`Hash (hash6a, 0)) account3 >>=? fun balance3 ->
  Helpers.Endorse.endorse
    ~slot:slot3 account3 (`Hash (hash6a, 0)) >>=? fun ops ->
  Helpers.Baking.bake (`Hash (hash6a, 0)) b1 [ ops ] >>=? fun hash7a ->
  Helpers.display_level (`Hash (hash7a, 0)) >>=? fun () ->

  (* working on fork *)
  Helpers.Endorse.endorsers_list (`Hash (hash6b, 0)) >>=? fun accounts ->
  get_endorser_except [ b1 ] accounts >>=? fun (account4, slot4) ->
  Helpers.Account.balance ~block:(`Hash (hash7a, 0)) account4 >>=? fun _balance4 ->
  Helpers.Endorse.endorse ~slot:slot4 account4 (`Hash (hash6b, 0)) >>=? fun ops ->
  Helpers.Baking.bake (`Hash (hash6b, 0)) b1 [ ops ] >>=? fun _new_fork ->
  Helpers.display_level (`Hash (_new_fork, 0)) >>=? fun () ->
  Helpers.Account.balance ~block:(`Hash (hash7a, 0)) account4 >>=? fun balance4 ->

  Helpers.Baking.bake (`Hash (hash7a, 0)) b1 [] >>=? fun hash8a ->
  Helpers.display_level (`Hash (hash8a, 0)) >>=? fun () ->
  Helpers.Baking.bake (`Hash (hash8a, 0)) b1 [] >>=? fun hash9a ->
  Helpers.display_level (`Hash (hash9a, 0)) >>=? fun () ->

  (* Check rewards after one cycle *)
  Helpers.Baking.endorsement_reward (`Hash (hash7a, 0)) >>=? fun reward ->
  Assert.balance_equal ~block:(`Hash (hash9a, 0)) ~msg:__LOC__ account3
    (Int64.add (Tez.to_mutez balance3) reward) >>=? fun () ->

  (* Check no reward for the fork *)
  begin
    if account3 = account4 then return ()
    (* if account4 is different from account3, we need to check that there
       is no reward for him since the endorsement was in the fork branch *)
    else Assert.balance_equal ~block:(`Hash (hash9a, 0)) ~msg:__LOC__ account4 (Tez.to_mutez balance4)
  end >>=? fun () ->

*)
  return ()

let test_endorsement_rights contract block =
  Helpers.Endorse.endorsement_rights contract block >>|? fun possibilities ->
  possibilities <> []

let run genesis =

  Helpers.Baking.bake genesis b1 [] >>=? fun blk ->

  let block = `Hash (blk, 0) in
  test_endorsement_rights
    default_account block >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse false ;
  test_endorsement_rights b1 block >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse true ;
  test_endorsement_rights b1 block >>=? fun has_right_to_endorse ->
  Assert.equal_bool ~msg:__LOC__ has_right_to_endorse true ;

  Assert.balance_equal
    ~block:block ~msg:__LOC__ b1 3_999_488_000_000L >>=? fun () ->
  Assert.balance_equal
    ~block:block ~msg:__LOC__ b2 4_000_000_000_000L >>=? fun () ->
  Assert.balance_equal
    ~block:block ~msg:__LOC__ b3 4_000_000_000_000L >>=? fun () ->
  Assert.balance_equal
    ~block:block ~msg:__LOC__ b4 4_000_000_000_000L >>=? fun () ->
  Assert.balance_equal
    ~block:block ~msg:__LOC__ b5 4_000_000_000_000L >>=? fun () ->

  (* Check Rewards *)
  test_endorsement_rewards block >>=? fun () ->

  (* Endorse with a contract with wrong delegate:
      - contract with no endorsement rights
      - contract which signs at every available slots *)
  test_wrong_delegate ~baker:b1 default_account block >>= fun () ->
  test_wrong_delegate ~baker:b1 b5 block >>= fun () ->

  (* Endorse with a wrong slot : -1 and max (16) *)
  test_invalid_endorsement_slot b3 block >>=? fun () ->

  (* FIXME: Baking.Invalid_signature is still unclassified *)
  test_invalid_signature block >>=? fun _ ->

  (* FIXME: cannot inject double endorsement operation yet, but the
     code is still here
     Double endorsement *)
  test_double_endorsement_evidence b4 block >>=? fun _ ->

  return ()

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let sandbox = try Sys.argv.(2) with _ -> "sandbox.json"
let rpc_port = try int_of_string Sys.argv.(3) with _ -> 18100

let main () =
  Helpers.init ~exe ~sandbox ~rpc_port () >>=? fun (_node_pid, genesis) ->
  run (`Hash (genesis, 0))


let tests = [
  "main", (fun _ -> main ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-client-alpha" [
    "endorsement", List.map wrap tests
  ]
