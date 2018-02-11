(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context
open Proto_alpha_helpers

let demo_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let print_level head =
  level (`Hash head) >>=? fun lvl ->
  return @@ Format.eprintf "voting_period = %a.%ld@."
    Voting_period.pp lvl.voting_period lvl.voting_period_position

let run_change_to_demo_proto block
    ({ b1 ; b2 ; b3 ; b4 ; b5 } : Account.bootstrap_accounts) =
  Baking.bake block b1 [] >>=? fun head ->
  Format.eprintf "Entering `Proposal` voting period@.";
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Proposal >>=? fun () ->
  Baking.bake (`Hash head) b2 [] >>=? fun head ->

  (* 1. Propose the 'demo' protocol as b1 (during the Proposal period) *)
  Protocol.proposals
    ~block:(`Hash head)
    ~src:b1
    [demo_protocol] >>=? fun op ->

  (* Mine blocks to switch to next vote period (Testing_vote) *)
  Baking.bake (`Hash head) b3 [op] >>=? fun head ->
  Format.eprintf "Entering `Testing_vote` voting period@.";
  Baking.bake (`Hash head) b4 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Testing_vote >>=? fun () ->

  (* 2. Vote unanimously for a proposal *)

  let vote_for_demo ~src ~block ballot =
    Protocol.ballot
      ~block
      ~src
      ~proposal:demo_protocol
      ballot
  in
  let all_accounts = [b1; b2; b3; b4; b5] in

  map_s (fun src -> vote_for_demo ~src ~block:(`Hash head) Vote.Yay)
    all_accounts >>=? fun operations ->

  (* Mine blocks to switch to next vote period (Testing) *)
  Baking.bake (`Hash head) b5 operations >>=? fun head ->
  Format.eprintf "Entering `Testing` voting period@.";
  Baking.bake (`Hash head) b1 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Testing >>=? fun () ->

  (* 3. Test the proposed protocol *)

  (* Mine blocks to switch to next vote period (Promote_vote) *)
  Baking.bake (`Hash head) b2 [] >>=? fun head ->
  Format.eprintf "Entering `Promote_vote` voting period@.";
  Baking.bake (`Hash head) b3 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Promotion_vote >>=? fun () ->

  (* 4. Vote unanimously for promoting the protocol *)
  map_s (fun src -> vote_for_demo ~src ~block:(`Hash head) Vote.Yay)
    all_accounts >>=? fun operations ->

  (* Mine blocks to switch to end the vote cycle (back to Proposal) *)
  Format.eprintf "Switching to `demo` protocol@.";
  Baking.bake (`Hash head) b4 operations >>=? fun head ->
  Baking.bake (`Hash head) b5 [] >>=? fun head ->

  Assert.check_protocol
    ~msg:__LOC__ ~block:(`Hash head) demo_protocol >>=? fun () ->

  return (`Hash head)

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let sandbox = try Sys.argv.(2) with _ -> "sandbox-vote.json"
let rpc_port = try int_of_string Sys.argv.(3) with _ -> 18400

let change_to_demo_proto () =
  init ~exe ~sandbox ~rpc_port () >>=? fun (_node_pid, hash) ->
  run_change_to_demo_proto (`Hash hash) Account.bootstrap_accounts >>=? fun _blkh ->
  return ()

let tests = [
  "change_to_demo_proto", (fun _ -> change_to_demo_proto ()) ;
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
    "amendment", List.map wrap tests
  ]
