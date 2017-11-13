(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  Baking.mine block b1 [] >>=? fun head ->
  Format.eprintf "Entering `Proposal` voting period@.";
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Proposal >>=? fun () ->
  Baking.mine (`Hash head) b2 [] >>=? fun head ->

  (* 1. Propose the 'demo' protocol as b1 (during the Proposal period) *)
  Protocol.proposals
    ~block:(`Hash head)
    ~src:b1
    [demo_protocol] >>=? fun op ->

  (* Mine blocks to switch to next vote period (Testing_vote) *)
  Baking.mine (`Hash head) b3 [op] >>=? fun head ->
  Format.eprintf "Entering `Testing_vote` voting period@.";
  Baking.mine (`Hash head) b4 [] >>=? fun head ->
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
  Baking.mine (`Hash head) b5 operations >>=? fun head ->
  Format.eprintf "Entering `Testing` voting period@.";
  Baking.mine (`Hash head) b1 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Testing >>=? fun () ->

  (* 3. Test the proposed protocol *)

  (* Mine blocks to switch to next vote period (Promote_vote) *)
  Baking.mine (`Hash head) b2 [] >>=? fun head ->
  Format.eprintf "Entering `Promote_vote` voting period@.";
  Baking.mine (`Hash head) b3 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash head)
    Voting_period.Promotion_vote >>=? fun () ->

  (* 4. Vote unanimously for promoting the protocol *)
  map_s (fun src -> vote_for_demo ~src ~block:(`Hash head) Vote.Yay)
    all_accounts >>=? fun operations ->

  (* Mine blocks to switch to end the vote cycle (back to Proposal) *)
  Format.eprintf "Switching to `demo` protocol@.";
  Baking.mine (`Hash head) b4 operations >>=? fun head ->
  Baking.mine (`Hash head) b5 [] >>=? fun head ->

  Assert.check_protocol
    ~msg:__LOC__ ~block:(`Hash head) demo_protocol >>=? fun () ->

  return (`Hash head)

let change_to_demo_proto () =
  init ~sandbox:"sandbox-vote.json" ~rpc_port:18400 () >>=? fun (_node_pid, hash) ->
  run_change_to_demo_proto (`Hash hash) Account.bootstrap_accounts >>=? fun _blkh ->
  return ()

let tests = [
  "change_to_demo_proto", (fun _ -> change_to_demo_proto ()) ;
]

let () =
  Test.run "amendment." tests
