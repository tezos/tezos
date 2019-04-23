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
open Alpha_context
open Proto_alpha_helpers

let demo_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let print_level head =
  level (`Main, `Hash (head, 0)) >>=? fun lvl ->
  return @@ Format.eprintf "voting_period = %a.%ld@."
    Voting_period.pp lvl.voting_period lvl.voting_period_position

let run_change_to_demo_proto block
    ({ b1 ; b2 ; b3 ; b4 ; b5 } : Account.bootstrap_accounts) =
  Baking.bake block b1 [] >>=? fun head ->
  Format.eprintf "Entering `Proposal` voting period@.";
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash (head, 0))
    Voting_period.Proposal >>=? fun () ->
  Baking.bake (`Hash (head, 0)) b2 [] >>=? fun head ->

  (* 1. Propose the 'demo' protocol as b1 (during the Proposal period) *)
  Protocol.proposals
    ~block:(`Hash (head, 0))
    ~src:b1
    [demo_protocol] >>=? fun op ->

  (* Mine blocks to switch to next vote period (Testing_vote) *)
  Baking.bake (`Hash (head, 0)) b3 [op] >>=? fun head ->
  Format.eprintf "Entering `Testing_vote` voting period@.";
  Baking.bake (`Hash (head, 0)) b4 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash (head, 0))
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

  map_s (fun src -> vote_for_demo ~src ~block:(`Hash (head, 0)) Vote.Yay)
    all_accounts >>=? fun operations ->

  (* Mine blocks to switch to next vote period (Testing) *)
  Baking.bake (`Hash (head, 0)) b5 operations >>=? fun head ->
  Format.eprintf "Entering `Testing` voting period@.";
  Baking.bake (`Hash (head, 0)) b1 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash (head, 0))
    Voting_period.Testing >>=? fun () ->

  (* 3. Test the proposed protocol *)

  (* Mine blocks to switch to next vote period (Promote_vote) *)
  Baking.bake (`Hash (head, 0)) b2 [] >>=? fun head ->
  Format.eprintf "Entering `Promote_vote` voting period@.";
  Baking.bake (`Hash (head, 0)) b3 [] >>=? fun head ->
  Assert.check_voting_period_kind ~msg:__LOC__ ~block:(`Hash (head, 0))
    Voting_period.Promotion_vote >>=? fun () ->

  (* 4. Vote unanimously for promoting the protocol *)
  map_s (fun src -> vote_for_demo ~src ~block:(`Hash (head, 0)) Vote.Yay)
    all_accounts >>=? fun operations ->

  (* Mine blocks to switch to end the vote cycle (back to Proposal) *)
  Format.eprintf "Switching to `demo` protocol@.";
  Baking.bake (`Hash (head, 0)) b4 operations >>=? fun head ->

  Assert.check_protocol
    ~msg:__LOC__ ~block:(`Hash (head, 0)) demo_protocol >>=? fun () ->

  return (`Hash (head, 0))

let exe = try Sys.argv.(1) with _ -> "tezos-node"
let rpc_port = try int_of_string Sys.argv.(2) with _ -> 18400

let change_to_demo_proto () =
  init ~exe ~vote:true ~rpc_port () >>=? fun (_node_pid, hash) ->
  run_change_to_demo_proto (`Hash (hash, 0)) Account.bootstrap_accounts >>=? fun _blkh ->
  return_unit

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
