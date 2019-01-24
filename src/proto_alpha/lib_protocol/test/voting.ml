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

(* missing stuff in Alpha_context.Vote *)
let ballots_zero = Alpha_context.Vote.{ yay = 0l ; nay = 0l ; pass = 0l }
let ballots_equal b1 b2 =
  Alpha_context.Vote.(b1.yay = b2.yay && b1.nay = b2.nay && b1.pass = b2.pass)
let ballots_pp ppf v = Alpha_context.Vote.(
    Format.fprintf ppf "{ yay = %ld ; nay = %ld ; pass = %ld" v.yay v.nay v.pass)

let protos = Array.map (fun s -> Protocol_hash.of_b58check_exn s)
    [| "ProtoALphaALphaALphaALphaALphaALphaALpha61322gcLUGH" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphabc2a7ebx6WB" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha84efbeiF6cm" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha91249Z65tWS" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha537f5h25LnN" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha5c8fefgDYkr" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha3f31feSSarC" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphabe31ahnkxSC" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphabab3bgRb7zQ" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphaf8d39cctbpk" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha3b981byuYxD" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphaa116bccYowi" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphacce68eHqboj" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha225c7YrWwR7" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha58743cJL6FG" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphac91bcdvmJFR" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha1faaadhV7oW" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha98232gD94QJ" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha9d1d8cijvAh" ;
       "ProtoALphaALphaALphaALphaALphaALphaALphaeec52dKF6Gx" ;
       "ProtoALphaALphaALphaALphaALphaALphaALpha841f2cQqajX" ; |]

let test_voting () =
  Context.init 5 >>=? fun (b,delegates) ->

  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period} } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  (* no ballots in proposal period *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal ~loc:__LOC__ ballots_equal "Unexpected ballots" ballots_pp
    v ballots_zero >>=? fun () ->

  (* no ballots in proposal period *)
  Context.Vote.get_ballot_list (B b) >>=? begin function
    | [] -> return_unit
    | _ -> failwith "%s - Unexpected ballot list" __LOC__
  end >>=? fun () ->

  (* period 1 *)
  Context.Vote.get_voting_period (B b) >>=? fun v ->
  let open Alpha_context in
  Assert.equal ~loc:__LOC__ Voting_period.equal "Unexpected period"
    Voting_period.pp v Voting_period.(succ root)
  >>=? fun () ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* quorum starts at 80% *)
  Context.Vote.get_current_quorum (B b) >>=? fun v ->
  Assert.equal_int ~loc:__LOC__ 8000 (Int32.to_int v) >>=? fun () ->

  (* listings must be populated in proposal period *)
  Context.Vote.get_listings (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty listings" __LOC__
    | _ -> return_unit
  end >>=? fun () ->

  (* no proposals at the beginning of proposal period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  begin if Alpha_environment.Protocol_hash.Map.is_empty ps
    then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  end >>=? fun () ->

  (* no current proposal during proposal period *)
  Context.Vote.get_current_proposal (B b) >>=? begin function
    | None -> return_unit
    | Some _ -> failwith "%s - Unexpected proposal" __LOC__
  end >>=? fun () ->

  let del1 = List.nth delegates 0 in
  let del2 = List.nth delegates 1 in
  let props = List.map (fun i -> protos.(i)) (2--Constants.max_proposals_per_delegate) in
  Op.proposals (B b) del1 (Protocol_hash.zero::props) >>=? fun ops1 ->
  Op.proposals (B b) del2 [Protocol_hash.zero] >>=? fun ops2 ->
  Block.bake ~operations:[ops1;ops2] b >>=? fun b ->

  (* proposals are now populated *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->

  (* compute the rolls of each delegate *)
  map_s (fun delegate ->
      Context.Contract.pkh delegate >>=? fun pkh ->
      Context.Vote.get_listings (B b) >>=? fun l ->
      match List.find_opt (fun (del,_) -> del = pkh) l with
      | None -> failwith "%s - Missing delegate" __LOC__
      | Some (_, rolls) -> return rolls
    ) delegates >>=? fun rolls ->

  (* correctly count the double proposal for zero *)
  begin
    let weight = Int32.add (List.nth rolls 0) (List.nth rolls 1) in
    match Alpha_environment.Protocol_hash.(Map.find_opt zero ps) with
    | Some v -> if v = weight then return_unit
        else failwith "%s - Wrong count %ld is not %ld" __LOC__ v weight
    | None -> failwith "%s - Missing proposal" __LOC__
  end >>=? fun () ->

  (* proposing more than maximum_proposals fails *)
  Op.proposals (B b) del1 (Protocol_hash.zero::props) >>=? fun ops ->
  Block.bake ~operations:[ops] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Amendment.Too_many_proposals -> true
    | _ -> false
  end >>=? fun () ->

  (* proposing less than one proposal fails *)
  Op.proposals (B b) del1 [] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Amendment.Empty_proposal -> true
    | _ -> false
  end >>=? fun () ->

  (* skip to vote_testing period
     -1 because we already baked one block with the proposal *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-2) b >>=? fun b ->

  (* we moved to a testing_vote period with one proposal *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* period 2 *)
  Context.Vote.get_voting_period (B b) >>=? fun v ->
  let open Alpha_context in
  Assert.equal ~loc:__LOC__ Voting_period.equal "Unexpected period"
    Voting_period.pp v Voting_period.(succ (succ root))
  >>=? fun () ->

  (* listings must be populated in testing_vote period *)
  Context.Vote.get_listings (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty listings" __LOC__
    | _ -> return_unit
  end >>=? fun () ->

  (* no proposals during testing_vote period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  begin if Alpha_environment.Protocol_hash.Map.is_empty ps
    then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  end >>=? fun () ->

  (* current proposal must be set during testing_vote period *)
  Context.Vote.get_current_proposal (B b) >>=? begin function
    | Some v -> if Protocol_hash.(equal zero v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  end >>=? fun () ->

  (* unanimous vote *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  (* voting twice for the same proposal is not allowed *)
  Op.ballot (B b) del1 Protocol_hash.zero Vote.Nay >>=? fun op ->
  Block.bake ~operations:[op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Amendment.Unauthorized_ballot -> true
    | _ -> false
  end >>=? fun () ->

  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l rolls >>=? fun rolls_sum ->

  (* # of Yays in ballots matches rolls of the delegate *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal ~loc:__LOC__ ballots_equal "Unexpected ballots" ballots_pp
    v Vote.{ yay = rolls_sum ; nay = 0l ; pass = 0l } >>=? fun () ->

  (* One Yay ballot per delegate *)
  Context.Vote.get_ballot_list (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
    | l ->
        iter_s (fun delegate ->
            Context.Contract.pkh delegate >>=? fun pkh ->
            match List.find_opt (fun (del,_) -> del = pkh) l with
            | None -> failwith "%s - Missing delegate" __LOC__
            | Some (_, Vote.Yay) -> return_unit
            | Some _ -> failwith "%s - Wrong ballot" __LOC__
          ) delegates
  end >>=? fun () ->


  (* skip to testing period
     -1 because we already baked one block with the ballot *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* period 3 *)
  Context.Vote.get_voting_period (B b) >>=? fun v ->
  let open Alpha_context in
  Assert.equal ~loc:__LOC__ Voting_period.equal "Unexpected period"
    Voting_period.pp v Voting_period.(succ (succ (succ root)))
  >>=? fun () ->

  (* no ballots in testing period *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal ~loc:__LOC__ ballots_equal "Unexpected ballots" ballots_pp
    v ballots_zero >>=? fun () ->

  (* listings must be empty in testing period *)
  Context.Vote.get_listings (B b) >>=? begin function
    | [] -> return_unit
    | _ -> failwith "%s - Unexpected listings" __LOC__
  end >>=? fun () ->


  (* skip to promotion_vote period *)
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Promotion_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* period 4 *)
  Context.Vote.get_voting_period (B b) >>=? fun v ->
  let open Alpha_context in
  Assert.equal ~loc:__LOC__ Voting_period.equal "Unexpected period"
    Voting_period.pp v Voting_period.(succ (succ (succ (succ root))))
  >>=? fun () ->

  (* listings must be populated in promotion_vote period *)
  Context.Vote.get_listings (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty listings" __LOC__
    | _ -> return_unit
  end >>=? fun () ->

  (* no proposals during promotion_vote period *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->
  begin if Alpha_environment.Protocol_hash.Map.is_empty ps
    then return_unit
    else failwith "%s - Unexpected proposals" __LOC__
  end >>=? fun () ->

  (* current proposal must be set during promotion_vote period *)
  Context.Vote.get_current_proposal (B b) >>=? begin function
    | Some v -> if Protocol_hash.(equal zero v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  end >>=? fun () ->

  (* unanimous vote *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  fold_left_s (fun acc delegate ->
      Context.Contract.pkh delegate >>=? fun pkh ->
      Context.Vote.get_listings (B b) >>=? fun l ->
      match List.find_opt (fun (del,_) -> del = pkh) l with
      | None -> failwith "%s - Missing delegate" __LOC__
      | Some (_, rolls) -> return (Int32.add acc rolls)
    ) 0l delegates >>=? fun rolls ->

  (* # of Yays in ballots matches rolls of the delegate *)
  Context.Vote.get_ballots (B b) >>=? fun v ->
  Assert.equal ~loc:__LOC__ ballots_equal "Unexpected ballots" ballots_pp
    v Vote.{ yay = rolls ; nay = 0l ; pass = 0l } >>=? fun () ->

  (* One Yay ballot per delegate *)
  Context.Vote.get_ballot_list (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty ballot list" __LOC__
    | l ->
        iter_s (fun delegate ->
            Context.Contract.pkh delegate >>=? fun pkh ->
            match List.find_opt (fun (del,_) -> del = pkh) l with
            | None -> failwith "%s - Missing delegate" __LOC__
            | Some (_, Vote.Yay) -> return_unit
            | Some _ -> failwith "%s - Wrong ballot" __LOC__
          ) delegates
  end >>=? fun () ->

  (* skip to end of promotion_vote period and activation*)
  Block.bake_n Int32.((to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* zero is the new protocol (before the vote this value is unset) *)
  Context.Vote.get_protocol b >>= fun p ->
  Assert.equal ~loc:__LOC__ Protocol_hash.equal "Unexpected proposal"
    Protocol_hash.pp p Protocol_hash.zero >>=? fun () ->

  return_unit

let test_period1 () =
  Context.init 10 >>=? fun (b,delegates) ->

  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period} } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  let del1 = List.nth delegates 0 in
  let del2 = List.nth delegates 1 in

  Op.proposals (B b) del1 [protos.(0)] >>=? fun ops1 ->
  Op.proposals (B b) del2 [protos.(1)] >>=? fun ops2 ->
  Block.bake ~operations:[ops1;ops2] b >>=? fun b ->
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* we remain in the proposal when there is no winner *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit


let test_period2_supermajority supermajority () =
  Context.init 100 >>=? fun (b,delegates) ->

  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period} } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  let del1 = List.nth delegates 0 in
  let proposal = protos.(0) in

  Op.proposals (B b) del1 [proposal] >>=? fun ops1 ->
  Block.bake ~operations:[ops1] b >>=? fun b ->
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* we remain in the proposal when there is no winner *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* assert our proposal won *)
  Context.Vote.get_current_proposal (B b) >>=? begin function
    | Some v -> if Protocol_hash.(equal proposal v) then return_unit
        else failwith "%s - Wrong proposal" __LOC__
    | None -> failwith "%s - Missing proposal" __LOC__
  end >>=? fun () ->

  (* majority/minority vote depending on the [ok] parameter *)
  filter_s
    (fun del ->
       Context.Contract.pkh del >>=? fun pkh ->
       Context.Delegate.info (B b) pkh >>=? fun {deactivated} -> return (not deactivated))
    delegates >>=? fun active_delegates ->

  let num_delegates = List.length active_delegates in
  let num_nays = num_delegates / 5 in
  let num_yays = num_nays * 4 in
  let num_yays = if supermajority then num_yays else num_yays - 1 in

  let open Alpha_context in

  let nays_delegates, rest = List.split_n num_nays active_delegates in
  let yays_delegates, _ = List.split_n num_yays rest in
  map_s (fun del ->
      Op.ballot (B b) del proposal Vote.Yay)
    yays_delegates >>=? fun operations_yays ->
  map_s (fun del ->
      Op.ballot (B b) del proposal Vote.Nay)
    nays_delegates >>=? fun operations_nays ->
  let operations = operations_yays @ operations_nays in

  Block.bake ~operations b >>=? fun b ->
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing ->
        if supermajority then return_unit
        else failwith "%s - Expected period kind Proposal, obtained Testing" __LOC__
    | Proposal ->
        if not supermajority then return_unit
        else failwith "%s - Expected period kind Proposal, obtained Testing_vote" __LOC__
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit


let tests = [
  Test.tztest "voting" `Quick (test_voting) ;
  Test.tztest "voting: test period 1" `Quick (test_period1) ;
  Test.tztest "voting: test period 2, with supermajority" `Quick (test_period2_supermajority true) ;
  Test.tztest "voting: test period 2, without supermajority" `Quick (test_period2_supermajority false) ;
]
