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
open Test_utils

(* missing stuff in Alpha_context.Vote *)
let ballots_zero = Alpha_context.Vote.{ yay = 0l ; nay = 0l ; pass = 0l }
let ballots_equal b1 b2 =
  Alpha_context.Vote.(b1.yay = b2.yay && b1.nay = b2.nay && b1.pass = b2.pass)
let ballots_pp ppf v = Alpha_context.Vote.(
    Format.fprintf ppf "{ yay = %ld ; nay = %ld ; pass = %ld" v.yay v.nay v.pass)

(* constantans and ratios used in voting:
   percent_mul denotes the percent multiplier
   initial_qr is 8000 that is, 8/10 * percent_mul
   the quorum ratio qr_num / den = 8 / 10
   the participation ration pr_num / den = 2 / 10
   note: we use the same denominator for both quorum and participation rate.
   supermajority rate is s_num / s_den = 8 / 10 *)
let percent_mul = 100_00
let initial_qr = 8 * percent_mul / 10
let qr_num = 8
let den = 10
let pr_num = den - qr_num
let s_num = 8
let s_den = 10

(* Protocol_hash.zero is "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i" *)
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

(** helper functions *)
let mk_contracts_from_pkh pkh_list =
  List.map (Alpha_context.Contract.implicit_contract) pkh_list

(* get the list of delegates and the list of their rolls from listings *)
let get_delegates_and_rolls_from_listings b =
  Context.Vote.get_listings (B b) >>=? fun l ->
  return ((mk_contracts_from_pkh (List.map fst l)), List.map snd l)

(* compute the rolls of each delegate *)
let get_rolls b delegates loc =
  map_s (fun delegate ->
      Context.Contract.pkh delegate >>=? fun pkh ->
      Context.Vote.get_listings (B b) >>=? fun l ->
      match List.find_opt (fun (del,_) -> del = pkh) l with
      | None -> failwith "%s - Missing delegate" loc
      | Some (_, rolls) -> return rolls
    ) delegates

let test_successful_vote num_delegates () =
  Context.init num_delegates >>=? fun (b,_) ->

  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
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

  (* quorum starts at initial_qr *)
  Context.Vote.get_current_quorum (B b) >>=? fun v ->
  Assert.equal_int ~loc:__LOC__ initial_qr (Int32.to_int v) >>=? fun () ->

  (* listings must be populated in proposal period *)
  Context.Vote.get_listings (B b) >>=? begin function
    | [] -> failwith "%s - Unexpected empty listings" __LOC__
    | _ -> return_unit
  end >>=? fun () ->

  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p1, rolls_p1) ->

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

  let del1 = List.nth delegates_p1 0 in
  let del2 = List.nth delegates_p1 1 in
  let props = List.map (fun i -> protos.(i))
      (2 -- Constants.max_proposals_per_delegate) in
  Op.proposals (B b) del1 (Protocol_hash.zero::props) >>=? fun ops1 ->
  Op.proposals (B b) del2 [Protocol_hash.zero] >>=? fun ops2 ->
  Block.bake ~operations:[ops1;ops2] b >>=? fun b ->

  (* proposals are now populated *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->

  (* correctly count the double proposal for zero *)
  begin
    let weight = Int32.add (List.nth rolls_p1 0) (List.nth rolls_p1 1) in
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

  (* skip to testing_vote period
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

  (* beginning of testing_vote period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->

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

  (* unanimous vote: all delegates --active when p2 started-- vote *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates_p2 >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  Op.ballot (B b) del1 Protocol_hash.zero Vote.Nay >>=? fun op ->
  Block.bake ~operations:[op] b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Amendment.Unauthorized_ballot -> true
    | _ -> false
  end >>=? fun () ->

  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l rolls_p2 >>=? fun rolls_sum ->

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
          ) delegates_p2
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

  (* beginning of promotion_vote period, denoted by _p4;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p4, rolls_p4) ->

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

  (* unanimous vote: all delegates --active when p4 started-- vote *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    delegates_p4 >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l rolls_p4 >>=? fun rolls_sum ->

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
          ) delegates_p4
  end >>=? fun () ->

  (* skip to end of promotion_vote period and activation*)
  Block.bake_n Int32.((to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* zero is the new protocol (before the vote this value is unset) *)
  Context.Vote.get_protocol b >>= fun p ->
  Assert.equal ~loc:__LOC__ Protocol_hash.equal "Unexpected proposal"
    Protocol_hash.pp p Protocol_hash.zero >>=? fun () ->

  return_unit

(* given a list of active delegates,
   return the first k active delegates with which one can have quorum, that is:
   their roll sum divided by the total roll sum is bigger than qr_num/qr_den *)
let get_smallest_prefix_voters_for_quorum active_delegates active_rolls =
  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l active_rolls >>=? fun active_rolls_sum ->
  let rec loop delegates rolls sum selected =
    match delegates, rolls with
    | [], [] -> selected
    | del :: delegates, del_rolls :: rolls ->
        if den * sum < qr_num * (Int32.to_int active_rolls_sum) then
          loop delegates rolls (sum + (Int32.to_int del_rolls)) (del :: selected)
        else selected
    | _, _ -> [] in
  return (loop active_delegates active_rolls 0 [])

let get_expected_quorum ?(min_participation=0) rolls voter_rolls old_quorum =
  (* formula to compute the updated quorum as in the whitepaper *)
  let get_updated_quorum old_quorum participation =
    (* if not enough participation, don't update the quorum *)
    if participation < min_participation
    then (Int32.to_int old_quorum)
    else (qr_num * (Int32.to_int old_quorum) +
          pr_num * participation) / den
  in
  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l rolls >>=? fun rolls_sum ->
  fold_left_s (fun v acc -> return Int32.(add v acc))
    0l voter_rolls >>=? fun voter_rolls_sum ->
  let participation = (Int32.to_int voter_rolls_sum) * percent_mul /
                      (Int32.to_int rolls_sum) in
  return (get_updated_quorum old_quorum participation)

(* if not enough quorum -- get_updated_quorum < qr_num/qr_den -- in testing vote,
   go back to proposal period *)
let test_not_enough_quorum_in_testing_vote num_delegates () =
  Context.init num_delegates >>=? fun (b,delegates) ->

  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  (* proposal period *)
  let open Alpha_context in
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  let proposer = List.nth delegates 0 in
  Op.proposals (B b) proposer [Protocol_hash.zero] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->

  (* skip to vote_testing period
     -1 because we already baked one block with the proposal *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-2) b >>=? fun b ->

  (* we moved to a testing_vote period with one proposal *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  Context.Vote.get_current_quorum (B b) >>=? fun initial_quorum ->
  (* beginning of testing_vote period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->

  get_smallest_prefix_voters_for_quorum delegates_p2 rolls_p2 >>=? fun voters ->
  (* take the first voter out so there cannot be quorum *)
  let voters_without_quorum = List.tl voters in
  get_rolls b voters_without_quorum __LOC__ >>=? fun voters_rolls_in_testing_vote ->

  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters_without_quorum >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  (* skip to testing period *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* we move back to the proposal period because not enough quorum *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* check quorum update *)
  get_expected_quorum rolls_p2
    voters_rolls_in_testing_vote initial_quorum >>=? fun expected_quorum ->
  Context.Vote.get_current_quorum (B b) >>=? fun new_quorum ->
  (* assert the formula to calculate quorum is correct *)
  Assert.equal_int ~loc:__LOC__ expected_quorum
    (Int32.to_int new_quorum) >>=? fun () ->

  return_unit

(* if not enough quorum -- get_updated_quorum < qr_num/qr_den -- in promotion vote,
   go back to proposal period *)
let test_not_enough_quorum_in_promotion_vote num_delegates () =
  Context.init num_delegates >>=? fun (b,delegates) ->
  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  let proposer = List.nth delegates 0 in
  Op.proposals (B b) proposer (Protocol_hash.zero::[]) >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->

  (* skip to vote_testing period
     -1 because we already baked one block with the proposal *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-2) b >>=? fun b ->

  (* we moved to a testing_vote period with one proposal *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* beginning of testing_vote period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, rolls_p2) ->

  get_smallest_prefix_voters_for_quorum delegates_p2 rolls_p2 >>=? fun voters ->

  let open Alpha_context in

  (* all voters vote, for yays;
       no nays, so supermajority is satisfied *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters >>=? fun operations ->

  Block.bake ~operations b >>=? fun b ->

  (* skip to testing period *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* we move to testing because we have supermajority and enough quorum *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  (* skip to promotion_vote period *)
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Promotion_vote -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  Context.Vote.get_current_quorum (B b) >>=? fun initial_quorum ->
  (* beginning of promotion period, denoted by _p4;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p4, rolls_p4) ->
  get_smallest_prefix_voters_for_quorum delegates_p4 rolls_p4 >>=? fun voters ->

  (* take the first voter out so there cannot be quorum *)
  let voters_without_quorum = List.tl voters in
  get_rolls b voters_without_quorum __LOC__ >>=? fun voter_rolls ->

  (* all voters_without_quorum vote, for yays;
     no nays, so supermajority is satisfied *)
  map_s (fun del ->
      Op.ballot (B b) del Protocol_hash.zero Vote.Yay)
    voters_without_quorum >>=? fun operations ->
  Block.bake ~operations b >>=? fun b ->

  (* skip to end of promotion_vote period *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  get_expected_quorum rolls_p4 voter_rolls
    initial_quorum >>=? fun expected_quorum ->

  Context.Vote.get_current_quorum (B b) >>=? fun new_quorum ->

  (* assert the formula to calculate quorum is correct *)
  Assert.equal_int ~loc:__LOC__ expected_quorum
    (Int32.to_int new_quorum) >>=? fun () ->

  (* we move back to the proposal period because not enough quorum *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit

let test_multiple_identical_proposals_count_as_one () =
  Context.init 1 >>=? fun (b,delegates) ->

  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  let proposer = List.hd delegates in
  Op.proposals (B b) proposer
    [Protocol_hash.zero; Protocol_hash.zero] >>=? fun ops ->
  Block.bake ~operations:[ops] b >>=? fun b ->
  (* compute the weight of proposals *)
  Context.Vote.get_proposals (B b) >>=? fun ps ->

  (* compute the rolls of proposer *)
  Context.Contract.pkh proposer >>=? fun pkh ->
  Context.Vote.get_listings (B b) >>=? fun l ->
  begin match List.find_opt (fun (del,_) -> del = pkh) l with
    | None -> failwith "%s - Missing delegate" __LOC__
    | Some (_, proposer_rolls) -> return proposer_rolls
  end >>=? fun proposer_rolls ->

  (* correctly count the double proposal for zero as one proposal *)
  let expected_weight_proposer = proposer_rolls in
  match Alpha_environment.Protocol_hash.(Map.find_opt zero ps) with
  | Some v -> if v = expected_weight_proposer then return_unit
      else failwith
          "%s - Wrong count %ld is not %ld; identical proposals count as one"
          __LOC__ v expected_weight_proposer
  | None -> failwith "%s - Missing proposal" __LOC__


(* assumes the initial balance of allocated by Context.init is at
   least 4 time the value of the tokens_per_roll constant *)
let test_supermajority_in_proposal there_is_a_winner () =
  Context.init ~initial_balances:[1L; 1L; 1L] 10 >>=? fun (b,delegates) ->
  Context.get_constants (B b)
  >>=? fun { parametric = {blocks_per_cycle; blocks_per_voting_period; tokens_per_roll; _ } ; _ } ->

  let del1 = List.nth delegates 0 in
  let del2 = List.nth delegates 1 in
  let del3 = List.nth delegates 2 in

  map_s (fun del -> Context.Contract.pkh del) [del1; del2; del3] >>=? fun pkhs ->
  let policy = Block.Excluding pkhs in

  Op.transaction (B b) (List.nth delegates 3) del1 tokens_per_roll >>=? fun op1 ->
  Op.transaction (B b) (List.nth delegates 4) del2 tokens_per_roll >>=? fun op2 ->
  begin
    if there_is_a_winner
    then Test_tez.Tez.( *? ) tokens_per_roll 3L
    else Test_tez.Tez.( *? ) tokens_per_roll 2L
  end >>?= fun bal3 ->
  Op.transaction (B b) (List.nth delegates 5) del3 bal3 >>=? fun op3 ->

  Block.bake ~policy ~operations:[op1; op2; op3] b >>=? fun b ->

  (* to avoid the bug where the listings are not initialized, we let
     one voting period pass; we make sure that the three selected
     delegates remain active and their number of rolls do not change *)
  let amount = let open Test_tez in Tez.of_int 10 in
  fold_left_s (fun b _ ->
      Op.transaction (B b) del1 del2 amount >>=? fun op1 ->
      Op.transaction (B b) del2 del3 amount >>=? fun op2 ->
      Op.transaction (B b) del3 del1 amount >>=? fun op3 ->
      Block.bake ~policy ~operations:[op1; op2; op3] b >>=? fun b ->
      Block.bake_until_cycle_end ~policy b
    ) b (1 --
         (Int32.to_int (Int32.div blocks_per_voting_period blocks_per_cycle))) >>=? fun b ->

  (* make the proposals *)
  Op.proposals (B b) del1 [protos.(0)] >>=? fun ops1 ->
  Op.proposals (B b) del2 [protos.(0)] >>=? fun ops2 ->
  Op.proposals (B b) del3 [protos.(1)] >>=? fun ops3 ->
  Block.bake ~policy ~operations:[ops1;ops2;ops3] b >>=? fun b ->
  Block.bake_n ~policy ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* we remain in the proposal period when there is no winner,
     otherwise we move to the testing vote period *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Testing_vote ->
        if there_is_a_winner then return_unit
        else failwith "%s - Expected period kind Proposal, obtained Testing_vote" __LOC__
    | Proposal ->
        if not there_is_a_winner then return_unit
        else failwith "%s - Expected period kind Testing_vote, obtained Proposal" __LOC__
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit


let test_supermajority_in_testing_vote supermajority () =
  Context.init 100 >>=? fun (b,delegates) ->

  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  let del1 = List.nth delegates 0 in
  let proposal = protos.(0) in

  Op.proposals (B b) del1 [proposal] >>=? fun ops1 ->
  Block.bake ~operations:[ops1] b >>=? fun b ->
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-1) b >>=? fun b ->

  (* move to testing_vote *)
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

  (* beginning of testing_vote period, denoted by _p2;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p2, _olls_p2) ->

  (* supermajority means [num_yays / (num_yays + num_nays) >= s_num / s_den],
     which is equivalent with [num_yays >= num_nays * s_num / (s_den - s_num)] *)
  let num_delegates = List.length delegates_p2 in
  let num_nays = num_delegates / 5 in (* any smaller number will do as well *)
  let num_yays = num_nays * s_num / (s_den - s_num) in
  (* majority/minority vote depending on the [supermajority] parameter *)
  let num_yays = if supermajority then num_yays else num_yays - 1 in

  let open Alpha_context in

  let nays_delegates, rest = List.split_n num_nays delegates_p2 in
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
        else failwith "%s - Expected period kind Testing_vote, obtained Proposal" __LOC__
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit

(* test also how the selection scales: all delegates propose max proposals *)
let test_no_winning_proposal num_delegates () =
  Context.init num_delegates >>=? fun (b,_) ->

  (* Because of a minor bug in the initialization of the voting state, the
     listings are not populated in the very first period. After that they get
     correctly populated. An empty listing means no proposals will be accepted. *)
  Context.get_constants (B b) >>=? fun { parametric = {blocks_per_voting_period; _ } ; _ } ->
  Block.bake_n (Int32.to_int blocks_per_voting_period) b >>=? fun b ->

  (* beginning of proposal, denoted by _p1;
     take a snapshot of the active delegates and their rolls from listings *)
  get_delegates_and_rolls_from_listings b >>=? fun (delegates_p1, _rolls_p1) ->

  let open Alpha_context in
  let props = List.map (fun i -> protos.(i))
      (1 -- Constants.max_proposals_per_delegate) in
  (* all delegates active in p1 propose the same proposals *)
  map_s
    (fun del -> Op.proposals (B b) del props)
    delegates_p1 >>=? fun ops_list ->
  Block.bake ~operations:ops_list b >>=? fun b ->

  (* skip to testing_vote period
     -1 because we already baked one block with the proposal *)
  Block.bake_n ((Int32.to_int blocks_per_voting_period)-2) b >>=? fun b ->

  (* we stay in the same proposal period because no winning proposal *)
  Context.Vote.get_current_period_kind (B b) >>=? begin function
    | Proposal -> return_unit
    | _ -> failwith "%s - Unexpected period kind" __LOC__
  end >>=? fun () ->

  return_unit

let tests = [
  Test.tztest "voting successful_vote" `Quick (test_successful_vote 137) ;
  Test.tztest "voting testing vote, not enough quorum" `Quick (test_not_enough_quorum_in_testing_vote 245) ;
  Test.tztest "voting promotion vote, not enough quorum" `Quick (test_not_enough_quorum_in_promotion_vote 432) ;
  Test.tztest "voting counting double proposal" `Quick test_multiple_identical_proposals_count_as_one;
  Test.tztest "voting proposal, with supermajority" `Quick (test_supermajority_in_proposal true) ;
  Test.tztest "voting proposal, without supermajority" `Quick (test_supermajority_in_proposal false) ;
  Test.tztest "voting testing vote, with supermajority" `Quick (test_supermajority_in_testing_vote true) ;
  Test.tztest "voting testing vote, without supermajority" `Quick (test_supermajority_in_testing_vote false) ;
  Test.tztest "voting proposal, no winning proposal" `Quick (test_no_winning_proposal 400) ;
]
