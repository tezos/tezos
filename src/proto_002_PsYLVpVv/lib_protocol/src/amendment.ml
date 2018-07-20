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

open Alpha_context

let () = ()

let select_winning_proposal proposals =
  let merge proposal vote winners =
    match winners with
    | None -> Some ([proposal], vote)
    | Some (winners, winners_vote) as previous ->
        if Compare.Int32.(vote = winners_vote) then
          Some (proposal :: winners, winners_vote)
        else if Compare.Int32.(vote >= winners_vote) then
          Some ([proposal], vote)
        else
          previous in
  match Protocol_hash.Map.fold merge proposals None with
  | None -> None
  | Some ([proposal], _) -> Some proposal
  | Some _ -> None (* in case of a tie, lets do nothing. *)

let check_approval_and_update_quorum ctxt =
  Vote.get_ballots ctxt >>=? fun ballots ->
  Vote.listing_size ctxt >>=? fun maximum_vote ->
  Vote.get_current_quorum ctxt >>=? fun expected_quorum ->
  (* FIXME check overflow ??? *)
  let casted_vote = Int32.add ballots.yay ballots.nay in
  let actual_vote = Int32.add casted_vote ballots.pass in
  let actual_quorum =
    Int32.div (Int32.mul actual_vote 100_00l) maximum_vote in
  let supermajority = Int32.div (Int32.mul 8l casted_vote) 10l in
  let updated_quorum =
    Int32.div
      (Int32.add (Int32.mul 8l expected_quorum)
         (Int32.mul 2l actual_quorum))
      10l in
  Vote.set_current_quorum ctxt updated_quorum >>=? fun ctxt ->
  return
    (ctxt,
     Compare.Int32.(actual_quorum >= expected_quorum
                    && ballots.yay >= supermajority))

let start_new_voting_cycle ctxt =
  Vote.get_current_period_kind ctxt >>=? function
  | Proposal -> begin
      Vote.get_proposals ctxt >>= fun proposals ->
      Vote.clear_proposals ctxt >>= fun ctxt ->
      Vote.clear_listings ctxt >>=? fun ctxt ->
      match select_winning_proposal proposals with
      | None ->
          Vote.freeze_listings ctxt >>=? fun ctxt ->
          return ctxt
      | Some proposal ->
          Vote.init_current_proposal ctxt proposal >>=? fun ctxt ->
          Vote.freeze_listings ctxt >>=? fun ctxt ->
          Vote.set_current_period_kind ctxt Testing_vote >>=? fun ctxt ->
          return ctxt
    end
  | Testing_vote ->
      check_approval_and_update_quorum ctxt >>=? fun (ctxt, approved) ->
      Vote.clear_ballots ctxt >>= fun ctxt ->
      Vote.clear_listings ctxt >>=? fun ctxt ->
      if approved then
        let expiration = (* in two days maximum... *)
          Time.add (Timestamp.current ctxt) (Int64.mul 48L 3600L) in
        Vote.get_current_proposal ctxt >>=? fun proposal ->
        fork_test_chain ctxt proposal expiration >>= fun ctxt ->
        Vote.set_current_period_kind ctxt Testing >>=? fun ctxt ->
        return ctxt
      else
        Vote.clear_current_proposal ctxt >>=? fun ctxt ->
        Vote.freeze_listings ctxt >>=? fun ctxt ->
        Vote.set_current_period_kind ctxt Proposal >>=? fun ctxt ->
        return ctxt
  | Testing ->
      Vote.freeze_listings ctxt >>=? fun ctxt ->
      Vote.set_current_period_kind ctxt Promotion_vote >>=? fun ctxt ->
      return ctxt
  | Promotion_vote ->
      check_approval_and_update_quorum ctxt >>=? fun (ctxt, approved) ->
      begin
        if approved then
          Vote.get_current_proposal ctxt >>=? fun proposal ->
          activate ctxt proposal >>= fun ctxt ->
          return ctxt
        else
          return ctxt
      end >>=? fun ctxt ->
      Vote.clear_ballots ctxt >>= fun ctxt ->
      Vote.clear_listings ctxt >>=? fun ctxt ->
      Vote.clear_current_proposal ctxt >>=? fun ctxt ->
      Vote.freeze_listings ctxt >>=? fun ctxt ->
      Vote.set_current_period_kind ctxt Proposal >>=? fun ctxt ->
      return ctxt

type error += (* `Branch *)
  | Invalid_proposal
  | Unexpected_proposal
  | Unauthorized_proposal
  | Unexpected_ballot
  | Unauthorized_ballot

let () =
  let open Data_encoding in
  (* Invalid proposal *)
  register_error_kind
    `Branch
    ~id:"invalid_proposal"
    ~title:"Invalid proposal"
    ~description:"Ballot provided for a proposal that is not the current one."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid proposal")
    empty
    (function Invalid_proposal -> Some () | _ -> None)
    (fun () -> Invalid_proposal) ;
  (* Unexpected proposal *)
  register_error_kind
    `Branch
    ~id:"unexpected_proposal"
    ~title:"Unexpected proposal"
    ~description:"Proposal recorded outside of a proposal period."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected proposal")
    empty
    (function Unexpected_proposal -> Some () | _ -> None)
    (fun () -> Unexpected_proposal) ;
  (* Unauthorized proposal *)
  register_error_kind
    `Branch
    ~id:"unauthorized_proposal"
    ~title:"Unauthorized proposal"
    ~description:"The delegate provided for the proposal is not in the voting listings."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unauthorized proposal")
    empty
    (function Unauthorized_proposal -> Some () | _ -> None)
    (fun () -> Unauthorized_proposal) ;
  (* Unexpected ballot *)
  register_error_kind
    `Branch
    ~id:"unexpected_ballot"
    ~title:"Unexpected ballot"
    ~description:"Ballot recorded outside of a voting period."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected ballot")
    empty
    (function Unexpected_ballot -> Some () | _ -> None)
    (fun () -> Unexpected_ballot) ;
  (* Unauthorized ballot *)
  register_error_kind
    `Branch
    ~id:"unauthorized_ballot"
    ~title:"Unauthorized ballot"
    ~description:"The delegate provided for the ballot is not in the voting listings."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unauthorized ballot")
    empty
    (function Unauthorized_ballot -> Some () | _ -> None)
    (fun () -> Unauthorized_ballot)

let record_proposals ctxt delegate proposals =
  Vote.get_current_period_kind ctxt >>=? function
  | Proposal ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then
        Lwt_list.fold_left_s
          (fun ctxt proposal ->
             Vote.record_proposal ctxt proposal delegate)
          ctxt proposals >>= return
      else
        fail Unauthorized_proposal
  | Testing_vote | Testing | Promotion_vote ->
      fail Unexpected_proposal

let record_ballot ctxt delegate proposal ballot =
  Vote.get_current_period_kind ctxt >>=? function
  | Testing_vote | Promotion_vote ->
      Vote.get_current_proposal ctxt >>=? fun current_proposal ->
      fail_unless (Protocol_hash.equal proposal current_proposal)
        Invalid_proposal >>=? fun () ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then
        Vote.record_ballot ctxt delegate ballot >>= return
      else
        fail Unauthorized_ballot
  | Testing | Proposal ->
      fail Unexpected_ballot

let last_of_a_voting_period ctxt l =
  Compare.Int32.(Int32.succ l.Level.voting_period_position =
                 Constants.blocks_per_voting_period ctxt )

let may_start_new_voting_cycle ctxt =
  let level = Level.current ctxt in
  if last_of_a_voting_period ctxt level then
    start_new_voting_cycle ctxt
  else
    return ctxt
