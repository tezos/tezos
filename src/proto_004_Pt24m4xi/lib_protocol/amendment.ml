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

(** Returns the proposal submitted by the most delegates.
    Returns None in case of a tie or if there are no proposals. *)
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

(** A proposal is approved if it has supermajority and the participation reaches
    the current quorum.
    Supermajority means the yays are more 8/10 of casted votes.
    The participation is the ratio of all received votes, including passes, with
    respect to the number of possible votes. The quorum starts at 80% and at
    each vote is updated using the last expected quorum and the current
    participation with the following weights:
    newQ = oldQ * 8/10 + participation * 2/10 *)
let check_approval_and_update_quorum ctxt =
  Vote.get_ballots ctxt >>=? fun ballots ->
  Vote.listing_size ctxt >>=? fun maximum_vote ->
  Vote.get_current_quorum ctxt >>=? fun expected_quorum ->
  (* Note overflows: considering a maximum of 8e8 tokens, with roll size as
     small as 1e3, there is a maximum of 8e5 rolls and thus votes.
     In 'participation' an Int64 is used because in the worst case 'all_votes is
     8e5 and after the multiplication is 8e9, making it potentially overflow a
     signed Int32 which is 2e9. *)
  let casted_votes = Int32.add ballots.yay ballots.nay in
  let all_votes = Int32.add casted_votes ballots.pass in
  let supermajority = Int32.div (Int32.mul 8l casted_votes) 10l in
  let participation = (* in centile of percentage *)
    Int64.to_int32
      (Int64.div
         (Int64.mul (Int64.of_int32 all_votes) 100_00L)
         (Int64.of_int32 maximum_vote)) in
  let outcome = Compare.Int32.(participation >= expected_quorum &&
                               ballots.yay >= supermajority) in
  let updated_quorum =
    Int32.div (Int32.add (Int32.mul 8l expected_quorum) (Int32.mul 2l participation)) 10l in
  Vote.set_current_quorum ctxt updated_quorum >>=? fun ctxt ->
  return (ctxt, outcome)

(** Implements the state machine of the amendment procedure.
    Note that [freeze_listings], that computes the vote weight of each delegate,
    is run at the beginning of each voting period.
*)
let start_new_voting_period ctxt =
  Vote.get_current_period_kind ctxt >>=? function
  | Proposal -> begin
      Vote.get_proposals ctxt >>=? fun proposals ->
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
          Time.add (Timestamp.current ctxt) (Constants.test_chain_duration ctxt) in
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
  | Too_many_proposals
  | Empty_proposal
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
    (fun () -> Unauthorized_ballot) ;
  (* Too many proposals *)
  register_error_kind
    `Branch
    ~id:"too_many_proposals"
    ~title:"Too many proposals"
    ~description:"The delegate reached the maximum number of allowed proposals."
    ~pp:(fun ppf () -> Format.fprintf ppf "Too many proposals")
    empty
    (function Too_many_proposals -> Some () | _ -> None)
    (fun () -> Too_many_proposals) ;
  (* Empty proposal *)
  register_error_kind
    `Branch
    ~id:"empty_proposal"
    ~title:"Empty proposal"
    ~description:"Proposal lists cannot be empty."
    ~pp:(fun ppf () -> Format.fprintf ppf "Empty proposal")
    empty
    (function Empty_proposal -> Some () | _ -> None)
    (fun () -> Empty_proposal)

(* @return [true] if [List.length l] > [n] w/o computing length *)
let rec longer_than l n =
  if Compare.Int.(n < 0) then assert false else
    match l with
    | [] -> false
    | _ :: rest ->
        if Compare.Int.(n = 0) then true
        else (* n > 0 *)
          longer_than rest (n-1)

let record_proposals ctxt delegate proposals =
  begin match proposals with
    | [] -> fail Empty_proposal
    | _ :: _ -> return_unit
  end >>=? fun () ->
  Vote.get_current_period_kind ctxt >>=? function
  | Proposal ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then
        Vote.recorded_proposal_count_for_delegate ctxt delegate >>=? fun count ->
        fail_when
          (longer_than proposals (Constants.max_proposals_per_delegate - count))
          Too_many_proposals >>=? fun () ->
        fold_left_s
          (fun ctxt proposal ->
             Vote.record_proposal ctxt proposal delegate)
          ctxt proposals >>=? fun ctxt ->
        return ctxt
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
      Vote.has_recorded_ballot ctxt delegate >>= fun has_ballot ->
      fail_when has_ballot Unauthorized_ballot >>=? fun () ->
      Vote.in_listings ctxt delegate >>= fun in_listings ->
      if in_listings then
        Vote.record_ballot ctxt delegate ballot
      else
        fail Unauthorized_ballot
  | Testing | Proposal ->
      fail Unexpected_ballot

let last_of_a_voting_period ctxt l =
  Compare.Int32.(Int32.succ l.Level.voting_period_position =
                 Constants.blocks_per_voting_period ctxt )

let may_start_new_voting_period ctxt =
  let level = Level.current ctxt in
  if last_of_a_voting_period ctxt level then
    start_new_voting_period ctxt
  else
    return ctxt
