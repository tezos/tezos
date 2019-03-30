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

(**
   Only delegates with at least one roll take part in the amendment procedure.
   It works as follows:
   - Proposal period: delegates can submit protocol amendment proposals using
     the proposal operation. At the end of a proposal period, the proposal with
     most supporters is selected and we move to a testing_vote period.
     If there are no proposals, or a tie between proposals, a new proposal
     period starts.
   - Testing_vote period: delegates can cast votes to test or not the winning
     proposal using the ballot operation.
     At the end of a testing_vote period if participation reaches the quorum
     and the proposal has a supermajority in favor, we proceed to a testing
     period. Otherwise we go back to a proposal period.
     In any case, if there is enough participation the quorum is updated.
   - Testing period: a test chain is forked for the lengh of the period.
     At the end of a testing period we move to a promotion_vote period.
   - Promotion_vote period: delegates can cast votes to promote or not the
     tested proposal using the ballot operation.
     At the end of a promotion_vote period if participation reaches the quorum
     and the tested proposal has a supermajority in favor, it is activated as
     the new protocol. Otherwise we go back to a proposal period.
     In any case, if there is enough participation the quorum is updated.
*)

open Alpha_context

(** If at the end of a voting period, moves to the next one following
    the state machine of the amendment procedure. *)
val may_start_new_voting_period:
  context -> context tzresult Lwt.t

type error +=
  | Unexpected_proposal
  | Unauthorized_proposal
  | Too_many_proposals
  | Empty_proposal

(** Records a list of proposals for a delegate.
    @raise Unexpected_proposal if [ctxt] is not in a proposal period.
    @raise Unauthorized_proposal if [delegate] is not in the listing. *)
val record_proposals:
  context ->
  public_key_hash -> Protocol_hash.t list ->
  context tzresult Lwt.t

type error +=
  | Invalid_proposal
  | Unexpected_ballot
  | Unauthorized_ballot

val record_ballot:
  context ->
  public_key_hash -> Protocol_hash.t -> Vote.ballot ->
  context tzresult Lwt.t
