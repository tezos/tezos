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

(** Records a proposal per delegate *)
val record_proposal:
  Raw_context.t -> Protocol_hash.t -> Signature.Public_key_hash.t ->
  Raw_context.t tzresult Lwt.t

val recorded_proposal_count_for_delegate:
  Raw_context.t -> Signature.Public_key_hash.t ->
  int tzresult Lwt.t

val get_proposals:
  Raw_context.t -> int32 Protocol_hash.Map.t tzresult Lwt.t

val clear_proposals: Raw_context.t -> Raw_context.t Lwt.t

type ballots = {
  yay: int32 ;
  nay: int32 ;
  pass: int32 ;
}

val ballots_encoding : ballots Data_encoding.t

val has_recorded_ballot : Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t
val record_ballot:
  Raw_context.t -> Signature.Public_key_hash.t -> Vote_repr.ballot ->
  Raw_context.t tzresult Lwt.t
val get_ballots: Raw_context.t -> ballots tzresult Lwt.t
val get_ballot_list :
  Raw_context.t -> (Signature.Public_key_hash.t * Vote_repr.ballot) list Lwt.t
val clear_ballots: Raw_context.t -> Raw_context.t Lwt.t

val listings_encoding : (Signature.Public_key_hash.t * int32) list Data_encoding.t

val freeze_listings: Raw_context.t -> Raw_context.t tzresult Lwt.t
val clear_listings: Raw_context.t -> Raw_context.t tzresult Lwt.t

val listing_size: Raw_context.t -> int32 tzresult Lwt.t
val in_listings:
  Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t
val get_listings : Raw_context.t -> (Signature.Public_key_hash.t * int32) list Lwt.t

val get_current_quorum: Raw_context.t -> int32 tzresult Lwt.t
val set_current_quorum: Raw_context.t -> int32 -> Raw_context.t tzresult Lwt.t

val get_current_period_kind:
  Raw_context.t -> Voting_period_repr.kind tzresult Lwt.t
val set_current_period_kind:
  Raw_context.t -> Voting_period_repr.kind -> Raw_context.t tzresult Lwt.t

val get_current_proposal:
  Raw_context.t -> Protocol_hash.t tzresult Lwt.t
val init_current_proposal:
  Raw_context.t -> Protocol_hash.t -> Raw_context.t tzresult Lwt.t
val clear_current_proposal: Raw_context.t -> Raw_context.t tzresult Lwt.t

val init: Raw_context.t -> Raw_context.t tzresult Lwt.t
