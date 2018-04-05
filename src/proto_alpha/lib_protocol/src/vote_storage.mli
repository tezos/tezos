(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val record_proposal:
  Raw_context.t -> Protocol_hash.t -> Signature.Public_key_hash.t ->
  Raw_context.t Lwt.t

val get_proposals:
  Raw_context.t -> int32 Protocol_hash.Map.t Lwt.t

val clear_proposals: Raw_context.t -> Raw_context.t Lwt.t

type ballots = {
  yay: int32 ;
  nay: int32 ;
  pass: int32 ;
}

val record_ballot:
  Raw_context.t -> Signature.Public_key_hash.t -> Vote_repr.ballot ->
  Raw_context.t Lwt.t
val get_ballots: Raw_context.t -> ballots tzresult Lwt.t
val clear_ballots: Raw_context.t -> Raw_context.t Lwt.t

val freeze_listings: Raw_context.t -> Raw_context.t tzresult Lwt.t
val clear_listings: Raw_context.t -> Raw_context.t tzresult Lwt.t

val listing_size: Raw_context.t -> int32 tzresult Lwt.t
val in_listings:
  Raw_context.t -> Signature.Public_key_hash.t -> bool Lwt.t

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
