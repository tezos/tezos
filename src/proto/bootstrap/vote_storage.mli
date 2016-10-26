(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val record_proposal:
  Storage.t -> Protocol_hash.t -> Ed25519.public_key_hash ->
  Storage.t tzresult Lwt.t

val get_proposals:
  Storage.t -> int32 Protocol_hash_map.t tzresult Lwt.t

val clear_proposals: Storage.t -> Storage.t tzresult Lwt.t

type ballots = {
  yay: int32 ;
  nay: int32 ;
  pass: int32 ;
}

val record_ballot:
  Storage.t -> Ed25519.public_key_hash -> Vote_repr.ballot ->
  Storage.t tzresult Lwt.t
val get_ballots: Storage.t -> ballots tzresult Lwt.t
val clear_ballots: Storage.t -> Storage.t Lwt.t

val freeze_listings: Storage.t -> Storage.t tzresult Lwt.t
val clear_listings: Storage.t -> Storage.t tzresult Lwt.t

val listing_size: Storage.t -> int32 tzresult Lwt.t
val in_listings:
  Storage.t -> Ed25519.public_key_hash -> bool Lwt.t

val get_current_quorum: Storage.t -> int32 tzresult Lwt.t
val set_current_quorum: Storage.t -> int32 -> Storage.t tzresult Lwt.t

val get_current_period_kind:
  Storage.t -> Voting_period_repr.kind tzresult Lwt.t
val set_current_period_kind:
  Storage.t -> Voting_period_repr.kind -> Storage.t tzresult Lwt.t

val get_current_proposal:
  Storage.t -> Protocol_hash.t tzresult Lwt.t
val set_current_proposal:
  Storage.t -> Protocol_hash.t -> Storage.t tzresult Lwt.t
val clear_current_proposal: Storage.t -> Storage.t tzresult Lwt.t

val init: Storage.t -> Storage.t tzresult Lwt.t
