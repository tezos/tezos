(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

val may_start_new_voting_cycle:
  context -> context tzresult Lwt.t

type error +=
  | Unexpected_proposal
  | Unauthorized_proposal

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
