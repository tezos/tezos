(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val get_opt:
  Raw_context.t -> Unclaimed_public_key_hash.t ->
  Commitment_repr.t option tzresult Lwt.t

val delete:
  Raw_context.t -> Unclaimed_public_key_hash.t ->
  Raw_context.t tzresult Lwt.t
