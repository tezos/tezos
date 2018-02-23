(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Non_delegatable_contract of Contract_repr.contract (* `Permanent *)

val is_delegatable:
  Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

val init:
  Raw_context.t -> Contract_repr.t -> Ed25519.Public_key_hash.t ->
  Raw_context.t tzresult Lwt.t

val get:
  Raw_context.t -> Contract_repr.t ->
  Ed25519.Public_key_hash.t option tzresult Lwt.t

val set:
  Raw_context.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option ->
  Raw_context.t tzresult Lwt.t

val remove: Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t
