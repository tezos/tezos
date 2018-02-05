(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception Unknown_protocol

(** Miscellaneous self-descriptive functions *)

val no_ops_hash : Operation_list_list_hash.t

val find_account :
  Helpers_account.t list -> Ed25519.Public_key_hash.t -> Helpers_account.t

val read_file : string -> string

