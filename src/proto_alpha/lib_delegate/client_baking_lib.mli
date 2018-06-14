(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

(** Mine a block *)
val bake_block:
  #Proto_alpha.full ->
  ?chain:Chain_services.chain ->
  Block_services.block ->
  ?force:bool ->
  ?max_priority: int ->
  ?minimal_timestamp: bool ->
  ?src_sk:Client_keys.sk_uri ->
  public_key_hash ->
  unit tzresult Lwt.t

(** Endorse a block *)
val endorse_block:
  #Proto_alpha.full ->
  Client_keys.Public_key_hash.t ->
  unit Error_monad.tzresult Lwt.t

(** Get the previous cycle of the given cycle *)
val get_predecessor_cycle:
  #Proto_alpha.full ->
  Cycle.t ->
  Cycle.t Lwt.t

(** Reveal the nonces used to bake each block in the given list *)
val reveal_block_nonces :
  #Proto_alpha.full ->
  Block_hash.t list ->
  unit Error_monad.tzresult Lwt.t

(** Reveal all unrevealed nonces *)
val reveal_nonces :
  #Proto_alpha.full ->
  unit ->
  unit Error_monad.tzresult Lwt.t
