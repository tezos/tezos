(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

(** Mine a block *)
val bake_block:
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?max_priority: int ->
  ?free_baking: bool ->
  ?src_sk:secret_key ->
  public_key_hash ->
  unit tzresult Lwt.t

(** Endorse a block *)
val endorse_block:
  Client_commands.full_context ->
  ?max_priority:int ->
  Client_keys.Public_key_hash.t ->
  unit Error_monad.tzresult Lwt.t

(** Get the previous cycle of the given cycle *)
val get_predecessor_cycle:
  Client_commands.full_context ->
  Cycle.t ->
  Cycle.t Lwt.t

(** Reveal the nonces used to bake each block in the given list *)
val reveal_block_nonces :
  Client_commands.full_context ->
  Block_hash.t list ->
  unit Error_monad.tzresult Lwt.t

(** Reveal all unrevealed nonces *)
val reveal_nonces :
  Client_commands.full_context ->
  unit ->
  unit Error_monad.tzresult Lwt.t

(** Initialize the baking daemon *)
val run_daemon:
  Client_commands.full_context ->
  ?max_priority:int ->
  endorsement_delay:int ->
  ('a * public_key_hash) list ->
  endorsement:bool ->
  baking:bool ->
  denunciation:bool ->
  unit Error_monad.tzresult Lwt.t
