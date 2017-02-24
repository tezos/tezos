(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val generate_seed_nonce: unit -> Nonce.t

val inject_block:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  priority:int ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  seed_nonce:Nonce.t ->
  src_sk:secret_key ->
  Operation_hash.t list ->
  Block_hash.t tzresult Lwt.t

val forge_block:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?operations:Operation_hash.t list ->
  ?best_effort:bool ->
  ?sort:bool ->
  ?timestamp:Time.t ->
  ?max_priority:int ->
  ?priority:int ->
  seed_nonce:Nonce.t ->
  src_sk:secret_key ->
  public_key_hash ->
  Block_hash.t tzresult Lwt.t

module State : sig
  val get_block:
    Client_commands.context ->
    Raw_level.t -> Block_hash.t list tzresult Lwt.t
  val record_block:
    Client_commands.context ->
    Raw_level.t -> Block_hash.t -> Nonce.t -> unit tzresult Lwt.t
end

val create:
  Client_commands.context ->
  ?max_priority: int ->
  public_key_hash list ->
  Client_mining_blocks.block_info list Lwt_stream.t ->
  Client_mining_operations.valid_endorsement Lwt_stream.t ->
  unit Lwt.t

val get_unrevealed_nonces:
  Client_commands.context ->
  ?force:bool ->
  Client_proto_rpcs.block ->
  (Block_hash.t * (Raw_level.t * Nonce.t)) list tzresult Lwt.t
