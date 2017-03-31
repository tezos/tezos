(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val generate_seed_nonce: unit -> Nonce.t
(** [generate_seed_nonce ()] is a random nonce that is typically used
    in block headers. When baking, bakers generate random nonces whose
    hash is commited in the block they bake. They will typically
    reveal the aforementionned nonce during the next cycle. *)

val inject_block:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  priority:int ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  seed_nonce:Nonce.t ->
  src_sk:secret_key ->
  Operation_hash.t list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt blk ?force ~priority ~timestamp ~fitness
    ~seed_nonce ~src_sk ops] tries to inject a block in the node. If
    [?force] is set, the fitness check will be bypassed. [priority]
    will be used to compute the mining slot (level is
    precomputed). [src_sk] is used to sign the block header. *)

val forge_block:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?operations:Operation_hash.t list ->
  ?best_effort:bool ->
  ?sort:bool ->
  ?timestamp:Time.t ->
  priority:[`Set of int | `Auto of (public_key_hash * int option)] ->
  seed_nonce:Nonce.t ->
  src_sk:secret_key ->
  unit ->
  Block_hash.t tzresult Lwt.t
(** [forge_block cctxt parent_blk ?force ?operations ?best_effort
    ?sort ?timestamp ?max_priority ?priority ~seed_nonce ~src_sk
    pk_hash] injects a block in the node. In addition of inject_block,
    it will:

    * Operations: If [?operations] is [None], it will get pending
      operations and add them to the block. Otherwise, provided
      operations will be used. In both cases, they will be validated.

    * Mining priority: If [`Auto] is used, it will be computed from
      the public key hash of the specified contract, optionally capped
      to a maximum value.

    * Timestamp: If [?timestamp] is set, and is compatible with the
      computed mining priority, it will be used. Otherwise, it will be
      set at the best mining priority.
*)

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
