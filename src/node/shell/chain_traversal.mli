(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open State

val path: Block.t -> Block.t -> Block.t list option Lwt.t
(** If [h1] is an ancestor of [h2] in the current [state],
    then [path state h1 h2] returns the chain of block from
    [h1] (excluded) to [h2] (included). Returns [None] otherwise. *)

val common_ancestor: Block.t -> Block.t -> Block.t Lwt.t
(** [common_ancestor state h1 h2] returns the first common ancestors
    in the history of blocks [h1] and [h2]. *)

val block_locator: Block.t -> int -> Block_hash.t list Lwt.t
(** [block_locator state max_length h] compute the sparse block locator
    (/à la/ Bitcoin) for the block [h]. *)

val iter_predecessors:
  ?max:int ->
  ?min_fitness:Fitness.t ->
  ?min_date:Time.t ->
  Block.t list ->
  f:(Block.t -> unit Lwt.t) ->
  unit Lwt.t
(** [iter_predecessors state blocks f] iter [f] on [blocks] and
    their recursive predecessors. Blocks are visited with a
    decreasing fitness (then decreasing timestamp). If the optional
    argument [max] is provided, the iteration is stopped after [max]
    visited block. If [min_fitness] id provided, blocks with a
    fitness lower than [min_fitness] are ignored. If [min_date],
    blocks with a fitness lower than [min_date] are ignored. *)

val new_blocks:
  from_block:Block.t -> to_block:Block.t ->
  (Block.t * Block.t list) Lwt.t
(** [new_blocks ~from_block ~to_block] returns a pair [(ancestor,
    path)], where [ancestor] is the common ancestor of [from_block]
    and [to_block] and where [path] is the chain from [ancestor]
    (excluded) to [to_block] (included).  The function raises an
    exception when the two provided blocks do not belong the the same
    [net].  *)

val live_blocks:
  Block.t -> int -> (Block_hash.Set.t * Operation_hash.Set.t) Lwt.t
(** [live_blocks b n] return a pair [(blocks,operations)] where
    [blocks] is the set of arity [n], that contains [b] and its [n-1]
    predecessors. And where [operations] is the set of operations
    included in those blocks.
 *)
