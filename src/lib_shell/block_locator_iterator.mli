(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val fold:
  f:('a ->
     block:Block_hash.t -> pred:Block_hash.t ->
     step:int -> strict_step:bool -> 'a) ->
  'a -> Block_locator.t -> 'a
(** [map f l] applies [f] to each block of the locator, the last one
    excepted. The function also receives the expected predecessor
    [pred] of the [block] after [step] steps, i.e. the next block in
    the locator. When [strict_step] is [true], then [step] is the
    exact number of predecessor to be followed before to found
    [pred]. Otherwise, it is only an upper bound. *)

type step = {
  block: Block_hash.t ;
  predecessor: Block_hash.t ;
  step: int ;
  strict_step: bool ;
}
(** A 'step' in a locator is a couple of consecutives hashes in the
    locator, and the expected difference of level the two blocks (or
    an upper bounds when [strict_step = false]). *)

val to_steps: Block_locator.t -> step list
(** Build all the 'steps' composing the locator, starting with the
    oldest one (typically the predecessor of the first step will be
    `genesis`). All steps contains [strict_step = true], except the
    first one. *)

val estimated_length: Block_locator.t -> int
(** [estimated_length locator] estimate the length of the chain
    represented by [locator]. *)

val known_ancestor:
  State.Chain.t -> Block_locator.t -> (State.Block.t * Block_locator.t) option Lwt.t
(** [known_ancestor chain_state locator] computes the first block of
    [locator] that is known to be a valid block. It also computes the
    'prefix' of [locator] with end at the first valid block.  The
    function returns [None] when no block in the locator are known or
    if the first known block is invalid. *)

val find_new:
  State.Chain.t -> Block_locator.t -> int -> Block_hash.t list Lwt.t
(** [find_new chain locator max_length] returns the blocks from our
    current branch that would be unknown to a peer that sends us the
    [locator]. *)

