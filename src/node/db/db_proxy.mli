(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type DISTRIBUTED_DB = sig
  type t
  type state
  type store
  type key
  type value
  val create: state -> store Persist.shared_ref -> t
  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value option Lwt.t
  val prefetch: t -> Store.net_id -> key -> unit
  val fetch: t -> Store.net_id -> key -> value Lwt.t
  val pending: t -> key -> bool
  val store: t -> key -> value -> bool Lwt.t
  val update: t -> key -> value -> bool Lwt.t
  val remove: t -> key -> bool Lwt.t
  val shutdown: t -> unit Lwt.t
end

type operation_state = {
  request_operations: Store.net_id -> Operation_hash.t list -> unit ;
}

module Operation :
  DISTRIBUTED_DB with type store := Store.Operation.t
                  and type key := Store.Operation.key
                  and type value := Store.Operation.value
                  and type state := operation_state

type block_state = {
  request_blocks: Store.net_id -> Block_hash.t list -> unit ;
}

module Block :
  DISTRIBUTED_DB with type store := Store.Block.t
                  and type key := Store.Block.key
                  and type value := Store.Block.value
                  and type state := block_state
