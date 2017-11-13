(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell - Prevalidation of pending operations (a.k.a Mempool) *)

(** The prevalidation worker is in charge of the "mempool" (a.k.a. the
    set of known not-invalid-for-sure operations that are not yet
    included in the blockchain).

    The worker also maintains a sorted subset of the mempool that
    might correspond to a valid block on top of the current head. The
    "in-progress" context produced by the application of those
    operations is called the (pre)validation context.

    Before to include an operation into the mempool, the prevalidation
    worker tries to append the operation the prevalidation context. If
    the operation is (strongly) refused, it will not be added into the
    mempool and then it will be ignored by the node and never
    broadcasted. If the operation is only "branch_refused" or
    "branch_delayed", the operation won't be appended in the
    prevalidation context, but still broadcasted.

*)

type t

(** Creation and destruction of a "prevalidation" worker. *)
val create:
  operation_timeout: float ->
  Distributed_db.net_db -> t Lwt.t
val shutdown: t -> unit Lwt.t

val notify_operations: t -> P2p.Peer_id.t -> Mempool.t -> unit

(** Conditionnaly inject a new operation in the node: the operation will
    be ignored when it is (strongly) refused. This is the
    entry-point used by the P2P layer. The operation content has been
    previously stored on disk. *)
val inject_operation: t -> ?force:bool -> Operation.t -> unit tzresult Lwt.t

val flush: t -> State.Block.t -> unit
val timestamp: t -> Time.t
val operations: t -> error Prevalidation.preapply_result * Operation.t Operation_hash.Map.t
val context: t -> Updater.validation_result tzresult Lwt.t

val pending: ?block:State.Block.t -> t -> Operation.t Operation_hash.Map.t Lwt.t
