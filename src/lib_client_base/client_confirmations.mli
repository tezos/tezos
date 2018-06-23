(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** [wait_for_operation_inclusion chain ~predecessors ~confirmations
    oph] waits for `oph` to appears in the main chain with at least
    `confirmations`. It returns the hash of the block that contains
    the operation and the operation position in the block.

    This functions also looks for the operations in the `predecessors`
    of the intial chain head. *)
val wait_for_operation_inclusion:
  #Client_context.full ->
  chain:Chain_services.chain ->
  ?predecessors:int ->
  ?confirmations:int ->
  Operation_hash.t ->
  (Block_hash.t * int * int) tzresult Lwt.t

val wait_for_bootstrapped:
  #Client_context.full -> unit tzresult Lwt.t
