(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type t
type incremental = t

val predecessor: incremental -> Block.t
val header: incremental -> Block_header.t

val level: incremental -> int32

val begin_construction:
  ?priority:int ->
  ?timestamp:Time.t ->
  Block.t -> incremental tzresult Lwt.t

val add_operation:
  ?allow_failure:bool ->
  incremental -> Operation.packed -> incremental tzresult Lwt.t

val finalize_block: incremental -> Block.t tzresult Lwt.t

val rpc_ctxt: incremental Alpha_environment.RPC_context.simple
