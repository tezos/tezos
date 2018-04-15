(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val get_chain_id: State.t -> Chain_services.chain -> Chain_id.t Lwt.t
val get_chain: State.t -> Chain_services.chain -> State.Chain.t Lwt.t

val rpc_directory: State.Chain.t Lwt.t RPC_directory.t

val build_rpc_directory: State.t -> Validator.t -> unit RPC_directory.t
