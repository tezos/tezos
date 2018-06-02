(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type config = {
  genesis: State.Chain.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_chain_max_tll: int option ;
  checkpoint: (Int32.t * Block_hash.t) option ;
}

and peer_validator_limits = {
  new_head_request_timeout: float ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  protocol_timeout: float ;
  worker_limits: Worker_types.limits
}
and prevalidator_limits = {
  max_refused_operations: int ;
  operation_timeout: float ;
  worker_limits : Worker_types.limits ;
}
and block_validator_limits = {
  protocol_timeout: float ;
  worker_limits : Worker_types.limits ;
}
and chain_validator_limits = {
  bootstrap_threshold: int ;
  worker_limits : Worker_types.limits ;
}

val default_peer_validator_limits: peer_validator_limits
val default_prevalidator_limits: prevalidator_limits
val default_block_validator_limits: block_validator_limits
val default_chain_validator_limits: chain_validator_limits

val create:
  config ->
  peer_validator_limits ->
  block_validator_limits ->
  prevalidator_limits ->
  chain_validator_limits ->
  t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val build_rpc_directory: t -> unit RPC_directory.t
