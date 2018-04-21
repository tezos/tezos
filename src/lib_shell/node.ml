(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Worker_logging

type t = {
  state: State.t ;
  distributed_db: Distributed_db.t ;
  validator: Validator.t ;
  mainchain_validator: Chain_validator.t ;
  p2p: Distributed_db.p2p ; (* For P2P RPCs *)
  shutdown: unit -> unit Lwt.t ;
}


let peer_metadata_cfg : _ P2p.peer_meta_config = {
  peer_meta_encoding = Peer_metadata.encoding ;
  peer_meta_initial = () ;
  score = fun _ -> 0. ;
}

let connection_metadata_cfg cfg : _ P2p.conn_meta_config = {
  conn_meta_encoding = Connection_metadata.encoding ;
  private_node = (fun { private_node } -> private_node) ;
  conn_meta_value = fun _ -> cfg;
}

let init_connection_metadata opt =
  let open Connection_metadata in
  match opt with
  | None ->
      { disable_mempool = false ;
        private_node = false }
  | Some c ->
      { disable_mempool = c.P2p.disable_mempool ;
        private_node = c.P2p.private_mode }

let init_p2p p2p_params =
  match p2p_params with
  | None ->
      let c_meta = init_connection_metadata None in
      lwt_log_notice "P2P layer is disabled" >>= fun () ->
      return (P2p.faked_network peer_metadata_cfg c_meta)
  | Some (config, limits) ->
      let c_meta = init_connection_metadata (Some config) in
      let conn_metadata_cfg = connection_metadata_cfg c_meta in
      lwt_log_notice "bootstraping chain..." >>= fun () ->
      P2p.create
        ~config ~limits
        peer_metadata_cfg
        conn_metadata_cfg
        Distributed_db_message.cfg >>=? fun p2p ->
      Lwt.async (fun () -> P2p.maintain p2p) ;
      return p2p

type config = {
  genesis: State.Chain.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_chain_max_tll: int option ;
}

and peer_validator_limits = Peer_validator.limits = {
  new_head_request_timeout: float ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  protocol_timeout: float ;
  worker_limits: Worker_types.limits
}

and prevalidator_limits = Prevalidator.limits = {
  max_refused_operations: int ;
  operation_timeout: float ;
  worker_limits : Worker_types.limits ;
}

and block_validator_limits = Block_validator.limits = {
  protocol_timeout: float ;
  worker_limits : Worker_types.limits ;
}

and chain_validator_limits = Chain_validator.limits = {
  bootstrap_threshold: int ;
  worker_limits : Worker_types.limits ;
}

let create { genesis ; store_root ; context_root ;
             patch_context ; p2p = p2p_params ;
             test_chain_max_tll = max_child_ttl }
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits =
  let start_prevalidator =
    match p2p_params with
    | Some (config, _limits) -> not config.P2p.disable_mempool
    | None -> true in
  init_p2p p2p_params >>=? fun p2p ->
  State.read
    ~store_root ~context_root ?patch_context genesis >>=? fun (state, mainchain_state) ->
  let distributed_db = Distributed_db.create state p2p in
  Validator.create state distributed_db
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits >>= fun validator ->
  Validator.activate validator
    ?max_child_ttl ~start_prevalidator mainchain_state >>= fun mainchain_validator ->
  let shutdown () =
    P2p.shutdown p2p >>= fun () ->
    Validator.shutdown validator >>= fun () ->
    State.close state >>= fun () ->
    Lwt.return_unit
  in
  return {
    state ;
    distributed_db ;
    validator ;
    mainchain_validator ;
    p2p ;
    shutdown ;
  }

let shutdown node = node.shutdown ()

let build_rpc_directory node =
  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let merge d = dir := RPC_directory.merge !dir d in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q) in

  merge (Protocol_directory.build_rpc_directory node.state) ;
  merge (Monitor_directory.build_rpc_directory
           node.validator node.mainchain_validator) ;
  merge Shell_directory.rpc_directory ;
  merge (Injection_directory.build_rpc_directory node.validator) ;
  merge (Chain_directory.build_rpc_directory node.validator) ;
  merge (P2p.build_rpc_directory node.p2p) ;
  merge Worker_directory.rpc_directory ;

  register0 RPC_service.error_service begin fun () () ->
    return (Data_encoding.Json.schema Error_monad.error_encoding)
  end ;

  RPC_directory.register_describe_directory_service
    !dir RPC_service.description_service
