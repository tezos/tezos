(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2019 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

[@@@ocaml.warning "-30"]

open Lwt.Infix
open Tezos_base

module Initialization_event = struct
  type t = {
    time_stamp : float ;
    status : [ `P2p_layer_disabled | `Bootstrapping | `P2p_maintain_started ] ;
  }
  let status_names = [
    "p2p_layer_disabled", `P2p_layer_disabled ;
    "bootstrapping", `Bootstrapping ;
    "p2p_maintain_started", `P2p_maintain_started ;
  ]
  module Definition = struct
    let name = "shell-node"
    type nonrec t = t
    let encoding =
      let open Data_encoding in
      let v0_encoding =
        conv
          (function { time_stamp ; status } -> time_stamp, status)
          (fun (time_stamp, status) -> { time_stamp ; status } )
          (obj2
             (req "time-stamp" float)
             (req "status"
                (string_enum status_names))) in
      With_version.(encoding ~name (first_version v0_encoding))
    let pp ppf { status ; _ } =
      Format.fprintf ppf "%s initialization: %s"
        name (List.find (fun (_, s) -> s = status) status_names |> fst)
    let doc = "Status of the initialization of the P2P layer."
    let level _ = Internal_event.Notice
  end
  module Event = Internal_event.Make(Definition)
  let lwt_emit status =
    let time_stamp = Unix.gettimeofday () in
    Event.emit (fun () -> { time_stamp ; status }) >>= function
    | Ok () -> Lwt.return_unit
    | Error el ->
        Format.kasprintf Lwt.fail_with "Initialization_event.emit: %a"
          pp_print_error el
end


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
  peer_meta_initial = Peer_metadata.empty ;
  score = Peer_metadata.score ;
}

let connection_metadata_cfg cfg : _ P2p.conn_meta_config = {
  conn_meta_encoding = Connection_metadata.encoding ;
  private_node = (fun { private_node ; _ } -> private_node) ;
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

let init_p2p ?(sandboxed = false) p2p_params =
  match p2p_params with
  | None ->
      let c_meta = init_connection_metadata None in
      Initialization_event.lwt_emit `P2p_layer_disabled >>= fun () ->
      return (P2p.faked_network Distributed_db_message.cfg peer_metadata_cfg c_meta)
  | Some (config, limits) ->
      let c_meta = init_connection_metadata (Some config) in
      let conn_metadata_cfg = connection_metadata_cfg c_meta in
      Initialization_event.lwt_emit `Bootstrapping >>= fun () ->
      let message_cfg =
        if sandboxed then
          { Distributed_db_message.cfg with
            chain_name = Distributed_db_version.sandboxed_chain_name }
        else
          Distributed_db_message.cfg in
      P2p.create
        ~config ~limits
        peer_metadata_cfg
        conn_metadata_cfg
        message_cfg >>=? fun p2p ->
      Lwt.async (fun () -> P2p.maintain p2p) ;
      Initialization_event.lwt_emit `P2p_maintain_started >>= fun () ->
      return p2p

type config = {
  genesis: State.Chain.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_chain_max_tll: int option ;
  checkpoint: (Int32.t * Block_hash.t) option ;
}

and peer_validator_limits = Peer_validator.limits = {
  new_head_request_timeout: Time.System.Span.t ;
  block_header_timeout: Time.System.Span.t ;
  block_operations_timeout: Time.System.Span.t ;
  protocol_timeout: Time.System.Span.t ;
  worker_limits: Worker_types.limits
}

and prevalidator_limits = Prevalidator.limits = {
  max_refused_operations: int ;
  operation_timeout: Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}

and block_validator_limits = Block_validator.limits = {
  protocol_timeout: Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}

and chain_validator_limits = Chain_validator.limits = {
  bootstrap_threshold: int ;
  worker_limits : Worker_types.limits ;
}

let default_block_validator_limits = {
  protocol_timeout = Time.System.Span.of_seconds_exn 120. ;
  worker_limits = {
    backlog_size = 1000 ;
    backlog_level = Internal_event.Debug ;
  }
}
let default_prevalidator_limits = {
  operation_timeout = Time.System.Span.of_seconds_exn 10. ;
  max_refused_operations = 1000 ;
  worker_limits = {
    backlog_size = 1000 ;
    backlog_level = Internal_event.Info ;
  }
}
let default_peer_validator_limits = {
  block_header_timeout = Time.System.Span.of_seconds_exn 60. ;
  block_operations_timeout = Time.System.Span.of_seconds_exn 60. ;
  protocol_timeout = Time.System.Span.of_seconds_exn 120. ;
  new_head_request_timeout = Time.System.Span.of_seconds_exn 90. ;
  worker_limits = {
    backlog_size = 1000 ;
    backlog_level = Internal_event.Info ;
  }
}
let default_chain_validator_limits = {
  bootstrap_threshold = 4 ;
  worker_limits = {
    backlog_size = 1000 ;
    backlog_level = Internal_event.Info ;
  }
}

let may_update_checkpoint chain_state checkpoint =
  match checkpoint with
  | None ->
      Lwt.return_unit
  | Some checkpoint ->
      State.best_known_head_for_checkpoint
        chain_state checkpoint >>= fun new_head ->
      Chain.set_head chain_state new_head >>= fun _old_head ->
      State.Chain.set_checkpoint chain_state checkpoint

module Local_logging =
  Internal_event.Legacy_logging.Make_semantic
    (struct let name = "node.worker" end)

let store_known_protocols state =
  let open Local_logging in
  let embedded_protocols = Registered_protocol.list_embedded () in
  Lwt_list.iter_s
    (fun protocol_hash ->
       State.Protocol.known state protocol_hash >>= function
       | true ->
           lwt_log_info Tag.DSL.(fun f ->
               f "protocol %a is already in store: nothing to do"
               -% a Protocol_hash.Logging.tag protocol_hash
               -% t event "embedded_protocol_already_stored")
       | false ->
           match Registered_protocol.get_embedded_sources protocol_hash with
           | None ->
               lwt_log_info Tag.DSL.(fun f ->
                   f "protocol %a won't be stored: missing source files"
                   -% a Protocol_hash.Logging.tag protocol_hash
                   -% t event "embedded_protocol_missing_sources"
                 )
           | Some protocol ->
               let hash = Protocol.hash protocol in
               if not (Protocol_hash.equal hash protocol_hash) then
                 lwt_log_info Tag.DSL.(fun f ->
                     f "protocol %a won't be stored: wrong hash"
                     -% a Protocol_hash.Logging.tag protocol_hash
                     -% t event "embedded_protocol_inconsistent_hash")
               else
                 State.Protocol.store state protocol >>= function
                 | Some hash' ->
                     assert (hash = hash') ;
                     lwt_log_info Tag.DSL.(fun f ->
                         f "protocol %a successfully stored"
                         -% a Protocol_hash.Logging.tag protocol_hash
                         -% t event "embedded_protocol_stored")
                 | None ->
                     lwt_log_info Tag.DSL.(fun f ->
                         f "protocol %a is already in store: nothing to do"
                         -% a Protocol_hash.Logging.tag protocol_hash
                         -% t event "embedded_protocol_already_stored")
    ) embedded_protocols

let create
    ?(sandboxed = false)
    { genesis ; store_root ; context_root ;
      patch_context ; p2p = p2p_params ;
      test_chain_max_tll = max_child_ttl ;
      checkpoint }
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits =
  let (start_prevalidator, start_testchain) =
    match p2p_params with
    | Some (config, _limits) -> not config.P2p.disable_mempool, not config.P2p.disable_testchain
    | None -> true, true in
  init_p2p ~sandboxed p2p_params >>=? fun p2p ->
  State.init
    ~store_root ~context_root ?patch_context
    genesis >>=? fun (state, mainchain_state, context_index) ->
  may_update_checkpoint mainchain_state checkpoint >>= fun () ->
  let distributed_db = Distributed_db.create state p2p in
  store_known_protocols state >>= fun () ->
  Validator.create state distributed_db
    peer_validator_limits
    block_validator_limits
    (Block_validator.Internal context_index)
    prevalidator_limits
    chain_validator_limits
    ~start_testchain
  >>=? fun validator ->
  (* TODO : Check that the testchain is correctly activated after a node restart *)
  Validator.activate validator
    ?max_child_ttl ~start_prevalidator
    mainchain_state >>=? fun mainchain_validator ->
  let shutdown () =
    P2p.shutdown p2p >>= fun () ->
    Distributed_db.shutdown distributed_db >>= fun () ->
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

  merge (Protocol_directory.build_rpc_directory
           (Block_validator.running_worker ()) node.state) ;
  merge (Monitor_directory.build_rpc_directory
           node.validator node.mainchain_validator) ;
  merge (Injection_directory.build_rpc_directory node.validator) ;
  merge (Chain_directory.build_rpc_directory node.validator) ;
  merge (P2p.build_rpc_directory node.p2p) ;
  merge (Worker_directory.build_rpc_directory node.state) ;

  merge (Stat_directory.rpc_directory ()) ;

  register0 RPC_service.error_service begin fun () () ->
    return (Data_encoding.Json.schema Error_monad.error_encoding)
  end ;

  RPC_directory.register_describe_directory_service
    !dir RPC_service.description_service
