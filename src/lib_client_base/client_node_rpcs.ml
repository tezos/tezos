(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - RPC Calls *)

open Client_rpcs

let errors (rpc : #Client_rpcs.ctxt) =
  call_service0 rpc RPC_error.service ()

let forge_block_header rpc header =
  call_service0 rpc Shell_services.forge_block_header header

let inject_block cctxt
    ?(async = false) ?(force = false) ?net_id
    raw operations =
  call_err_service0 cctxt Shell_services.inject_block
    { raw ; blocking = not async ; force ; net_id ; operations }

let inject_operation cctxt ?(async = false) ?net_id operation =
  call_err_service0 cctxt Shell_services.inject_operation
    (operation, not async, net_id)

let inject_protocol cctxt ?(async = false) ?force protocol =
  call_err_service0 cctxt Shell_services.inject_protocol
    (protocol, not async, force)

let bootstrapped cctxt =
  call_streamed_service0 cctxt Shell_services.bootstrapped ()

let complete cctxt ?block prefix =
  match block with
  | None ->
      call_service1 cctxt Shell_services.complete prefix ()
  | Some block ->
      call_service2 cctxt Block_services.complete block prefix ()

let describe cctxt ?(recurse = true) path =
  Client_rpcs.call_service cctxt
    Shell_services.describe
    ((), path) { recurse } ()

module Blocks = struct

  type block = Block_services.block

  type block_info = Block_services.block_info = {
    hash: Block_hash.t ;
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ; (* uint8 *)
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
    context: Context_hash.t ;
    data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_network: Test_network_status.t;
  }
  type preapply_param = Block_services.preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: Operation.t list list ;
    sort_operations: bool ;
  }
  type preapply_result = Block_services.preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Preapply_result.t list ;
  }
  let net_id cctxt h =
    call_service1 cctxt Block_services.net_id h ()
  let level cctxt h =
    call_service1 cctxt Block_services.level h ()
  let predecessor cctxt h =
    call_service1 cctxt Block_services.predecessor h ()
  let predecessors cctxt h l =
    call_service1 cctxt Block_services.predecessors h l
  let hash cctxt h =
    call_service1 cctxt Block_services.hash h ()
  let timestamp cctxt h =
    call_service1 cctxt Block_services.timestamp h ()
  let fitness cctxt h =
    call_service1 cctxt Block_services.fitness h ()
  let operations cctxt ?(contents = false) h =
    call_service1 cctxt Block_services.operations h
      { contents ; monitor = false }
  let protocol cctxt h =
    call_service1 cctxt Block_services.protocol h ()
  let test_network cctxt h =
    call_service1 cctxt Block_services.test_network h ()

  let preapply cctxt h
      ?(timestamp = Time.now ()) ?(sort = false) ~proto_header operations =
    call_err_service1
      cctxt Block_services.preapply h
      { timestamp ; proto_header ; sort_operations = sort ; operations }
  let pending_operations cctxt block =
    call_service1 cctxt Block_services.pending_operations block ()
  let info cctxt ?(include_ops = true) h =
    call_service1 cctxt Block_services.info h include_ops
  let complete cctxt block prefix =
    call_service2 cctxt Block_services.complete block prefix ()
  let list cctxt ?(include_ops = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_service0 cctxt Block_services.list
      { include_ops ; length ; heads ; monitor = Some false ; delay ;
        min_date ; min_heads }
  let monitor cctxt ?(include_ops = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_streamed_service0 cctxt Block_services.list
      { include_ops ; length ; heads ; monitor = Some true ; delay ;
        min_date ; min_heads }

end

module Operations = struct

  let monitor cctxt ?(contents = false) () =
    call_streamed_service1 cctxt Block_services.operations
      `Prevalidation
      { contents ; monitor = true }

end

module Protocols = struct

  let contents cctxt hash =
    call_service1 cctxt Protocol_services.contents hash ()

  let list cctxt ?contents () =
    call_service0
      cctxt Protocol_services.list
      { contents; monitor = Some false }

end

module Network = struct

  let stat cctxt =
    call_service0 cctxt P2p_services.stat ()

  let connections cctxt =
    call_service0 cctxt P2p_services.Connection.list ()

  let peers cctxt =
    call_service0 cctxt P2p_services.Peer_id.list []

  let points cctxt =
    call_service0 cctxt P2p_services.Point.list []

end
