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
module Services = Node_rpc_services

let errors cctxt =
  call_service0 cctxt Services.Error.service ()

let forge_block_header cctxt header =
  call_service0 cctxt Services.forge_block_header header

let inject_block cctxt ?(async = false) ?(force = false) raw operations =
  call_err_service0 cctxt Services.inject_block
    { raw ; blocking = not async ; force ; operations }

let inject_operation cctxt ?(async = false) ?force ?net_id operation =
  call_err_service0 cctxt Services.inject_operation
    (operation, not async, net_id, force)

let inject_protocol cctxt ?(async = false) ?force protocol =
  call_err_service0 cctxt Services.inject_protocol
    (protocol, not async, force)

let bootstrapped cctxt =
  call_streamed_service0 cctxt Services.bootstrapped ()

let complete cctxt ?block prefix =
  match block with
  | None ->
      call_service1 cctxt Services.complete prefix ()
  | Some block ->
      call_service2 cctxt Services.Blocks.complete block prefix ()

let describe config ?recurse path =
  call_describe0 config Services.describe path recurse

module Blocks = struct

  type block = Services.Blocks.block

  type block_info = Services.Blocks.block_info = {
    hash: Block_hash.t ;
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ; (* uint8 *)
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
    data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_network: Context.test_network;
  }
  type preapply_param = Services.Blocks.preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: Operation.t list ;
    sort_operations: bool ;
  }
  type preapply_result = Services.Blocks.preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Prevalidation.preapply_result ;
  }
  let net_id cctxt h =
    call_service1 cctxt Services.Blocks.net_id h ()
  let level cctxt h =
    call_service1 cctxt Services.Blocks.level h ()
  let predecessor cctxt h =
    call_service1 cctxt Services.Blocks.predecessor h ()
  let predecessors cctxt h l =
    call_service1 cctxt Services.Blocks.predecessors h l
  let hash cctxt h =
    call_service1 cctxt Services.Blocks.hash h ()
  let timestamp cctxt h =
    call_service1 cctxt Services.Blocks.timestamp h ()
  let fitness cctxt h =
    call_service1 cctxt Services.Blocks.fitness h ()
  let operations cctxt ?(contents = false) h =
    call_service1 cctxt Services.Blocks.operations h
      { contents ; monitor = false }
  let protocol cctxt h =
    call_service1 cctxt Services.Blocks.protocol h ()
  let test_network cctxt h =
    call_service1 cctxt Services.Blocks.test_network h ()

  let preapply cctxt h
      ?(timestamp = Time.now ()) ?(sort = false) ~proto_header operations =
    call_err_service1
      cctxt Services.Blocks.preapply h
      { timestamp ; proto_header ; sort_operations = sort ; operations }
  let pending_operations cctxt block =
    call_service1 cctxt Services.Blocks.pending_operations block ()
  let info cctxt ?(include_ops = true) h =
    call_service1 cctxt Services.Blocks.info h include_ops
  let complete cctxt block prefix =
    call_service2 cctxt Services.Blocks.complete block prefix ()
  let list cctxt ?(include_ops = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_service0 cctxt Services.Blocks.list
      { include_ops ; length ; heads ; monitor = Some false ; delay ;
        min_date ; min_heads }
  let monitor cctxt ?(include_ops = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_streamed_service0 cctxt Services.Blocks.list
      { include_ops ; length ; heads ; monitor = Some true ; delay ;
        min_date ; min_heads }

end

module Operations = struct

  let monitor cctxt ?(contents = false) () =
    call_streamed_service1 cctxt Services.Blocks.operations
      `Prevalidation
      { contents ; monitor = true }

end

module Protocols = struct

  let contents cctxt hash =
    call_service1 cctxt Services.Protocols.contents hash ()

  let list cctxt ?contents () =
    call_service0
      cctxt Services.Protocols.list
      { contents; monitor = Some false }

end

module Network = struct

  let stat cctxt =
    call_service0 cctxt Services.Network.stat ()

  let connections cctxt =
    call_service0 cctxt Services.Network.Connection.list ()

  let peers cctxt =
    call_service0 cctxt Services.Network.Peer_id.list []

  let points cctxt =
    call_service0 cctxt Services.Network.Point.list []

end
