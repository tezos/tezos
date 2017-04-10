(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

let forge_block cctxt ?net ?level ?predecessor ?timestamp fitness ops header =
  call_service0 cctxt Services.forge_block
    (net, level, predecessor, timestamp, fitness, ops, header)

let validate_block cctxt net block =
  call_err_service0 cctxt Services.validate_block (net, block)

let inject_block cctxt ?(async = false) ?(force = false) raw operations =
  call_err_service0 cctxt Services.inject_block
    { raw ; blocking = not async ; force ; operations }

let inject_operation cctxt ?(async = false) ?force operation =
  call_err_service0 cctxt Services.inject_operation
    (operation, not async, force)

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
    level: Int32.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations_hash: Operation_list_list_hash.t ;
    operations: Operation_hash.t list list option ;
    data: MBytes.t option ;
    net: Net_id.t ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Net_id.t * Time.t) option ;
  }
  type preapply_param = Services.Blocks.preapply_param = {
    operations: Operation_hash.t list ;
    sort: bool ;
    timestamp: Time.t option ;
  }
  type preapply_result = Services.Blocks.preapply_result = {
    operations: error Prevalidation.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }
  let net cctxt h =
    call_service1 cctxt Services.Blocks.net h ()
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
  let operations cctxt h =
    call_service1 cctxt Services.Blocks.operations h ()
  let protocol cctxt h =
    call_service1 cctxt Services.Blocks.protocol h ()
  let test_protocol cctxt h =
    call_service1 cctxt Services.Blocks.test_protocol h ()
  let test_network cctxt h =
    call_service1 cctxt Services.Blocks.test_network h ()

  let preapply cctxt h ?timestamp ?(sort = false) operations =
    call_err_service1
      cctxt Services.Blocks.preapply h
      { operations ; sort ; timestamp }
  let pending_operations cctxt block =
    call_service1 cctxt Services.Blocks.pending_operations block ()
  let info cctxt ?(operations = true) ?(data = true) h =
    call_service1 cctxt Services.Blocks.info h (operations, data)
  let complete cctxt block prefix =
    call_service2 cctxt Services.Blocks.complete block prefix ()
  let list cctxt ?(operations = false) ?(data = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_service0 cctxt Services.Blocks.list
      { operations ; data ; length ; heads ; monitor = Some false ; delay ;
        min_date ; min_heads }
  let monitor cctxt ?(operations = false) ?(data = false)
      ?length ?heads ?delay ?min_date ?min_heads () =
    call_streamed_service0 cctxt Services.Blocks.list
      { operations ; data ; length ; heads ; monitor = Some true ; delay ;
        min_date ; min_heads }

end

module Operations = struct

  let contents cctxt hashes =
    call_service1 cctxt Services.Operations.contents hashes ()

  let monitor cctxt ?contents () =
    call_streamed_service0 cctxt Services.Operations.list
      { monitor = Some true ; contents }

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
