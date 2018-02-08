(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
      Block_services.complete cctxt block prefix

let describe cctxt ?(recurse = true) path =
  Client_rpcs.call_service cctxt
    Shell_services.describe
    ((), path) { recurse } ()

module Protocols = struct

  let contents cctxt hash =
    call_service1 cctxt Protocol_services.contents hash ()

  let list cctxt ?contents () =
    call_service0
      cctxt Protocol_services.list
      { contents; monitor = Some false }

end
