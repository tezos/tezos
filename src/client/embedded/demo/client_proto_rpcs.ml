(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let call_service1 cctxt s block a1 =
  Client_rpcs.call_service1 cctxt
    (s Node_rpc_services.Blocks.proto_path) block a1
let call_error_service1 cctxt s block a1 =
  call_service1 cctxt s block a1 >>= function
  | Ok (Error _ as err) -> Lwt.return (Environment.wrap_error err)
  | Ok (Ok v) -> return v
  | Error _ as err -> Lwt.return err

let echo cctxt = call_service1 cctxt Services.echo_service
let failing cctxt = call_error_service1 cctxt Services.failing_service
