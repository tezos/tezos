(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let call_service1 s block a1 =
  Client_node_rpcs.call_service1
    (s Node_rpc_services.Blocks.proto_path) block a1
let call_error_service1 s block a1 =
  call_service1 s block a1 >|= wrap_error

let echo = call_service1 Services.echo_service
let failing = call_error_service1 Services.failing_service
