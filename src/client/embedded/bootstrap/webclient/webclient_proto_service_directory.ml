(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Services = Webclient_proto_services.Make (struct
    type root = Node_rpc_services.Blocks.block
  end)

let cctxt = Client_commands.ignore_context

let root =
  let root =
    RPC.register RPC.empty Services.contracts @@ fun _block () ->
    Client_proto_contracts.RawContractAlias.load cctxt >>= fun list ->
    let (names, _) = List.split list in
    RPC.Answer.return names in
  let root =
    RPC.register root Services.hash @@ fun block () ->
    Client_node_rpcs.(call_service1 cctxt Node_rpc_services.Blocks.hash block ()) >>= fun res ->
    RPC.Answer.return (Hash.Block_hash.to_b48check res) in
  root
