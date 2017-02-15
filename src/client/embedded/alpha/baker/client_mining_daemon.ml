(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Client.Mining

let run cctxt ?max_priority ~delay ?min_date delegates =
  (* TODO really detach... *)
  let endorsement =
    if Client_proto_args.Daemon.(!all || !endorsement) then
      Client_mining_blocks.monitor
        cctxt ?min_date ~min_heads:1 () >>= fun block_stream ->
      Client_mining_endorsement.create cctxt ~delay delegates block_stream
    else
      Lwt.return_unit
  in
  let denunciation =
    if Client_proto_args.Daemon.(!all || !denunciation) then
      Client_mining_operations.monitor_endorsement
        cctxt >>= fun endorsement_stream ->
      Client_mining_denunciation.create cctxt endorsement_stream
    else
      Lwt.return_unit
  in
  let forge =
    Client_mining_blocks.monitor
      cctxt ?min_date ~min_heads:1 () >>= fun block_stream ->
    Client_mining_operations.monitor_endorsement
      cctxt >>= fun endorsement_stream ->
    if Client_proto_args.Daemon.(!all || !mining) then
      Client_mining_forge.create cctxt
        ?max_priority delegates block_stream endorsement_stream
    else
      Lwt.return_unit
  in
  denunciation >>= fun () ->
  endorsement >>= fun () ->
  forge
