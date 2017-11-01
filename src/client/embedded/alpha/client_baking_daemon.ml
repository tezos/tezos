(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_commands
open Logging.Client.Mining

let run cctxt ?max_priority ~delay ?min_date delegates ~endorsement ~denunciation ~baking =
  (* TODO really detach... *)
  let endorsement =
    if endorsement then
      Client_baking_blocks.monitor
        cctxt.rpc_config ?min_date ~min_heads:1 () >>=? fun block_stream ->
      Client_baking_endorsement.create cctxt ~delay delegates block_stream >>= fun () ->
      return ()
    else
      return ()
  in
  let denunciation =
    if denunciation then
      Client_baking_operations.monitor_endorsement
        cctxt.rpc_config >>=? fun endorsement_stream ->
      Client_baking_denunciation.create cctxt endorsement_stream >>= fun () ->
      return ()
    else
      return ()
  in
  let forge =
    if baking then begin
      Client_baking_blocks.monitor
        cctxt.rpc_config ?min_date ~min_heads:1 () >>=? fun block_stream ->
      Client_baking_operations.monitor_endorsement
        cctxt.rpc_config >>=? fun endorsement_stream ->
      Client_baking_forge.create cctxt
        ?max_priority delegates block_stream endorsement_stream >>=? fun () ->
      return ()
    end else
      return ()
  in
  denunciation >>=? fun () ->
  endorsement >>=? fun () ->
  forge
