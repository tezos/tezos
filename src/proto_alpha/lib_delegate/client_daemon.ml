(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Endorser = struct

  let run (cctxt : #Proto_alpha.full) ~delay ?min_date delegates =
    Client_baking_blocks.monitor
      cctxt ?min_date ~min_heads:1 () >>=? fun block_stream ->
    Client_baking_endorsement.create cctxt ~delay delegates block_stream >>= fun () ->
    return ()

end

module Accuser = struct

  let run (cctxt : #Proto_alpha.full) =
    Client_baking_operations.monitor_endorsement
      cctxt >>=? fun endorsement_stream ->
    Client_baking_denunciation.create cctxt endorsement_stream >>= fun () ->
    return ()

end

module Baker = struct

  let run (cctxt : #Proto_alpha.full) ?max_priority ?min_date delegates =
    Client_baking_blocks.monitor
      cctxt ?min_date ~min_heads:1 () >>=? fun block_stream ->
    Client_baking_operations.monitor_endorsement
      cctxt >>=? fun endorsement_stream ->
    Client_baking_forge.create cctxt
      ?max_priority delegates block_stream endorsement_stream >>=? fun () ->
    return ()

end
