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
    Client_baking_blocks.monitor_heads
      cctxt `Main >>=? fun block_stream ->
    Client_baking_endorsement.create cctxt ~delay delegates block_stream >>= fun () ->
    ignore min_date;
    return ()

end

module Baker = struct

  let run (cctxt : #Proto_alpha.full) ?threshold ?max_priority ?min_date ~context_path delegates =
    Client_baking_blocks.monitor_heads
      cctxt `Main >>=? fun block_stream ->
    Client_baking_forge.create cctxt
      ?threshold ?max_priority ~context_path delegates block_stream >>=? fun () ->
    ignore min_date;
    return ()

end

module Accuser = struct

  let run (cctxt : #Proto_alpha.full) =
    Client_baking_operations.monitor_endorsement
      cctxt >>=? fun endorsement_stream ->
    Client_baking_denunciation.create cctxt endorsement_stream >>= fun () ->
    return ()

end
