(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

val forge_block_header:
  #simple ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

module S : sig

  val forge_block_header:
    ([ `POST ], unit,
     unit, unit, Block_header.t,
     MBytes.t) RPC_service.t

end
