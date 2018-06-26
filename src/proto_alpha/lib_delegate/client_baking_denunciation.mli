(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val create:
  #Proto_alpha.full ->
  preserved_levels: int ->
  Client_baking_blocks.block_info tzresult Lwt_stream.t ->
  unit tzresult Lwt.t
