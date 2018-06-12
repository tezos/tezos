(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_stdlib.Logging.Make_semantic(struct let name = "node.state" end)

let chain_id = Tag.def ~doc:"Chain ID" "chain_id" Chain_id.pp
