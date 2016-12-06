(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type RPC_CONTEXT = sig
  type root
end

module Make (RPC_context : RPC_CONTEXT) : sig
  val contracts :
    (RPC_context.root, RPC_context.root, unit, string list) RPC.service
  val hash :
    (RPC_context.root, RPC_context.root, unit, string) RPC.service
end
