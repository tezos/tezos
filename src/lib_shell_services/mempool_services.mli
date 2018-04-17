(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


module S : sig
  val pending_operations:
    ([ `POST ], unit,
     unit , unit, unit,
     error Preapply_result.t * Operation.t Operation_hash.Map.t) RPC_service.t
end

open RPC_context

val pending_operations:
  #simple ->
  (error Preapply_result.t * Operation.t Operation_hash.Map.t) tzresult Lwt.t
