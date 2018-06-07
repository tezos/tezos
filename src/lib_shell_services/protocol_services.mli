(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

val contents:
  #simple -> Protocol_hash.t -> Protocol.t tzresult Lwt.t

val list:
  #simple ->
  Protocol_hash.t list tzresult Lwt.t

module S : sig

  val contents:
    ([ `GET ], unit,
     unit * Protocol_hash.t, unit, unit,
     Protocol.t) RPC_service.t

  val list:
    ([ `GET ], unit,
     unit, unit, unit,
     Protocol_hash.t list) RPC_service.t

end
