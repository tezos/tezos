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
  ?contents:bool ->
  #simple ->
  (Protocol_hash.t * Protocol.t option) list tzresult Lwt.t

val monitor:
  ?contents:bool ->
  #streamed ->
  ((Protocol_hash.t * Protocol.t option) list Lwt_stream.t * stopper) tzresult Lwt.t

module S : sig

  val contents:
    ([ `POST ], unit,
     unit * Protocol_hash.t, unit, unit,
     Protocol.t) RPC_service.t

  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }

  val list:
    ([ `POST ], unit,
     unit, unit, list_param,
     (Protocol_hash.t * Protocol.t option) list) RPC_service.t

end
