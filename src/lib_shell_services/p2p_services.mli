(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

val stat: #simple -> P2p_stat.t tzresult Lwt.t

val versions: #simple -> P2p_version.t list tzresult Lwt.t

val events: #streamed ->
  (P2p_connection.Pool_event.t Lwt_stream.t * stopper) tzresult Lwt.t

val connect: #simple -> timeout:float -> P2p_point.Id.t -> unit tzresult Lwt.t

module S : sig

  val stat :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_stat.t) RPC_service.t

  val versions :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_version.t list) RPC_service.t

  val events :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_connection.Pool_event.t) RPC_service.t

  val connect :
    ([ `POST ], unit,
     unit * P2p_point.Id.t, unit, float,
     unit) RPC_service.t

end

module Connections : sig

  open RPC_context

  val list: #simple -> P2p_connection.Info.t list tzresult Lwt.t

  val info: #simple -> P2p_peer.Id.t -> P2p_connection.Info.t tzresult Lwt.t

  val kick: #simple -> ?wait:bool -> P2p_peer.Id.t -> unit tzresult Lwt.t

  module S : sig

    val list :
      ([ `POST ], unit,
       unit, unit, unit,
       P2p_connection.Info.t list) RPC_service.t

    val info :
      ([ `POST ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       P2p_connection.Info.t) RPC_service.t

    val kick :
      ([ `POST ], unit,
       unit * P2p_peer.Id.t, unit, bool,
       unit) RPC_service.t

  end

end


module Points : sig

  val list:
    ?filter:(P2p_point.State.t list) ->
    #simple -> (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

  val info: #simple -> P2p_point.Id.t -> P2p_point.Info.t tzresult Lwt.t

  val events:
    #streamed ->
    P2p_point.Id.t ->
    (P2p_point.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  module S : sig

    val list :
      ([ `POST ], unit,
       unit, unit, P2p_point.State.t list,
       (P2p_point.Id.t * P2p_point.Info.t) list) RPC_service.t

    val info :
      ([ `POST ], unit,
       unit * P2p_point.Id.t, unit, unit,
       P2p_point.Info.t) RPC_service.t

    val events :
      ([ `POST ], unit,
       unit * P2p_point.Id.t, unit, bool,
       P2p_point.Pool_event.t list) RPC_service.t

  end

end


module Peers : sig

  val list:
    ?filter:(P2p_peer.State.t list) ->
    #simple ->
    (P2p_peer.Id.t * P2p_peer.Info.t) list tzresult Lwt.t

  val info: #simple -> P2p_peer.Id.t -> P2p_peer.Info.t tzresult Lwt.t

  val events:
    #streamed -> P2p_peer.Id.t ->
    (P2p_peer.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  module S : sig

    val list :
      ([ `POST ], unit,
       unit, unit, P2p_peer.State.t list,
       (P2p_peer.Id.t * P2p_peer.Info.t) list) RPC_service.t

    val info :
      ([ `POST ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       P2p_peer.Info.t) RPC_service.t

    val events :
      ([ `POST ], unit,
       unit * P2p_peer.Id.t, unit, bool,
       P2p_peer.Pool_event.t list) RPC_service.t

  end

end
