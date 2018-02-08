(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
   unit tzresult) RPC_service.t

module Connection : sig

  val list :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_connection.Info.t list) RPC_service.t

  val info :
    ([ `POST ], unit,
     unit * P2p_peer.Id.t, unit, unit,
     P2p_connection.Info.t option) RPC_service.t

  val kick :
    ([ `POST ], unit,
     unit * P2p_peer.Id.t, unit, bool,
     unit) RPC_service.t

end

module Point : sig
  val list :
    ([ `POST ], unit,
     unit, unit, P2p_point.State.t list,
     (P2p_point.Id.t * P2p_point.Info.t) list) RPC_service.t
  val info :
    ([ `POST ], unit,
     unit * P2p_point.Id.t, unit, unit,
     P2p_point.Info.t option) RPC_service.t
  val events :
    ([ `POST ], unit,
     unit * P2p_point.Id.t, unit, bool,
     P2p_point.Pool_event.t list) RPC_service.t
end

module Peer_id : sig

  val list :
    ([ `POST ], unit,
     unit, unit, P2p_peer.State.t list,
     (P2p_peer.Id.t * P2p_peer.Info.t) list) RPC_service.t

  val info :
    ([ `POST ], unit,
     unit * P2p_peer.Id.t, unit, unit,
     P2p_peer.Info.t option) RPC_service.t

  val events :
    ([ `POST ], unit,
     unit * P2p_peer.Id.t, unit, bool,
     P2p_peer.Pool_event.t list) RPC_service.t

end
