(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

val stat :
  ([ `POST ], unit,
   unit, unit, unit,
   Stat.t, unit) RPC_service.t

val versions :
  ([ `POST ], unit,
   unit, unit, unit,
   Version.t list, unit) RPC_service.t

val events :
  ([ `POST ], unit,
   unit, unit, unit,
   Connection_pool_log_event.t, unit) RPC_service.t

val connect :
  ([ `POST ], unit,
   unit * Point.t, unit, float,
   unit tzresult, unit) RPC_service.t

module Connection : sig

  val list :
    ([ `POST ], unit,
     unit, unit, unit,
     Connection_info.t list, unit) RPC_service.t

  val info :
    ([ `POST ], unit,
     unit * Peer_id.t, unit, unit,
     Connection_info.t option, unit) RPC_service.t

  val kick :
    ([ `POST ], unit,
     unit * Peer_id.t, unit, bool,
     unit, unit) RPC_service.t

end

module Point : sig
  val list :
    ([ `POST ], unit,
     unit, unit, Point_state.t list,
     (Point.t * Point_info.t) list, unit) RPC_service.t
  val info :
    ([ `POST ], unit,
     unit * Point.t, unit, unit,
     Point_info.t option, unit) RPC_service.t
  val events :
    ([ `POST ], unit,
     unit * Point.t, unit, bool,
     P2p_connection_pool_types.Point_info.Event.t list, unit) RPC_service.t
end

module Peer_id : sig

  val list :
    ([ `POST ], unit,
     unit, unit, Peer_state.t list,
     (Peer_id.t * Peer_info.t) list, unit) RPC_service.t

  val info :
    ([ `POST ], unit,
     unit * Peer_id.t, unit, unit,
     Peer_info.t option, unit) RPC_service.t

  val events :
    ([ `POST ], unit,
     unit * Peer_id.t, unit, bool,
     P2p_connection_pool_types.Peer_info.Event.t list, unit) RPC_service.t

end
