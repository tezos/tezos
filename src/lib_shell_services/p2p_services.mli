(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open RPC_context

val self: #simple -> P2p_peer.Id.t tzresult Lwt.t

val stat: #simple -> P2p_stat.t tzresult Lwt.t

val versions: #simple -> Network_version.t list tzresult Lwt.t

val events: #streamed ->
  (P2p_connection.Pool_event.t Lwt_stream.t * stopper) tzresult Lwt.t

val connect: #simple -> timeout:Ptime.Span.t -> P2p_point.Id.t -> unit tzresult Lwt.t

module S : sig

  val self :
    ([ `GET ], unit,
     unit, unit, unit,
     P2p_peer.Id.t) RPC_service.t

  val stat :
    ([ `GET ], unit,
     unit, unit, unit,
     P2p_stat.t) RPC_service.t

  val versions :
    ([ `GET ], unit,
     unit, unit, unit,
     Network_version.t list) RPC_service.t

  val events :
    ([ `GET ], unit,
     unit, unit, unit,
     P2p_connection.Pool_event.t) RPC_service.t

  val connect :
    ([ `PUT ], unit,
     unit * P2p_point.Id.t, < timeout: Ptime.Span.t >, unit,
     unit) RPC_service.t

end

module Connections : sig

  open RPC_context

  type connection_info = Connection_metadata.t P2p_connection.Info.t

  val list: #simple -> connection_info list tzresult Lwt.t

  val info: #simple -> P2p_peer.Id.t -> connection_info tzresult Lwt.t

  val kick: #simple -> ?wait:bool -> P2p_peer.Id.t -> unit tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ], unit,
       unit, unit, unit,
       connection_info list) RPC_service.t

    val info :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       connection_info) RPC_service.t

    val kick :
      ([ `DELETE ], unit,
       unit * P2p_peer.Id.t, < wait: bool >, unit,
       unit) RPC_service.t

  end

end


module Points : sig

  val list:
    ?filter:(P2p_point.Filter.t list) ->
    #simple -> (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

  val info: #simple -> P2p_point.Id.t -> P2p_point.Info.t tzresult Lwt.t

  val events:
    #streamed ->
    P2p_point.Id.t ->
    (P2p_point.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  val ban: #simple -> P2p_point.Id.t -> unit tzresult Lwt.t

  val unban: #simple -> P2p_point.Id.t -> unit tzresult Lwt.t

  val trust: #simple -> P2p_point.Id.t -> unit tzresult Lwt.t

  val untrust: #simple -> P2p_point.Id.t -> unit tzresult Lwt.t

  val banned: #simple -> P2p_point.Id.t -> bool tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ], unit,
       unit, < filters: P2p_point.Filter.t list >, unit,
       (P2p_point.Id.t * P2p_point.Info.t) list) RPC_service.t

    val info :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       P2p_point.Info.t) RPC_service.t

    val events :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, < monitor: bool>, unit,
       P2p_point.Pool_event.t list) RPC_service.t

    val ban :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       unit) RPC_service.t

    val unban :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       unit) RPC_service.t

    val trust :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       unit) RPC_service.t

    val untrust :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       unit) RPC_service.t

    val banned :
      ([ `GET ], unit,
       unit * P2p_point.Id.t, unit, unit,
       bool) RPC_service.t

  end

end

module Peers : sig

  val list:
    ?filter:(P2p_peer.Filter.t list) ->
    #simple ->
    (P2p_peer.Id.t * (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t) list tzresult Lwt.t

  val info:
    #simple -> P2p_peer.Id.t ->
    (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t tzresult Lwt.t

  val events:
    #streamed -> P2p_peer.Id.t ->
    (P2p_peer.Pool_event.t list Lwt_stream.t * stopper) tzresult Lwt.t

  val ban: #simple -> P2p_peer.Id.t -> unit tzresult Lwt.t

  val unban: #simple -> P2p_peer.Id.t -> unit tzresult Lwt.t

  val trust: #simple -> P2p_peer.Id.t -> unit tzresult Lwt.t

  val untrust: #simple -> P2p_peer.Id.t -> unit tzresult Lwt.t

  val banned: #simple -> P2p_peer.Id.t -> bool tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ], unit,
       unit, < filters: P2p_peer.Filter.t list >, unit,
       (P2p_peer.Id.t * (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t) list) RPC_service.t

    val info :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t) RPC_service.t

    val events :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, < monitor: bool>, unit,
       P2p_peer.Pool_event.t list) RPC_service.t

    val ban :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       unit) RPC_service.t

    val unban :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       unit) RPC_service.t

    val trust :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       unit) RPC_service.t

    val untrust :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       unit) RPC_service.t

    val banned :
      ([ `GET ], unit,
       unit * P2p_peer.Id.t, unit, unit,
       bool) RPC_service.t

  end

end

module ACL : sig

  val clear: #simple -> unit -> unit tzresult Lwt.t

  module S : sig

    val clear :
      ([ `GET ], unit,
       unit, unit, unit,
       unit) RPC_service.t

  end

end
