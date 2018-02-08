(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val errors:
  #Client_rpcs.ctxt -> Json_schema.schema tzresult Lwt.t

val forge_block_header:
  #Client_rpcs.ctxt ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

val inject_block:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?force:bool -> ?net_id:Net_id.t ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?net_id:Net_id.t ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module Protocols : sig

  val contents:
    #Client_rpcs.ctxt ->
    Protocol_hash.t -> Protocol.t tzresult Lwt.t

  val list:
    #Client_rpcs.ctxt ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Protocol.t option) list tzresult Lwt.t

end

val bootstrapped:
  #Client_rpcs.ctxt -> (Block_hash.t * Time.t) Lwt_stream.t tzresult Lwt.t

module Network : sig

  val stat:
    #Client_rpcs.ctxt -> P2p_stat.t tzresult Lwt.t

  val connections:
    #Client_rpcs.ctxt -> P2p_connection.Info.t list tzresult Lwt.t

  val peers:
    #Client_rpcs.ctxt -> (P2p_peer.Id.t * P2p_peer.Info.t) list tzresult Lwt.t

  val points:
    #Client_rpcs.ctxt -> (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

end

val complete:
  #Client_rpcs.ctxt ->
  ?block:Block_services.block -> string -> string list tzresult Lwt.t

val describe:
  #Client_rpcs.ctxt ->
  ?recurse:bool -> string list ->
  Data_encoding.json_schema RPC_description.directory tzresult Lwt.t
