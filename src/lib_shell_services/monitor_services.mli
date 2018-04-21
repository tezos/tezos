(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

val valid_blocks:
  #streamed ->
  ?chains:Chain_services.chain list ->
  ?protocols:Protocol_hash.t list ->
  ?next_protocols:Protocol_hash.t list ->
  unit -> ((Chain_id.t * Block_hash.t) Lwt_stream.t * stopper) tzresult Lwt.t

val heads:
  #streamed ->
  ?next_protocols:Protocol_hash.t list ->
  Chain_services.chain ->
  (Block_hash.t Lwt_stream.t * stopper) tzresult Lwt.t

val protocols:
  #streamed ->
  (Protocol_hash.t Lwt_stream.t * stopper) tzresult Lwt.t

module S : sig

  val valid_blocks:
    ([ `GET ], unit,
     unit, < chains : Chain_services.chain list;
             next_protocols : Protocol_hash.t list;
             protocols : Protocol_hash.t list >, unit,
     Chain_id.t * Block_hash.t) RPC_service.t

  val heads:
    ([ `GET ], unit,
     unit * Chain_services.chain,
     < next_protocols : Protocol_hash.t list >, unit,
     Block_hash.t) RPC_service.t

  val protocols:
    ([ `GET ], unit,
     unit, unit, unit,
     Protocol_hash.t) RPC_service.t

end

